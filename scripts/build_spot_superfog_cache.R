# update_cache.R

# -------------------------------
# LIBRARIES
# -------------------------------

library(tidyverse)
library(lubridate)
library(lutz)
library(httr2)
library(jsonlite)
library(sf)

# -------------------------------
# LOG MESSAGING
# -------------------------------

Sys.setenv(TZ = "America/New_York")

log_msg <- function(...) {
  cat(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    " - ",
    paste(..., collapse = " "),
    "\n"
  )
}

log_msg("Starting cache update")

# -------------------------------
# GITHUB ACTIONS RUN MODE
# -------------------------------

force_run <- identical(Sys.getenv("FORCE_RUN"), "true")

if (force_run) {
  log_msg("FORCE_RUN detected.")
} else {
  log_msg("Scheduled run detected.")
}

# -------------------------------
# ENSURE CACHE DIRECTORY EXISTS
# -------------------------------

dir.create("cache", showWarnings = FALSE, recursive = TRUE)

log_msg(paste("Working directory:", getwd()))

# -------------------------------
# STATIC DATA
# -------------------------------

wfo <- read_csv("./data/r8_wfo.csv", show_col_types = FALSE)

r8_forests <- readRDS("./data/r8_forests_simplified.rds")

r8 <- st_read("./data/region_8", quiet = TRUE) |>
  st_transform(4326)

r8_forests$forest_id <- seq_len(nrow(r8_forests))

fws_nws_base_url <- "https://api.weather.gov/products/types/FWS"
stq_nws_base_url <- "https://api.weather.gov/products/types/STQ"

# -------------------------------
# CACHE PATHS
# -------------------------------

out_dir <- Sys.getenv("CACHE_OUT_DIR", unset = "cache")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cache_path <- file.path(out_dir, "spot_superfog_cache.rds")

# -------------------------------
# EMPTY CACHE HELPER
# -------------------------------

empty_cache <- function() {
  list(
    forecast_df = tibble(),
    sfog_tables = list(),
    last_refresh = Sys.time()
  )
}

# -------------------------------
# API HELPERS
# -------------------------------

spot_df_fun <- function(SPOT_URL) {
  tryCatch({
    spot_api_ls <- request(SPOT_URL) %>%
      req_perform() %>%
      resp_body_json()
    
    if (is.null(spot_api_ls$productText) || length(spot_api_ls$productText) == 0) {
      log_msg("FWS missing productText:", SPOT_URL)
      return(tibble())
    }
    
    out <- as.data.frame(spot_api_ls[2:9]) %>%
      mutate(
        spot_id = str_extract(productText, "(?<=\\.TAG\\s)[0-9.]+"),
        usda = str_detect(productText, regex("\\busda\\.gov\\b|\\busfs\\b", ignore_case = TRUE)),
        issuanceTime = ymd_hms(issuanceTime, tz = "UTC"),
        date_issued = as.Date(with_tz(issuanceTime, "America/New_York"))
      ) %>%
      filter(usda == TRUE)
    
    if (nrow(out) == 0) {
      log_msg("FWS parsed but no USDA records:", SPOT_URL)
    }
    
    out
  }, error = function(e) {
    log_msg("FWS parse failed:", SPOT_URL, "::", e$message)
    tibble()
  })
}

stq_df_fun <- function(STQ_URL) {
  tryCatch({
    stq_api_ls <- request(STQ_URL) %>%
      req_perform() %>%
      resp_body_json()
    
    if (is.null(stq_api_ls$productText) || length(stq_api_ls$productText) == 0) {
      log_msg("STQ missing productText:", STQ_URL)
      return(tibble())
    }
    
    out <- as.data.frame(stq_api_ls[2:9]) %>%
      mutate(
        spot_id = str_match(productText, "OFILE:\\s*([0-9.]+)")[, 2],
        project_name = str_match(productText, "PROJECT NAME:\\s*([^\\n]+)")[, 2],
        project_type = str_match(productText, "PROJECT TYPE:\\s*([^\\n]+)")[,2],
        lat = as.numeric(str_match(productText, "DLAT:\\s*(-?[0-9.]+)")[, 2]),
        lon = -abs(as.numeric(str_match(productText, "DLON:\\s*(-?[0-9.]+)")[, 2])),
        date = str_match(productText, "DATE:\\s*(\\d{2}/\\d{2}/\\d{2})")[, 2],
        time = str_match(productText, "TIME:\\s*(\\d{4})")[, 2]
      ) %>%
      filter(!is.na(spot_id))
    
    if (nrow(out) == 0) {
      log_msg("STQ parsed but no valid OFILE spot_id:", STQ_URL)
    }
    
    out
  }, error = function(e) {
    log_msg("STQ parse failed:", STQ_URL, "::", e$message)
    tibble()
  })
}

# -------------------------------
# TIME HELPER
# -------------------------------

convert_to_24hr <- function(time_str) {
  if (length(time_str) != 1 || is.na(time_str)) return(NA_character_)
  
  x <- toupper(trimws(as.character(time_str)))
  if (!nzchar(x)) return(NA_character_)
  
  x <- gsub("^MID(NGT|NIGHT)?$", "12AM", x)
  x <- gsub("^NOON$", "12PM", x)
  x <- gsub("^([0-9]{1,2})\\s*A(M)?$", "\\1AM", x)
  x <- gsub("^([0-9]{1,2})\\s*P(M)?$", "\\1PM", x)
  x <- gsub("^([0-9]{1,2})A$", "\\1AM", x)
  x <- gsub("^([0-9]{1,2})P$", "\\1PM", x)
  
  if (!grepl("^[0-9]{1,2}(AM|PM)$", x)) return(NA_character_)
  
  hour <- as.integer(sub("^([0-9]{1,2})(AM|PM)$", "\\1", x))
  period <- sub("^([0-9]{1,2})(AM|PM)$", "\\2", x)
  
  if (is.na(hour) || hour < 1 || hour > 12) return(NA_character_)
  
  if (period == "AM") {
    if (hour == 12) "00" else sprintf("%02d", hour)
  } else {
    if (hour == 12) sprintf("%02d", hour) else sprintf("%02d", hour + 12)
  }
}
# -------------------------------
# SUPERFOG PARSING
# -------------------------------

parse_spot_forecasts <- function(df) {
  if (nrow(df) == 0) return(list())
  
  map(seq_len(nrow(df)), function(i) {
    tryCatch({
      spot_txt  <- df$productText[i]
      spot_info <- df[i, ]
      
      spot_chunks <- strsplit(spot_txt, "\n\n")[[1]]
      time_chunks <- spot_chunks[grepl("(^TIME\\s|\\nTIME\\s)", spot_chunks)]
      
      if (length(time_chunks) == 0) return(NULL)
      
      parse_time_chunk <- function(chunk) {
        lines <- strsplit(chunk, "\n")[[1]]
        lines <- lines[nzchar(trimws(lines))]
        lines_trim <- trimws(lines)
        
        time_line <- lines_trim[grepl("^TIME", lines_trim)]
        if (length(time_line) == 0) return(NULL)
        time_line <- time_line[1]
        
        time_line_norm <- toupper(time_line)
        time_line_norm <- gsub("\\(.*?\\)", " ", time_line_norm)
        time_line_norm <- gsub("MIDNGT", "12AM", time_line_norm, ignore.case = TRUE)
        time_line_norm <- gsub("\\bMID\\b", "12AM", time_line_norm, ignore.case = TRUE)
        time_line_norm <- gsub("\\bNOON\\b", "12PM", time_line_norm, ignore.case = TRUE)
        time_line_norm <- gsub("([0-9]{1,2})\\s+(AM|PM)\\b", "\\1\\2", time_line_norm, ignore.case = TRUE)
        time_line_norm <- gsub("\\b([0-9]{1,2})A\\b", "\\1AM", time_line_norm, ignore.case = TRUE)
        time_line_norm <- gsub("\\b([0-9]{1,2})P\\b", "\\1PM", time_line_norm, ignore.case = TRUE)
        
        time_vals <- stringr::str_extract_all(
          time_line_norm,
          "\\b\\d{1,2}(AM|PM)\\b"
        )[[1]]
        
        if (length(time_vals) == 0) return(NULL)
        
        out <- tibble(TIME = time_vals)
        
        strip_label <- function(x, type) {
          switch(
            type,
            SKY = {
              x <- sub("^Sky\\s*\\(\\s*%\\s*\\)\\.*\\s*", "", x, ignore.case = TRUE)
              x <- sub("^Sky cover\\.*\\s*", "", x, ignore.case = TRUE)
              x
            },
            TEMP = sub("^Temp\\.*\\s*", "", x, ignore.case = TRUE),
            RH   = sub("^RH\\.*\\s*", "", x, ignore.case = TRUE),
            WIND = {
              x <- sub("^Surface\\s*(wnd|wind)\\s*spd\\.*\\s*", "", x, ignore.case = TRUE)
              x <- sub("^20\\s*ft\\s*wind\\s*spd\\.*\\s*", "", x, ignore.case = TRUE)
              x <- sub("^20\\s*ft\\s*wind\\s*mph\\.*\\s*", "", x, ignore.case = TRUE)
              x <- sub("^20\\s*ft\\s*wind\\.*\\s*", "", x, ignore.case = TRUE)
              x
            },
            x
          )
        }
        
        split_cells <- function(x, n_expected = NULL, allow_single_space_fallback = FALSE) {
          x <- gsub("\\s+$", "", x)
          x <- trimws(x)
          
          if (!nzchar(x)) return(character(0))
          
          vals <- unlist(strsplit(x, "\\s{2,}"))
          vals <- trimws(vals)
          vals <- vals[nzchar(vals)]
          
          if (allow_single_space_fallback &&
              !is.null(n_expected) &&
              length(vals) != n_expected) {
            vals2 <- unlist(strsplit(x, "\\s+"))
            vals2 <- trimws(vals2)
            vals2 <- vals2[nzchar(vals2)]
            
            if (length(vals2) == n_expected) {
              vals <- vals2
            }
          }
          
          vals
        }
        
        split_sky_cells <- function(x, n_expected) {
          x <- trimws(x)
          if (!nzchar(x)) return(rep(NA_character_, n_expected))
          
          if (grepl("[A-Za-z]", x)) {
            vals <- unlist(strsplit(x, "\\s+"))
            vals <- trimws(vals)
            vals <- vals[nzchar(vals)]
          } else {
            vals <- unlist(strsplit(x, "\\s{2,}"))
            vals <- trimws(vals)
            vals <- vals[nzchar(vals)]
            
            if (length(vals) != n_expected) {
              vals2 <- unlist(strsplit(x, "\\s+"))
              vals2 <- trimws(vals2)
              vals2 <- vals2[nzchar(vals2)]
              if (length(vals2) == n_expected) vals <- vals2
            }
          }
          
          if (length(vals) < n_expected) vals <- c(vals, rep(NA_character_, n_expected - length(vals)))
          if (length(vals) > n_expected) vals <- vals[seq_len(n_expected)]
          vals
        }
        
        pad_vals <- function(vals, n) {
          vals <- as.character(vals)
          if (length(vals) < n) vals <- c(vals, rep(NA_character_, n - length(vals)))
          if (length(vals) > n) vals <- vals[seq_len(n)]
          vals
        }
        
        sky_line <- lines_trim[
          grepl("^Sky\\s*\\(\\s*%\\s*\\)", lines_trim, ignore.case = TRUE) |
            grepl("^Sky cover", lines_trim, ignore.case = TRUE)
        ]
        if (length(sky_line) > 0) {
          sky_raw <- strip_label(sky_line[1], "SKY")
          out$SKY <- split_sky_cells(sky_raw, length(time_vals))
        }
        
        temp_line <- lines_trim[grepl("^Temp", lines_trim, ignore.case = TRUE)]
        if (length(temp_line) > 0) {
          temp_raw <- strip_label(temp_line[1], "TEMP")
          temp_vals <- split_cells(
            temp_raw,
            n_expected = length(time_vals),
            allow_single_space_fallback = FALSE
          )
          out$TEMP <- pad_vals(temp_vals, length(time_vals))
        }
        
        rh_line <- lines_trim[grepl("^RH", lines_trim, ignore.case = TRUE)]
        if (length(rh_line) > 0) {
          rh_raw <- strip_label(rh_line[1], "RH")
          rh_vals <- unlist(strsplit(trimws(rh_raw), "\\s+"))
          rh_vals <- trimws(rh_vals)
          rh_vals <- rh_vals[nzchar(rh_vals)]
          out$RH <- pad_vals(rh_vals, length(time_vals))
        }
        
        wind_line <- lines_trim[
          grepl("^Surface\\s*(wnd|wind)\\s*spd", lines_trim, ignore.case = TRUE) |
            grepl("^20\\s*ft\\s*wind\\s*spd", lines_trim, ignore.case = TRUE) |
            grepl("^20\\s*ft\\s*wind\\s*mph", lines_trim, ignore.case = TRUE) |
            (grepl("^20\\s*ft\\s*wind", lines_trim, ignore.case = TRUE) &
               !grepl("^20\\s*ft\\s*wind\\s*(dir|gust)", lines_trim, ignore.case = TRUE))
        ]
        
        if (length(wind_line) > 0) {
          wind_raw <- strip_label(wind_line[1], "WIND")
          wind_cells <- split_cells(
            wind_raw,
            n_expected = length(time_vals),
            allow_single_space_fallback = FALSE
          )
          wind_cells <- pad_vals(wind_cells, length(time_vals))
          wind_vals <- stringr::str_extract(wind_cells, "\\d+")
          out$WIND <- wind_vals
        }
        
        out
      }
      
      chunk_dfs <- purrr::map(time_chunks, parse_time_chunk)
      chunk_dfs <- purrr::compact(chunk_dfs)
      if (length(chunk_dfs) == 0) return(NULL)
      
      t_all_times_df <- dplyr::bind_rows(chunk_dfs)
      if (nrow(t_all_times_df) == 0) return(NULL)
      
      t_all_times_df <- t_all_times_df %>%
        mutate(
          TIME = as.character(TIME),
          TIME_24 = vapply(TIME, convert_to_24hr, character(1)),
          hour_num = suppressWarnings(as.integer(TIME_24))
        ) %>%
        filter(!is.na(TIME_24))
      
      if (nrow(t_all_times_df) == 0) return(NULL)
      
      base_date <- as.Date(spot_info$date, format = "%m/%d/%y")
      if (is.na(base_date) && !is.null(spot_info$date_issued) && !is.na(spot_info$date_issued)) {
        base_date <- as.Date(spot_info$date_issued)
      }
      if (is.na(base_date)) {
        log_msg("parse_spot_forecasts row", i, ": base_date is NA")
        return(NULL)
      }
      
      forecast_dates <- rep(base_date, nrow(t_all_times_df))
      if (nrow(t_all_times_df) > 1) {
        for (j in 2:nrow(t_all_times_df)) {
          forecast_dates[j] <- forecast_dates[j - 1]
          if (t_all_times_df$hour_num[j] < t_all_times_df$hour_num[j - 1]) {
            forecast_dates[j] <- forecast_dates[j] + 1
          }
        }
      }
      
      filter_start <- as.POSIXct(
        paste(base_date, "18:00:00"),
        tz = spot_info$tz
      )
      
      filter_end <- as.POSIXct(
        paste(base_date + 1, "08:00:00"),
        tz = spot_info$tz
      )
      
      time_all_times_df <- t_all_times_df %>%
        mutate(
          TEMP = suppressWarnings(as.numeric(TEMP)),
          RH = suppressWarnings(as.numeric(RH)),
          WIND = suppressWarnings(as.numeric(WIND)),
          SKY_RAW = trimws(as.character(SKY)),
          SKY_NUM = suppressWarnings(as.numeric(SKY_RAW)),
          forecast_date = forecast_dates,
          DATETIME = as.POSIXct(
            paste(forecast_date, paste0(TIME_24, ":00:00")),
            tz = spot_info$tz
          )
        ) %>%
        filter(DATETIME >= filter_start, DATETIME <= filter_end) %>%
        arrange(DATETIME) %>%
        select(DATETIME, TEMP, RH, WIND, SKY = SKY_RAW, SKY_NUM)
      
      rownames(time_all_times_df) <- NULL
      
      sfog_df <- time_all_times_df %>%
        mutate(
          sky_screen = case_when(
            !is.na(SKY_NUM) & SKY_NUM <= 40 ~ "critical",
            !is.na(SKY_NUM) & SKY_NUM > 60 ~ "minimal",
            !is.na(SKY_NUM) ~ "watch_out",
            tolower(trimws(SKY)) %in% c("mcr", "clr", "clear", "mclear") ~ "critical",
            tolower(trimws(SKY)) %in% c("pc", "pcldy", "pcloudy") ~ "watch_out",
            TRUE ~ "minimal"
          ),
          temp_screen = ifelse(TEMP <= 55, "critical", ifelse(TEMP > 70, "minimal", "watch_out")),
          rh_screen = ifelse(RH >= 90, "critical", ifelse(RH < 70, "minimal", "watch_out")),
          wind_screen = ifelse(WIND <= 4, "critical", ifelse(WIND > 7, "minimal", "watch_out"))
        ) %>%
        select(DATETIME, TEMP, RH, WIND, SKY, sky_screen, temp_screen, rh_screen, wind_screen)
      
      sfog_df
    }, error = function(e) {
      log_msg("parse_spot_forecasts failed for row", i, ":", e$message)
      NULL
    })
  })
}

# -------------------------------
# BUILD CACHE
# -------------------------------

build_cache <- function() {
  today_val <- today(tzone = "America/New_York")
  
  fws_ls <- request(fws_nws_base_url) %>%
    req_perform() %>%
    resp_body_json()
  
  stq_ls <- request(stq_nws_base_url) %>%
    req_perform() %>%
    resp_body_json()
  
  fws_df <- bind_rows(fws_ls$`@graph`) %>%
    mutate(
      issuingOffice = str_sub(issuingOffice, 2),
      date_issued = as.Date(with_tz(ymd_hms(issuanceTime, tz = "UTC"), "America/New_York"))
    ) %>%
    filter(issuingOffice %in% wfo$WFO) %>%
    filter(productName == "Suppression Forecast") %>%
    rename(api_url = "@id") %>%
    filter(date_issued %in% c(today_val, today_val - 1))
  
  stq_df <- bind_rows(stq_ls$`@graph`) %>%
    mutate(
      issuingOffice = str_sub(issuingOffice, 2),
      date_issued = as.Date(with_tz(ymd_hms(issuanceTime, tz = "UTC"), "America/New_York"))
    ) %>%
    filter(issuingOffice %in% wfo$WFO) %>%
    filter(productName == "Spot Forecast Request") %>%
    rename(api_url = "@id") %>%
    filter(date_issued %in% c(today_val, today_val - 1, today_val - 2))
  
  log_msg("FWS products found:", nrow(fws_df))
  log_msg("STQ products found:", nrow(stq_df))
  
  if (nrow(fws_df) == 0 || nrow(stq_df) == 0) {
    log_msg("No FWS or STQ products found. Writing empty cache.")
    return(empty_cache())
  }
  
  log_msg("Starting FWS parse on", nrow(fws_df), "records")
  
  spot_api_df <- purrr::imap_dfr(fws_df$api_url, function(u, i) {
    log_msg("Parsing FWS", i, "of", nrow(fws_df))
    spot_df_fun(u)
  }) %>%
    group_by(spot_id) %>%
    slice_max(order_by = issuanceTime, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  log_msg("FWS spot forecasts retained:", nrow(spot_api_df))
  
  if (nrow(spot_api_df) == 0) {
    log_msg("No spot forecasts returned from FWS products. Writing empty cache.")
    return(empty_cache())
  }
  
  log_msg("Starting STQ parse on", nrow(stq_df), "records")
  
  request_api_df <- purrr::imap_dfr(stq_df$api_url, function(u, i) {
    log_msg("Parsing STQ", i, "of", nrow(stq_df))
    stq_df_fun(u)
  }) %>%
    filter(spot_id %in% spot_api_df$spot_id) %>%
    select(req_api_id = X.id, spot_id:time) %>%
    distinct()
  
  log_msg("Matching STQ requests retained:", nrow(request_api_df))
  
  if (nrow(request_api_df) == 0) {
    log_msg("No matching STQ requests found. Writing empty cache.")
    return(empty_cache())
  }
  
  request_api_df <- request_api_df %>%
    mutate(
      tz = purrr::pmap_chr(
        list(lat, lon),
        function(lat, lon) {
          tryCatch(
            tz_lookup_coords(lat, lon, method = "accurate"),
            error = function(e) {
              log_msg("tz_lookup_coords failed for lat/lon:", lat, lon, "::", e$message)
              NA_character_
            }
          )
        }
      )
    ) %>%
    filter(!is.na(tz)) %>%
    rowwise() %>%
    mutate(
      forecast_date_time_obj = paste(date, time),
      forecast_date_time = as.POSIXct(
        forecast_date_time_obj,
        format = "%m/%d/%y %H%M",
        tz = tz
      )
    ) %>%
    ungroup()
  
  forecast_df <- left_join(request_api_df, spot_api_df, by = "spot_id") %>%
    mutate(
      spot_id_clean = gsub("\\..*$", "", spot_id),
      nws_spot_url = paste0("https://spot.weather.gov/forecasts/", spot_id_clean),
      issued = ifelse(date_issued == today_val, "Today", "Yesterday")
    ) %>% 
    filter(str_to_upper(str_trim(project_type)) == "PRESCRIBED") %>% 
    group_by(nws_spot_url) %>% 
    arrange(desc(date_issued)) %>% 
    slice(1) %>% 
    ungroup()
  
  log_msg("Joined forecast rows:", nrow(forecast_df))
  
  if (nrow(forecast_df) == 0) {
    log_msg("Joined forecast data is empty. Writing empty cache.")
    return(empty_cache())
  }
  
  # -------------------------------------------------
  # FILTER TO REGION 8
  # -------------------------------------------------
  
  r8_bbox <- st_bbox(r8)
  
  forecast_df_bbox <- forecast_df %>%
    filter(
      lon >= r8_bbox["xmin"],
      lon <= r8_bbox["xmax"],
      lat >= r8_bbox["ymin"],
      lat <= r8_bbox["ymax"]
    )
  
  log_msg("After bbox filter:", nrow(forecast_df_bbox))
  
  if (nrow(forecast_df_bbox) == 0) {
    log_msg("No forecasts inside Region 8 bounding box. Writing empty cache.")
    return(empty_cache())
  }
  
  spots_sf <- st_as_sf(
    forecast_df_bbox,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
  
  spots_sf <- spots_sf[lengths(st_intersects(spots_sf, r8)) > 0, ]
  forecast_df <- st_drop_geometry(spots_sf) 
  
  forecast_df <- forecast_df %>%
    rowwise() %>%
    mutate(
      issuanceTime_utc = issuanceTime,
      issuance_tz = tz,
      issuance_display = format(
        lubridate::with_tz(
          issuanceTime_utc,
          issuance_tz
        ),
        "%b %d, %I:%M %p %Z"
      )
    ) %>%
    ungroup()
  
  log_msg("Inside Region 8 polygon:", nrow(forecast_df))
  
  if (nrow(forecast_df) == 0) {
    log_msg("No forecasts inside Region 8 polygon. Writing empty cache.")
    return(empty_cache())
  }
  
  # -------------------------------------------------
  # CREATE PARSED SPOT TABLES
  # -------------------------------------------------
  
  sfog_tables <- parse_spot_forecasts(forecast_df)
  
  parsed_ok <- purrr::map_lgl(sfog_tables, ~ !is.null(.x) && nrow(.x) > 0)
  log_msg("Parsed sfog tables retained:", sum(parsed_ok), "of", length(sfog_tables))
  
  forecast_df <- forecast_df[parsed_ok, , drop = FALSE]
  sfog_tables <- sfog_tables[parsed_ok]
  
  if (nrow(forecast_df) == 0 || length(sfog_tables) == 0) {
    log_msg("No parsed spot forecast tables retained. Writing empty cache.")
    return(empty_cache())
  }
  
  # -------------------------------------------------
  # COMBINE OUTPUTS
  # -------------------------------------------------
  
  list(
    forecast_df = forecast_df,
    sfog_tables = sfog_tables,
    last_refresh = Sys.time()
  )
}

# -------------------------------------------------
# GENERATE AND SAVE CACHE
# -------------------------------------------------

cache_obj <- tryCatch(
  build_cache(),
  error = function(e) {
    log_msg("Cache build failed:", e$message)
    
    if (file.exists(cache_path)) {
      log_msg("Using existing cache file as fallback.")
      fallback_cache <- readRDS(cache_path)
      fallback_cache$last_refresh <- Sys.time()
      fallback_cache
    } else {
      log_msg("No existing cache file found. Writing empty fallback cache.")
      empty_cache()
    }
  }
)

log_msg("About to save cache with last_refresh =", as.character(cache_obj$last_refresh))

saveRDS(cache_obj, cache_path)

check_obj <- readRDS(cache_path)
log_msg("Read-back cache last_refresh =", as.character(check_obj$last_refresh))

log_msg("Cache updated successfully at", as.character(cache_obj$last_refresh))