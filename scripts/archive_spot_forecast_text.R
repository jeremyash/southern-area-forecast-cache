library(tidyverse)
library(sf)
library(fs)
library(lubridate)
library(openxlsx)

# ------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------

cache_path <- Sys.getenv(
  "SPOT_CACHE_PATH",
  unset = "cache/spot_superfog_cache.rds"
)

text_archive_dir <- Sys.getenv(
  "TEXT_ARCHIVE_OUT_DIR",
  unset = "text-archive"
)

spreadsheet_archive_dir <- Sys.getenv(
  "SPREADSHEET_ARCHIVE_OUT_DIR",
  unset = "spreadsheet-archive"
)

csv_path <- file.path(
  spreadsheet_archive_dir,
  "spot_forecast_archive.csv"
)

xlsx_path <- file.path(
  spreadsheet_archive_dir,
  "spot_forecast_archive.xlsx"
)

dir.create(
  text_archive_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  spreadsheet_archive_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------

safe_name <- function(x, allow_decimal = FALSE) {
  x <- ifelse(
    is.na(x) | !nzchar(trimws(as.character(x))),
    "Unknown",
    as.character(x)
  )
  
  pattern <- if (allow_decimal) {
    "[^A-Za-z0-9.]+"
  } else {
    "[^A-Za-z0-9]+"
  }
  
  x |>
    stringr::str_replace_all(pattern, "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove("^_") |>
    stringr::str_remove("_$")
}

first_existing_column <- function(df, choices, default = NA_character_) {
  found <- choices[choices %in% names(df)]
  
  if (length(found) == 0) {
    return(rep(default, nrow(df)))
  }
  
  df[[found[1]]]
}

format_local_time <- function(utc_time, timezone) {
  purrr::map2_chr(
    utc_time,
    timezone,
    function(this_time, this_tz) {
      if (
        is.na(this_time) ||
        is.na(this_tz) ||
        !nzchar(this_tz)
      ) {
        return(NA_character_)
      }
      
      format(
        lubridate::with_tz(this_time, this_tz),
        "%Y-%m-%d %H:%M:%S %Z"
      )
    }
  )
}

local_issue_date <- function(utc_time, timezone) {
  purrr::map2_chr(
    utc_time,
    timezone,
    function(this_time, this_tz) {
      if (
        is.na(this_time) ||
        is.na(this_tz) ||
        !nzchar(this_tz)
      ) {
        return(NA_character_)
      }
      
      as.character(
        as.Date(
          lubridate::with_tz(
            this_time,
            this_tz
          )
        )
      )
    }
  ) |>
    as.Date()
}

write_archive_xlsx <- function(df, path) {
  xlsx_df <- df
  
  # Excel cells are limited to 32,767 characters.
  xlsx_df <- xlsx_df |>
    mutate(
      forecast_text = if_else(
        is.na(forecast_text),
        NA_character_,
        stringr::str_sub(forecast_text, 1, 32767)
      )
    )
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(
    wb,
    "Spot Forecast Archive",
    gridLines = FALSE
  )
  
  openxlsx::writeData(
    wb,
    sheet = "Spot Forecast Archive",
    x = xlsx_df,
    withFilter = TRUE,
    keepNA = FALSE
  )
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#243447",
    fontColour = "#FFFFFF",
    halign = "center",
    valign = "center",
    border = "Bottom",
    borderColour = "#243447"
  )
  
  wrap_style <- openxlsx::createStyle(
    wrapText = TRUE,
    valign = "top"
  )
  
  openxlsx::addStyle(
    wb,
    sheet = "Spot Forecast Archive",
    style = header_style,
    rows = 1,
    cols = seq_len(ncol(xlsx_df)),
    gridExpand = TRUE
  )
  
  openxlsx::freezePane(
    wb,
    sheet = "Spot Forecast Archive",
    firstRow = TRUE
  )
  
  openxlsx::setColWidths(
    wb,
    sheet = "Spot Forecast Archive",
    cols = seq_len(ncol(xlsx_df)),
    widths = "auto"
  )
  
  forecast_col <- which(names(xlsx_df) == "forecast_text")
  
  if (length(forecast_col) == 1) {
    openxlsx::setColWidths(
      wb,
      sheet = "Spot Forecast Archive",
      cols = forecast_col,
      widths = 80
    )
    
    if (nrow(xlsx_df) > 0) {
      openxlsx::addStyle(
        wb,
        sheet = "Spot Forecast Archive",
        style = wrap_style,
        rows = 2:(nrow(xlsx_df) + 1),
        cols = forecast_col,
        gridExpand = TRUE
      )
    }
  }
  
  openxlsx::saveWorkbook(
    wb,
    path,
    overwrite = TRUE
  )
}

# ------------------------------------------------------------------
# Read cache
# ------------------------------------------------------------------

x <- readRDS(cache_path)
forecast_df <- x$forecast_df

if (nrow(forecast_df) == 0) {
  message("No forecast rows found. Nothing to archive.")
  quit(save = "no", status = 0)
}

# ------------------------------------------------------------------
# Join forecasts to forests
# ------------------------------------------------------------------

r8_forests <- readRDS("data/r8_forests_simplified.rds") |>
  sf::st_transform(4326)

forecast_sf <- forecast_df |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

forecast_with_forest <- forecast_sf |>
  sf::st_join(
    r8_forests |>
      dplyr::select(forest_name = forest),
    join = sf::st_intersects,
    left = TRUE
  ) |>
  sf::st_drop_geometry() |>
  mutate(
    forest_name = if_else(
      is.na(forest_name),
      "Unknown Forest",
      forest_name
    )
  )

# ------------------------------------------------------------------
# Prepare archive metadata
# ------------------------------------------------------------------

forecast_with_forest <- forecast_with_forest |>
  mutate(
    issuance_time_utc = as.POSIXct(
      issuanceTime,
      tz = "UTC"
    ),
    
    timezone = if_else(
      is.na(tz) | !nzchar(tz),
      "America/New_York",
      tz
    ),
    
    issuance_time_local = format_local_time(
      issuance_time_utc,
      timezone
    ),
    
    issuance_date_local = local_issue_date(
      issuance_time_utc,
      timezone
    ),
    
    file_date = format(
      issuance_date_local,
      "%Y%m%d"
    ),
    
    spot_safe = safe_name(
      spot_id,
      allow_decimal = TRUE
    ),
    
    project_safe = safe_name(project_name),
    forest_safe = safe_name(forest_name),
    
    forest_dir = file.path(
      text_archive_dir,
      forest_safe
    ),
    
    file_name = paste0(
      file_date,
      "__spot_",
      spot_safe,
      "__project_",
      project_safe,
      ".txt"
    ),
    
    file_path = file.path(
      forest_dir,
      file_name
    ),
    
    archive_file = file.path(
      forest_safe,
      file_name
    ),
    
    forecast_url = first_existing_column(
      forecast_with_forest,
      c("nws_spot_url")
    ),
    
    forecast_product_id = first_existing_column(
      forecast_with_forest,
      c("X.id", "id", "product_id")
    ),
    
    request_product_id = first_existing_column(
      forecast_with_forest,
      c("req_api_id")
    ),
    
    archive_key = if_else(
      !is.na(forecast_product_id) &
        nzchar(as.character(forecast_product_id)),
      as.character(forecast_product_id),
      paste(
        spot_id,
        format(
          issuance_time_utc,
          "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        sep = "__"
      )
    )
  ) |>
  distinct(archive_key, .keep_all = TRUE)

# ------------------------------------------------------------------
# Write individual text files
# ------------------------------------------------------------------

purrr::walk(
  seq_len(nrow(forecast_with_forest)),
  function(i) {
    this_dir <- forecast_with_forest$forest_dir[i]
    this_file <- forecast_with_forest$file_path[i]
    this_text <- forecast_with_forest$productText[i]
    
    fs::dir_create(this_dir)
    
    if (!file.exists(this_file)) {
      writeLines(
        text = this_text,
        con = this_file,
        useBytes = TRUE
      )
      
      message("Wrote: ", this_file)
    } else {
      message("Exists, skipped: ", this_file)
    }
  }
)

# ------------------------------------------------------------------
# Build rows for master archive
# ------------------------------------------------------------------

new_archive_rows <- forecast_with_forest |>
  transmute(
    archive_key,
    issuance_date = as.character(issuance_date_local),
    issuance_time_utc = format(
      issuance_time_utc,
      "%Y-%m-%d %H:%M:%S UTC",
      tz = "UTC"
    ),
    issuance_time_local,
    timezone,
    spot_id = as.character(spot_id),
    project_name = as.character(project_name),
    project_type = as.character(project_type),
    forest = as.character(forest_name),
    latitude = as.numeric(lat),
    longitude = as.numeric(lon),
    issuing_office = as.character(
      first_existing_column(
        forecast_with_forest,
        c("issuingOffice")
      )
    ),
    forecast_url = as.character(forecast_url),
    forecast_product_id = as.character(forecast_product_id),
    request_product_id = as.character(request_product_id),
    archive_file,
    forecast_text = as.character(productText)
  )

# ------------------------------------------------------------------
# Merge with existing master archive
# ------------------------------------------------------------------
if (file.exists(csv_path)) {
  existing_archive <- readr::read_csv(
    csv_path,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
} else {
  existing_archive <- tibble()
}

master_archive <- bind_rows(
  existing_archive,
  new_archive_rows
) |>
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) |>
  arrange(
    desc(issuance_time_utc),
    forest,
    project_name
  ) |>
  distinct(
    archive_key,
    .keep_all = TRUE
  )

# ------------------------------------------------------------------
# Save master archive
# ------------------------------------------------------------------

readr::write_csv(
  master_archive,
  csv_path,
  na = ""
)

write_archive_xlsx(
  master_archive,
  xlsx_path
)

message("Master CSV rows: ", nrow(master_archive))
message("Saved CSV: ", csv_path)
message("Saved XLSX: ", xlsx_path)