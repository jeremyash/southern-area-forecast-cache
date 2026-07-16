library(tidyverse)
library(lubridate)
library(sf)
library(fs)

# ---------------------------------------------------------
# Settings
# ---------------------------------------------------------

git_ref <- "origin/cache-data"

cache_file_in_repo <- "cache/spot_superfog_cache.rds"

output_dir <- "spreadsheet-archive"

csv_path <- file.path(
  output_dir,
  "spot_forecast_archive_from_git_history.csv"
)

rds_path <- file.path(
  output_dir,
  "spot_forecast_archive_from_git_history.rds"
)

dir_create(output_dir)

# ---------------------------------------------------------
# Helpers
# ---------------------------------------------------------

run_git <- function(args) {
  result <- system2(
    command = "git",
    args = shQuote(args),
    stdout = TRUE,
    stderr = TRUE
  )
  
  status <- attr(result, "status")
  
  if (!is.null(status) && status != 0) {
    stop(
      "Git command failed:\n",
      paste(c("git", args), collapse = " "),
      "\n\n",
      paste(result, collapse = "\n")
    )
  }
  
  result
}
read_cache_from_commit <- function(commit_hash) {
  temp_rds <- tempfile(fileext = ".rds")
  
  object_spec <- paste0(
    commit_hash,
    ":",
    cache_file_in_repo
  )
  
  cmd <- paste(
    "git show",
    shQuote(object_spec),
    ">",
    shQuote(temp_rds)
  )
  
  status <- system(cmd)
  
  if (status != 0 || !file.exists(temp_rds)) {
    warning("Could not extract cache from commit: ", commit_hash)
    return(NULL)
  }
  
  out <- tryCatch(
    readRDS(temp_rds),
    error = function(e) {
      warning(
        "Could not read cache from commit ",
        commit_hash,
        ": ",
        e$message
      )
      NULL
    }
  )
  
  unlink(temp_rds)
  
  out
}

first_existing <- function(df, choices) {
  found <- choices[choices %in% names(df)]
  
  if (length(found) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  
  df[[found[1]]]
}

# ---------------------------------------------------------
# Find every commit that changed the cache file
# ---------------------------------------------------------
commit_hashes <- run_git(
  c(
    "log",
    "--format=%H",
    "origin/cache-data",
    "--",
    "cache/spot_superfog_cache.rds"
  )
)

if (length(commit_hashes) == 0) {
  stop("No historical cache commits were found.")
}

commit_times <- purrr::map_chr(
  commit_hashes,
  function(commit_hash) {
    run_git(
      c(
        "show",
        "-s",
        "--format=%aI",
        commit_hash
      )
    )[1]
  }
)

commit_index <- tibble::tibble(
  commit_hash = commit_hashes,
  commit_time = lubridate::ymd_hms(
    commit_times,
    quiet = TRUE
  )
)

message(
  "Historical cache commits found: ",
  nrow(commit_index)
)


# ---------------------------------------------------------
# Read every historical cache
# ---------------------------------------------------------

history_rows <- purrr::map2_dfr(
  commit_index$commit_hash,
  commit_index$commit_time,
  function(commit_hash, commit_time) {
    message("Reading commit: ", commit_hash)
    
    cache_obj <- read_cache_from_commit(commit_hash)
    
    if (
      is.null(cache_obj) ||
      is.null(cache_obj$forecast_df) ||
      nrow(cache_obj$forecast_df) == 0
    ) {
      return(tibble())
    }
    
    df <- cache_obj$forecast_df
    
    tibble(
      git_commit = commit_hash,
      git_commit_time = commit_time,
      
      cache_last_refresh = if (
        !is.null(cache_obj$last_refresh)
      ) {
        as.character(cache_obj$last_refresh)
      } else {
        NA_character_
      },
      
      spot_id = as.character(
        first_existing(df, c("spot_id"))
      ),
      
      project_name = as.character(
        first_existing(df, c("project_name"))
      ),
      
      project_type = as.character(
        first_existing(df, c("project_type"))
      ),
      
      latitude = suppressWarnings(
        as.numeric(first_existing(df, c("lat", "latitude")))
      ),
      
      longitude = suppressWarnings(
        as.numeric(first_existing(df, c("lon", "longitude")))
      ),
      
      timezone = as.character(
        first_existing(df, c("tz", "issuance_tz"))
      ),
      
      issuance_time_utc = as.character(
        first_existing(
          df,
          c("issuanceTime_utc", "issuanceTime")
        )
      ),
      
      issuance_display = as.character(
        first_existing(df, c("issuance_display"))
      ),
      
      issued = as.character(
        first_existing(df, c("issued"))
      ),
      
      nws_spot_url = as.character(
        first_existing(df, c("nws_spot_url"))
      ),
      
      issuing_office = as.character(
        first_existing(df, c("issuingOffice"))
      ),
      
      forecast_product_id = as.character(
        first_existing(
          df,
          c("X.id", "id", "product_id")
        )
      ),
      
      request_product_id = as.character(
        first_existing(df, c("req_api_id"))
      ),
      
      forecast_text = as.character(
        first_existing(df, c("productText"))
      )
    )
  }
)

if (nrow(history_rows) == 0) {
  stop("No forecast rows could be recovered from Git history.")
}

# ---------------------------------------------------------
# Create robust archive key and deduplicate
# ---------------------------------------------------------

history_rows <- history_rows |>
  mutate(
    issuance_time_utc_parsed = suppressWarnings(
      ymd_hms(
        issuance_time_utc,
        tz = "UTC",
        quiet = TRUE
      )
    ),
    
    spot_id_clean = str_remove(
      spot_id,
      "\\..*$"
    ),
    
    archive_key = case_when(
      !is.na(forecast_product_id) &
        nzchar(forecast_product_id) ~ forecast_product_id,
      
      !is.na(spot_id) &
        !is.na(issuance_time_utc) ~ paste(
          spot_id,
          issuance_time_utc,
          sep = "__"
        ),
      
      TRUE ~ paste(
        spot_id,
        project_name,
        latitude,
        longitude,
        str_sub(forecast_text, 1, 100),
        sep = "__"
      )
    )
  ) |>
  arrange(
    desc(git_commit_time)
  ) |>
  distinct(
    archive_key,
    .keep_all = TRUE
  )

# ---------------------------------------------------------
# Join forest names from coordinates
# ---------------------------------------------------------

forest_path <- "data/r8_forests_simplified.rds"

if (file.exists(forest_path)) {
  r8_forests <- readRDS(forest_path) |>
    st_transform(4326) |>
    select(forest)
  
  valid_coords <- history_rows |>
    mutate(row_id = row_number()) |>
    filter(
      !is.na(latitude),
      !is.na(longitude)
    )
  
  if (nrow(valid_coords) > 0) {
    valid_sf <- valid_coords |>
      st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
      ) |>
      st_join(
        r8_forests,
        join = st_intersects,
        left = TRUE
      ) |>
      st_drop_geometry() |>
      select(row_id, forest)
    
    history_rows <- history_rows |>
      mutate(row_id = row_number()) |>
      left_join(
        valid_sf,
        by = "row_id"
      ) |>
      select(-row_id)
  } else {
    history_rows$forest <- NA_character_
  }
} else {
  warning(
    "Forest file not found: ",
    forest_path,
    ". Forest names will be missing."
  )
  
  history_rows$forest <- NA_character_
}

# ---------------------------------------------------------
# Final archive table
# ---------------------------------------------------------

historical_archive <- history_rows |>
  transmute(
    archive_key,
    issuance_date = as.character(
      as.Date(issuance_time_utc_parsed)
    ),
    issuance_time_utc,
    issuance_display,
    timezone,
    spot_id,
    project_name,
    project_type,
    forest,
    latitude,
    longitude,
    issuing_office,
    nws_spot_url,
    forecast_product_id,
    request_product_id,
    first_seen_git_commit = git_commit,
    first_seen_git_time = as.character(git_commit_time),
    cache_last_refresh,
    forecast_text
  ) |>
  arrange(
    desc(issuance_time_utc),
    forest,
    project_name
  )

# ---------------------------------------------------------
# Save
# ---------------------------------------------------------

readr::write_csv(
  historical_archive,
  csv_path,
  na = ""
)

saveRDS(
  historical_archive,
  rds_path
)

message("Recovered unique forecasts: ", nrow(historical_archive))
message("With coordinates: ", sum(
  !is.na(historical_archive$latitude) &
    !is.na(historical_archive$longitude)
))
message("Saved CSV: ", csv_path)
message("Saved RDS: ", rds_path)