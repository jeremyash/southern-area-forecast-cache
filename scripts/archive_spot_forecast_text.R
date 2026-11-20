# Spot Forecast Text and CSV Archive
# Source: spot_superfog_cache.rds (FWS/STQ-derived forecast cache)
# Purpose: Preserve individual forecast versions and complete monthly CSV exports.
# Existing master CSV and historical text files are retained without renaming.

# 1. Packages and paths ----
library(tidyverse)
library(sf)
library(fs)
library(lubridate)

cache_path <- Sys.getenv("SPOT_CACHE_PATH", unset = "cache/spot_superfog_cache.rds")
text_archive_dir <- Sys.getenv("TEXT_ARCHIVE_OUT_DIR", unset = "text-archive")
spreadsheet_archive_dir <- Sys.getenv("SPREADSHEET_ARCHIVE_OUT_DIR", unset = "spreadsheet-archive")
master_path <- file.path(spreadsheet_archive_dir, "spot_forecast_archive.csv")
dir.create(text_archive_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(spreadsheet_archive_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Helpers ----
safe_name <- function(x, allow_decimal = FALSE) {
  x <- ifelse(is.na(x) | !nzchar(trimws(as.character(x))), "Unknown", as.character(x))
  pattern <- if (allow_decimal) "[^A-Za-z0-9.]+" else "[^A-Za-z0-9]+"
  x |> stringr::str_replace_all(pattern, "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove("^_") |> stringr::str_remove("_$")
}

first_existing_column <- function(df, choices, default = NA_character_) {
  found <- choices[choices %in% names(df)]
  if (!length(found)) return(rep(default, nrow(df)))
  df[[found[1]]]
}

format_local_time <- function(utc_time, timezone) {
  purrr::map2_chr(utc_time, timezone, function(t, z) {
    if (is.na(t) || is.na(z) || !nzchar(z)) return(NA_character_)
    format(lubridate::with_tz(t, z), "%Y-%m-%d %H:%M:%S %Z")
  })
}

local_issue_date <- function(utc_time, timezone) {
  as.Date(purrr::map2_chr(utc_time, timezone, function(t, z) {
    if (is.na(t) || is.na(z) || !nzchar(z)) return(NA_character_)
    as.character(as.Date(lubridate::with_tz(t, z)))
  }))
}

# Recover missing historical issuance dates.
recover_issue_date <- function(date, utc_time, timezone) {
  missing_date <- is.na(date) | !nzchar(trimws(date))
  
  if (!any(missing_date)) {
    return(date)
  }
  
  parsed_utc <- lubridate::ymd_hms(
    utc_time[missing_date],
    tz = "UTC",
    quiet = TRUE
  )
  
  recovered <- purrr::map2_chr(
    parsed_utc,
    timezone[missing_date],
    function(t, z) {
      if (is.na(t) || is.na(z) || !nzchar(z)) {
        return(NA_character_)
      }
      
      as.character(as.Date(lubridate::with_tz(t, z)))
    }
  )
  
  date[missing_date] <- recovered
  date
}

# Stable hash, without an additional R package dependency.
text_hash <- function(x) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(enc2utf8(x)), tmp)
  unname(tools::md5sum(tmp))
}

# Normalize line endings for forecast text comparisons.
normalize_forecast_text <- function(x) {
  x |>
    stringr::str_replace_all("\r\n?", "\n") |>
    stringr::str_replace("\\n+$", "")
}

write_csv_if_changed <- function(df, path) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  readr::write_csv(df, tmp, na = "")
  if (file.exists(path) && identical(unname(tools::md5sum(path)), unname(tools::md5sum(tmp)))) {
    message("Unchanged: ", path)
    return(invisible(FALSE))
  }
  if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not write: ", path)
  message("Updated: ", path)
  invisible(TRUE)
}

# 3. Read cache and historical master archive ----
x <- readRDS(cache_path)
forecast_df <- x$forecast_df
if (is.null(forecast_df)) stop("Cache is missing forecast_df")

existing_archive <- if (file.exists(master_path)) {
  readr::read_csv(master_path, col_types = readr::cols(.default = readr::col_character()),
                  show_col_types = FALSE, progress = FALSE)
} else {
  tibble()
}

# Recover missing issuance dates in historical archive records.
if (nrow(existing_archive) > 0) {
  existing_archive <- existing_archive |>
    mutate(
      issuance_date = recover_issue_date(
        issuance_date,
        issuance_time_utc,
        timezone
      )
    )
}

# Allow historical CSV exports when the current forecast cache is empty.
has_forecasts <- nrow(forecast_df) > 0

if (!has_forecasts) {
  message("No current forecasts; using historical archive records.")
}

# 4. Attach forest and issuance metadata ----
new_archive <- tibble()

if (has_forecasts) {
r8_forests <- readRDS("data/r8_forests_simplified.rds") |> sf::st_transform(4326)
forecast_sf <- forecast_df |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

forecasts <- forecast_sf |>
  sf::st_join(r8_forests |> dplyr::select(forest_name = forest),
              join = sf::st_intersects, left = TRUE) |>
  sf::st_drop_geometry() |>
  mutate(forest_name = if_else(is.na(forest_name), "Unknown Forest", forest_name),
         issuance_time_utc = as.POSIXct(issuanceTime, tz = "UTC"),
         timezone = tz,
         issuance_time_local = format_local_time(issuance_time_utc, timezone),
         issuance_date_local = local_issue_date(issuance_time_utc, timezone),
         spot_safe = safe_name(spot_id, allow_decimal = TRUE),
         project_safe = safe_name(project_name),
         forest_safe = safe_name(forest_name))
# Require a known timezone for every forecast.
if (anyNA(forecasts$timezone) ||
    any(!nzchar(forecasts$timezone))) {
  stop("Some forecasts have missing timezones; refusing to assign an assumed timezone")
}
forecasts <- forecasts |>
  mutate(forecast_url = first_existing_column(forecasts, c("nws_spot_url")),
         forecast_product_id = first_existing_column(forecasts, c("X.id", "id", "product_id")),
         request_product_id = first_existing_column(forecasts, c("req_api_id")))

if (anyNA(forecasts$issuance_time_utc) || anyNA(forecasts$issuance_date_local)) {
  stop("Some forecasts have missing issuance timestamps; refusing to archive ambiguous records")
}
if (anyNA(forecasts$productText)) stop("Some forecasts have missing product text")

# 5. Preserve unique forecast versions and existing filenames ----
# A product ID alone may be reused after an in-place text revision.
# A content hash distinguishes revised versions, even with identical issuance times.
new_rows <- vector("list", nrow(forecasts))
for (i in seq_len(nrow(forecasts))) {
  f <- forecasts[i, ]
  body <- as.character(f$productText[[1]])
  hash <- text_hash(body)
  utc_stamp <- format(f$issuance_time_utc[[1]], "%Y%m%dT%H%M%SZ", tz = "UTC")
  product_id <- as.character(f$forecast_product_id[[1]])
  legacy_key <- if (!is.na(product_id) && nzchar(product_id)) product_id else
    paste(f$spot_id[[1]], format(f$issuance_time_utc[[1]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), sep = "__")
  
  # Find historical versions using the NWS product ID.
  # Preserve the original archive key and filename when text matches.
  prior <- if (
    nrow(existing_archive) &&
    "forecast_product_id" %in% names(existing_archive)
  ) {
    existing_archive |>
      filter(
        !is.na(forecast_product_id),
        forecast_product_id == product_id
      )
  } else {
    tibble()
  }
  matching_prior <- if (nrow(prior) && "forecast_text" %in% names(prior)) {
    which(!is.na(prior$forecast_text) & prior$forecast_text == body)
  } else integer()
  
  if (length(matching_prior)) {
    archive_key <- prior$archive_key[matching_prior[1]]
    archive_file <- prior$archive_file[matching_prior[1]]
  } else {
    # New versions use a content-specific key, not a potentially reused product ID.
    archive_key <- paste0(legacy_key, "__v_", hash)
    file_name <- paste0(format(f$issuance_date_local[[1]], "%Y%m%d"),
                        "__spot_", f$spot_safe[[1]], "__project_", f$project_safe[[1]],
                        "__i", utc_stamp, "__", hash, ".txt")
    archive_file <- file.path(f$forest_safe[[1]], file_name)
  }
  
  full_path <- file.path(text_archive_dir, archive_file)
  fs::dir_create(dirname(full_path))
  if (!file.exists(full_path)) {
    writeLines(body, full_path, useBytes = TRUE)
    message("Wrote: ", full_path)
  } else {
    # Do not silently associate different text with an existing filename.
    stored <- paste(readLines(full_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (!identical(
      normalize_forecast_text(stored),
      normalize_forecast_text(body)
    )) {
      stop("Existing text file differs from forecast record: ", full_path)
    }
  }
  
  new_rows[[i]] <- tibble(
    archive_key = archive_key,
    issuance_date = as.character(f$issuance_date_local[[1]]),
    issuance_time_utc = format(f$issuance_time_utc[[1]], "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    issuance_time_local = f$issuance_time_local[[1]],
    timezone = f$timezone[[1]],
    spot_id = as.character(f$spot_id[[1]]),
    project_name = as.character(f$project_name[[1]]),
    project_type = as.character(f$project_type[[1]]),
    forest = as.character(f$forest_name[[1]]),
    latitude = as.character(f$lat[[1]]),
    longitude = as.character(f$lon[[1]]),
    coordinate_source = as.character(
      first_existing_column(f, c("coordinate_source"))[[1]]
    ),
    issuing_office = as.character(first_existing_column(f, c("issuingOffice"))[[1]]),
    forecast_url = as.character(f$forecast_url[[1]]),
    forecast_product_id = product_id,
    request_product_id = as.character(f$request_product_id[[1]]),
    archive_file = archive_file,
    forecast_text = body
  )
}
new_archive <- dplyr::bind_rows(new_rows) |>
  distinct(archive_key, .keep_all = TRUE)
}

# 6. Merge historical and new records ----
master <- bind_rows(existing_archive, new_archive) |>
  distinct(archive_key, .keep_all = TRUE) |>
  arrange(desc(issuance_time_utc), forest, project_name)

# 7. Write master and affected monthly CSVs ----
# Monthly grouping uses each forecast's local issuance date.
# Initial run creates all historical monthly files; subsequent runs only change affected months.
write_csv_if_changed(master, master_path)
months <- sort(unique(substr(master$issuance_date, 1, 7)))
if (anyNA(months) || any(!grepl("^[0-9]{4}-[0-9]{2}$", months))) {
  stop("Archive contains missing or invalid issuance_date; cannot build monthly CSVs")
}
for (month in months) {
  monthly <- master |> filter(substr(issuance_date, 1, 7) == month)
  path <- file.path(spreadsheet_archive_dir,
                    paste0("spot_forecasts_", gsub("-", "_", month), ".csv"))
  write_csv_if_changed(monthly, path)
}
message("Master archive records: ", nrow(master))
message("Monthly CSV files checked: ", length(months))
