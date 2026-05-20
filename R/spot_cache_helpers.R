download_remote_cache <- function(url) {
  tf <- tempfile(fileext = ".rds")
  
  resp <- httr2::request(url) |>
    httr2::req_perform()
  
  writeBin(httr2::resp_body_raw(resp), tf)
  x <- readRDS(tf)
  
  if (!all(c("forecast_df", "sfog_tables", "last_refresh") %in% names(x))) {
    stop("Remote cache is missing one or more required objects: forecast_df, sfog_tables, last_refresh")
  }
  
  x
}

format_issued_datetime <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_character_)
  
  x_posix <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC"))
  
  if (all(is.na(x_posix))) {
    x_posix <- suppressWarnings(lubridate::ymd_hm(x, tz = "UTC"))
  }
  
  if (all(is.na(x_posix))) {
    x_posix <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
  }
  
  if (all(is.na(x_posix))) {
    return(as.character(x))
  }
  
  format(lubridate::with_tz(x_posix, "America/New_York"), "%Y-%m-%d %H:%M %Z")
}