library(tidyverse)
library(sf)
library(fs)
library(lubridate)

cache_path <- Sys.getenv(
  "SPOT_CACHE_PATH",
  unset = "cache/spot_superfog_cache.rds"
)

out_dir <- Sys.getenv(
  "TEXT_ARCHIVE_OUT_DIR",
  unset = "text-archive"
)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

safe_name <- function(x, allow_decimal = FALSE) {
  x <- ifelse(is.na(x) | !nzchar(x), "Unknown", x)
  
  pattern <- if (allow_decimal) "[^A-Za-z0-9.]+" else "[^A-Za-z0-9]+"
  
  x %>%
    str_replace_all(pattern, "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove("^_") %>%
    str_remove("_$")
}

x <- readRDS(cache_path)
forecast_df <- x$forecast_df

if (nrow(forecast_df) == 0) {
  message("No forecast rows found. Nothing to archive.")
  quit(save = "no", status = 0)
}

r8_forests <- readRDS("data/r8_forests_simplified.rds") %>%
  st_transform(4326)

forecast_sf <- forecast_df %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

forecast_with_forest <- forecast_sf %>%
  st_join(
    r8_forests %>% select(forest_name = forest),
    join = st_intersects,
    left = TRUE
  ) %>%
  st_drop_geometry() %>%
  mutate(
    forest_name = if_else(is.na(forest_name), "Unknown_Forest", forest_name),
    file_date = format(as.Date(issuanceTime), "%Y%m%d"),
    spot_safe = safe_name(spot_id, allow_decimal = TRUE),
    project_safe = safe_name(project_name),
    forest_safe = safe_name(forest_name),
    forest_dir = file.path(out_dir, forest_safe),
    file_name = paste0(
      file_date,
      "__spot_", spot_safe,
      "__project_", project_safe,
      ".txt"
    ),
    file_path = file.path(forest_dir, file_name)
  ) %>%
  distinct(file_path, .keep_all = TRUE)

walk(seq_len(nrow(forecast_with_forest)), function(i) {
  this_dir <- forecast_with_forest$forest_dir[i]
  this_file <- forecast_with_forest$file_path[i]
  this_text <- forecast_with_forest$productText[i]
  
  dir_create(this_dir)
  
  if (!file.exists(this_file)) {
    writeLines(this_text, this_file, useBytes = TRUE)
    message("Wrote: ", this_file)
  } else {
    message("Exists, skipped: ", this_file)
  }
})