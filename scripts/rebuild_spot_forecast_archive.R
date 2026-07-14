library(tidyverse)
library(fs)
library(lubridate)
library(openxlsx)

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
  spreadsheet_archive_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

first_match <- function(text, patterns) {
  for (pattern in patterns) {
    value <- stringr::str_match(
      text,
      stringr::regex(
        pattern,
        ignore_case = TRUE,
        dotall = TRUE
      )
    )[, 2]
    
    if (
      length(value) > 0 &&
      !is.na(value[1]) &&
      nzchar(trimws(value[1]))
    ) {
      return(trimws(value[1]))
    }
  }
  
  NA_character_
}

parse_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

parse_archive_filename <- function(path) {
  file_name <- basename(path)
  
  tibble(
    filename_date = first_match(
      file_name,
      c("^(\\d{8})")
    ),
    
    filename_spot_id = first_match(
      file_name,
      c(
        "__spot_([^_]+(?:\\.[^_]+)?)__project_",
        "^\\d{8}__([^_]+)__"
      )
    ),
    
    filename_project = first_match(
      file_name,
      c(
        "__project_(.+)\\.txt$",
        "^\\d{8}__[^_]+__(.+)\\.txt$"
      )
    )
  )
}

parse_forecast_file <- function(path) {
  text <- paste(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  
  file_meta <- parse_archive_filename(path)
  
  forest <- basename(dirname(path)) |>
    stringr::str_replace_all("_", " ")
  
  archive_date <- suppressWarnings(
    as.Date(
      file_meta$filename_date,
      format = "%Y%m%d"
    )
  )
  
  spot_id_text <- first_match(
    text,
    c(
      "\\.TAG\\s+([0-9.]+)",
      "OFILE:\\s*([0-9.]+)",
      "SPOT\\s+NUMBER:\\s*([0-9.]+)"
    )
  )
  
  project_text <- first_match(
    text,
    c(
      "PROJECT NAME:\\s*([^\\r\\n]+)",
      "PROJECT:\\s*([^\\r\\n]+)"
    )
  )
  
  lat_text <- first_match(
    text,
    c(
      "DLAT:\\s*(-?[0-9.]+)",
      "LATITUDE:\\s*(-?[0-9.]+)",
      "LAT:\\s*(-?[0-9.]+)"
    )
  )
  
  lon_text <- first_match(
    text,
    c(
      "DLON:\\s*(-?[0-9.]+)",
      "LONGITUDE:\\s*(-?[0-9.]+)",
      "LON:\\s*(-?[0-9.]+)"
    )
  )
  
  spot_id <- dplyr::coalesce(
    spot_id_text,
    file_meta$filename_spot_id
  )
  
  project_name <- dplyr::coalesce(
    project_text,
    file_meta$filename_project
  ) |>
    stringr::str_replace_all("_", " ")
  
  longitude <- parse_number(lon_text)
  
  if (!is.na(longitude)) {
    longitude <- -abs(longitude)
  }
  
  archive_file <- fs::path_rel(
    path,
    start = text_archive_dir
  )
  
  archive_key <- paste(
    archive_date,
    spot_id,
    archive_file,
    sep = "__"
  )
  
  tibble(
    archive_key = archive_key,
    issuance_date = as.character(archive_date),
    issuance_time_utc = NA_character_,
    issuance_time_local = NA_character_,
    timezone = NA_character_,
    spot_id = spot_id,
    project_name = project_name,
    project_type = "PRESCRIBED",
    forest = forest,
    latitude = parse_number(lat_text),
    longitude = longitude,
    issuing_office = NA_character_,
    forecast_url = if_else(
      is.na(spot_id),
      NA_character_,
      paste0(
        "https://spot.weather.gov/forecasts/",
        stringr::str_remove(spot_id, "\\..*$")
      )
    ),
    forecast_product_id = NA_character_,
    request_product_id = NA_character_,
    archive_file = archive_file,
    forecast_text = text
  )
}

archive_files <- list.files(
  text_archive_dir,
  pattern = "\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(archive_files) == 0) {
  stop("No archived text files were found in: ", text_archive_dir)
}

historical_archive <- purrr::map_dfr(
  archive_files,
  parse_forecast_file
) |>
  arrange(
    desc(issuance_date),
    forest,
    project_name
  ) |>
  distinct(
    archive_key,
    .keep_all = TRUE
  )

# Preserve any more-complete rows already produced from live caches.
if (file.exists(csv_path)) {
  current_archive <- readr::read_csv(
    csv_path,
    show_col_types = FALSE,
    progress = FALSE
  )
  
  historical_archive <- bind_rows(
    current_archive,
    historical_archive
  ) |>
    arrange(
      desc(issuance_time_utc),
      desc(issuance_date)
    ) |>
    distinct(
      archive_key,
      .keep_all = TRUE
    )
}

readr::write_csv(
  historical_archive,
  csv_path,
  na = ""
)

xlsx_archive <- historical_archive |>
  mutate(
    forecast_text = if_else(
      is.na(forecast_text),
      NA_character_,
      stringr::str_sub(
        forecast_text,
        1,
        32767
      )
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
  "Spot Forecast Archive",
  xlsx_archive,
  withFilter = TRUE
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fgFill = "#243447",
  fontColour = "#FFFFFF",
  halign = "center"
)

openxlsx::addStyle(
  wb,
  "Spot Forecast Archive",
  header_style,
  rows = 1,
  cols = seq_len(ncol(xlsx_archive)),
  gridExpand = TRUE
)

openxlsx::freezePane(
  wb,
  "Spot Forecast Archive",
  firstRow = TRUE
)

openxlsx::setColWidths(
  wb,
  "Spot Forecast Archive",
  cols = seq_len(ncol(xlsx_archive)),
  widths = "auto"
)

forecast_col <- which(
  names(xlsx_archive) == "forecast_text"
)

openxlsx::setColWidths(
  wb,
  "Spot Forecast Archive",
  cols = forecast_col,
  widths = 80
)

openxlsx::saveWorkbook(
  wb,
  xlsx_path,
  overwrite = TRUE
)

message("Historical files parsed: ", length(archive_files))
message("Master archive rows: ", nrow(historical_archive))
message("Saved CSV: ", csv_path)
message("Saved XLSX: ", xlsx_path)