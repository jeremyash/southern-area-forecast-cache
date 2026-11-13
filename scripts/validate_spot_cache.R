# validate_spot_cache.R
# Purpose: Validate a multi-issuance spot/superfog cache before publication.
# Source: Stage A cache schema; no API calls or cache modifications.

# 1. Read cache ----

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript validate_spot_cache.R PATH_TO_CACHE.rds")
path <- args[[1]]
if (!file.exists(path)) stop("Cache file does not exist: ", path)
x <- readRDS(path)

# 2. Validate forecast and screening structure ----

required <- c("forecast_df", "sfog_tables", "last_refresh")
if (!all(required %in% names(x))) stop("Missing cache components: ", paste(setdiff(required, names(x)), collapse = ", "))
df <- x$forecast_df
tables <- x$sfog_tables
if (!is.data.frame(df) || !is.list(tables)) stop("Invalid forecast_df or sfog_tables type")
if (nrow(df) == 0L) stop("Refusing empty forecast cache")
if (nrow(df) != length(tables)) stop("Forecast rows and screening tables are misaligned")
needed <- c("spot_id", "issuanceTime", "forecast_version_id")
if (!all(needed %in% names(df))) stop("Missing forecast columns: ", paste(setdiff(needed, names(df)), collapse = ", "))
if (anyNA(df$spot_id) || anyNA(df$issuanceTime) || anyNA(df$forecast_version_id)) stop("Missing forecast identity values")
if (any(!nzchar(as.character(df$forecast_version_id)))) stop("Blank forecast version IDs")
if (anyDuplicated(df$forecast_version_id)) stop("Duplicate forecast version IDs")
if (!inherits(df$issuanceTime, "POSIXt")) stop("issuanceTime is not a datetime")
expected_id <- paste0(as.character(df$spot_id), "|", format(df$issuanceTime, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
if (!identical(as.character(df$forecast_version_id), expected_id)) stop("Forecast version IDs do not match spot ID and UTC issuance")
valid_tables <- vapply(tables, function(z) is.data.frame(z) && nrow(z) > 0L && "DATETIME" %in% names(z), logical(1))
if (!all(valid_tables)) stop("One or more screening tables are empty or invalid")

# 3. Validate local issuance dates ----

local_dates <- as.Date(df$issuanceTime, tz = "America/New_York")

# Use today's date unless a historical validation date is supplied.
validation_date <- as.Date(
  Sys.getenv(
    "VALIDATION_DATE",
    unset = format(Sys.time(), "%Y-%m-%d", tz = "America/New_York")
  )
)

if (is.na(validation_date)) {
  stop("Invalid VALIDATION_DATE; expected YYYY-MM-DD")
}

if (anyNA(local_dates) ||
    any(!local_dates %in% c(validation_date, validation_date - 1L))) {
  stop("Forecast issuance outside validation date/yesterday (America/New_York)")
}
if (length(x$last_refresh) != 1L || is.na(x$last_refresh)) stop("Invalid last_refresh")

# 4. Report validation result ----

cat("PASS: cache structure and issuance dates\n")
cat("Forecast versions:", nrow(df), "\n")
cat("Screening tables:", length(tables), "\n")
cat("Distinct spot IDs:", length(unique(df$spot_id)), "\n")
cat("Last refresh:", as.character(x$last_refresh), "\n")
if (is.null(x$last_successful_fetch) || is.na(x$last_successful_fetch)) {
  warning("No recorded successful API fetch; review before production publication")
}
