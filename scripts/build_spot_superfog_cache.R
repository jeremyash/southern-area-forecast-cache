# -------------------------------------------------
# BUILD SPOT SUPERFOG CACHE
# -------------------------------------------------

message("Spot superfog cache build script started.")

out_dir <- Sys.getenv("CACHE_OUT_DIR", unset = "cache")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cache_path <- file.path(out_dir, "superfog_cache.rds")

placeholder_cache <- list(
  generated_at = Sys.time(),
  status = "placeholder",
  message = "Replace this with real spot forecast/superfog cache generation."
)

saveRDS(placeholder_cache, cache_path)

message("Wrote cache to: ", cache_path)