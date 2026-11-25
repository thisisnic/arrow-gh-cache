source_owner <- "apache"
source_repo  <- "arrow"
release_tag  <- "cache-latest"

parse_timestamp <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Download one asset of the cache release into artifacts/ and read it, or
# NULL when the release does not have it.
download_asset <- function(filename) {
  dest <- file.path("artifacts", filename)
  repo <- Sys.getenv("GITHUB_REPOSITORY", "thisisnic/arrow-gh-cache")
  system2(
    "gh", c("release", "download", release_tag,
            "--repo", repo,
            "--pattern", filename,
            "--dir", "artifacts"),
    stdout = FALSE, stderr = FALSE
  )
  if (file.exists(dest)) arrow::read_parquet(dest) else NULL
}
