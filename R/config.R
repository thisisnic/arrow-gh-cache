source_owner <- "apache"
source_repo  <- "arrow"
release_tag  <- "cache-latest"

parse_timestamp <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}
