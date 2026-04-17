suppressPackageStartupMessages({
  library(arrow)
  library(cli)
  library(purrr)
  library(tibble)
})

source("R/config.R")
source("R/fetch_open_prs.R")
source("R/fetch_closed_prs.R")
source("R/fetch_open_issues.R")

fs::dir_create("artifacts")

# Download existing assets from the release so we can skip tables that are
# already cached. Returns TRUE if the asset was found and downloaded.
download_asset <- function(filename) {
  dest <- file.path("artifacts", filename)
  repo <- Sys.getenv("GITHUB_REPOSITORY", "thisisnic/arrow-gh-cache")
  result <- system2(
    "gh", c("release", "download", release_tag,
            "--repo", repo,
            "--pattern", filename,
            "--dir", "artifacts"),
    stdout = FALSE, stderr = FALSE
  )
  file.exists(dest)
}

# --- open_prs ---
if (download_asset("open_prs.parquet")) {
  cli_inform("open_prs.parquet already exists in release, skipping fetch")
} else {
  cli_inform("Building open_prs table from {source_owner}/{source_repo}")
  open_prs <- build_open_prs_table()
  write_parquet(open_prs, "artifacts/open_prs.parquet")
  cli_inform("Wrote {nrow(open_prs)} rows to artifacts/open_prs.parquet")
}

# --- closed_prs ---
if (download_asset("closed_prs.parquet")) {
  cli_inform("closed_prs.parquet already exists in release, skipping fetch")
} else {
  cli_inform("Building closed_prs table from {source_owner}/{source_repo}")
  closed_prs <- build_closed_prs_table()
  write_parquet(closed_prs, "artifacts/closed_prs.parquet")
  cli_inform("Wrote {nrow(closed_prs)} rows to artifacts/closed_prs.parquet")
}

# --- open_issues ---
if (download_asset("open_issues.parquet")) {
  cli_inform("open_issues.parquet already exists in release, skipping fetch")
} else {
  cli_inform("Building open_issues table from {source_owner}/{source_repo}")
  open_issues <- build_open_issues_table()
  write_parquet(open_issues, "artifacts/open_issues.parquet")
  cli_inform("Wrote {nrow(open_issues)} rows to artifacts/open_issues.parquet")
}
