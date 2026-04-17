suppressPackageStartupMessages({
  library(arrow)
  library(cli)
  library(purrr)
  library(tibble)
})

source("R/config.R")
source("R/fetch_open_prs.R")

fs::dir_create("artifacts")

cli_inform("Building open_prs table from {source_owner}/{source_repo}")
open_prs <- build_open_prs_table()

out <- "artifacts/open_prs.parquet"
write_parquet(open_prs, out)
cli_inform("Wrote {nrow(open_prs)} rows to {out}")
