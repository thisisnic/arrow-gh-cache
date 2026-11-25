# One-off repair of issue_comments.parquet on the cache release.
#
# The first backfill listed issue comments newest-updated first and GitHub
# cut it off after 30,000 rows (see R/fetch_issue_comments.R), so the asset
# has nothing last updated before that cut-off. This fetches everything
# older, in windows, and merges it into the asset as it is on the release.
#
# Run it locally from the repository root, with a personal token rather
# than in Actions (the history is thousands of requests):
#
#   Rscript R/backfill_issue_comments.R
#
# Each window of 5,000 comments (50 requests) is saved under
# artifacts/backfill/ as it arrives, and a rerun picks up after the last one
# saved, so an interrupted run loses one window at most. To do it in pieces,
# stop after a number of windows:
#
#   Rscript R/backfill_issue_comments.R --windows 20
#
# Once every window is in, the result is written to
# artifacts/issue_comments.parquet; uploading it is left to you (the command
# is printed at the end).

suppressPackageStartupMessages({
  library(arrow)
  library(cli)
  library(dplyr)
  library(purrr)
  library(tibble)
})

source("R/config.R")
source("R/fetch_issue_comments.R")

backfill_dir <- file.path("artifacts", "backfill")

# Smaller than the 30,000 GitHub allows per query: a window is what an
# interruption loses, and a smaller one costs no more requests.
backfill_window_size <- 5000L

# The release's asset as it is now. download_asset() keeps a file that is
# already in artifacts/, so a copy left by an earlier run is removed first.
current_issue_comments <- function() {
  dest <- file.path("artifacts", "issue_comments.parquet")
  if (file.exists(dest)) {
    file.remove(dest)
  }
  download_asset("issue_comments.parquet")
}

saved_windows <- function(dir = backfill_dir) {
  sort(as.character(fs::dir_ls(dir, glob = "*.parquet")))
}

# Fetch the comments last updated up to and including `until` into one
# parquet file per window under `dir`, starting after the windows already
# there. `until` itself is included because the first backfill may have
# been cut off part way through that second. Stops after `max_windows`
# windows; returns whether `until` was reached.
fetch_backfill_windows <- function(until, dir = backfill_dir,
                                   window_size = backfill_window_size,
                                   max_windows = Inf) {
  fs::dir_create(dir)
  saved <- saved_windows(dir)
  cursor <- issue_comments_epoch
  for (path in saved) {
    cursor <- max(cursor, max(read_parquet(path)$updated_at))
  }
  n <- length(saved)
  if (n > 0) {
    cli_inform("Resuming after {n} saved window{?s}, from {format(cursor, tz = 'UTC')}")
  }
  fetched <- 0
  while (cursor <= until) {
    if (fetched >= max_windows) {
      return(invisible(FALSE))
    }
    fetched <- fetched + 1
    window <- fetch_issue_comments_window(cursor, window_size = window_size)
    if (length(window) == 0) {
      break
    }
    n <- n + 1
    rows <- map_dfr(window, project_issue_comment)
    write_parquet(rows, file.path(dir, sprintf("window_%04d.parquet", n)))
    cli_inform(
      "Window {n}: {nrow(rows)} comments, updated {format(min(rows$updated_at), tz = 'UTC')} to {format(max(rows$updated_at), tz = 'UTC')}"
    )
    cursor <- next_issue_comments_cursor(window, cursor, window_size)
    if (is.null(cursor)) {
      break
    }
  }
  invisible(TRUE)
}

# The number after --windows on the command line, or Inf without it.
windows_arg <- function(args = commandArgs(trailingOnly = TRUE)) {
  at <- match("--windows", args)
  if (is.na(at)) {
    return(Inf)
  }
  n <- suppressWarnings(as.integer(args[at + 1]))
  if (is.na(n) || n < 1) {
    cli_abort("--windows takes a whole number of windows, 1 or more.")
  }
  n
}

# The saved windows and the release's rows as one table. A comment in both
# (windows overlap by a second, the last window runs past the cut-off, and
# the scheduled workflow may have refetched an edited comment meanwhile)
# keeps the copy updated last.
merge_backfill <- function(existing, dir = backfill_dir) {
  fetched <- map_dfr(saved_windows(dir), read_parquet)
  merged <- bind_rows(existing, fetched) |>
    arrange(desc(updated_at)) |>
    distinct(id, .keep_all = TRUE)
  stopifnot(
    !anyDuplicated(merged$id),
    all(existing$id %in% merged$id),
    all(fetched$id %in% merged$id)
  )
  merged
}

# Only when run as a script, so the functions can be sourced on their own.
if (sys.nframe() == 0) {
  fs::dir_create("artifacts")

  existing <- current_issue_comments()
  if (is.null(existing)) {
    cli_abort("The release has no issue_comments.parquet; R/run.R builds it from scratch.")
  }
  until <- min(existing$updated_at, na.rm = TRUE)
  cli_inform("Release has {nrow(existing)} issue comments, none updated before {format(until, tz = 'UTC')}")

  max_windows <- windows_arg()
  if (!fetch_backfill_windows(until, max_windows = max_windows)) {
    cli_inform(c(
      "Stopped after {max_windows} window{?s}; {length(saved_windows())} saved so far.",
      i = "Run again to continue. Nothing is merged until every window is in."
    ))
    quit(status = 0)
  }

  # Read the release again: the scheduled workflow may have replaced the
  # asset while the windows were being fetched.
  existing <- current_issue_comments()
  if (is.null(existing)) {
    cli_abort("Could not download issue_comments.parquet again; rerun to merge the saved windows.")
  }
  merged <- merge_backfill(existing)
  stopifnot(nrow(merged) >= nrow(existing))

  cache_repo <- Sys.getenv("GITHUB_REPOSITORY", "thisisnic/arrow-gh-cache")
  write_parquet(merged, "artifacts/issue_comments.parquet")
  cli_inform(c(
    "Wrote {nrow(merged)} rows to artifacts/issue_comments.parquet ({nrow(merged) - nrow(existing)} added)",
    i = "Oldest update is now {format(min(merged$updated_at), tz = 'UTC')}",
    i = "Upload when no cache workflow run is in progress:",
    " " = "gh release upload {release_tag} artifacts/issue_comments.parquet --repo {cache_repo} --clobber"
  ))
}
