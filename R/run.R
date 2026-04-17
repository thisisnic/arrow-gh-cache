suppressPackageStartupMessages({
  library(arrow)
  library(cli)
  library(dplyr)
  library(purrr)
  library(tibble)
})

source("R/config.R")
source("R/fetch_open_prs.R")
source("R/fetch_closed_prs.R")
source("R/fetch_open_issues.R")
source("R/fetch_closed_issues.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

fs::dir_create("artifacts")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

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
  if (file.exists(dest)) read_parquet(dest) else NULL
}

# Fetch all items (issues + PRs) updated since `since` via the /issues endpoint
fetch_updated_since <- function(since) {
  since_str <- format(since, "%Y-%m-%dT%H:%M:%SZ")
  cli_inform("Fetching items updated since {since_str}")

  gh::gh(
    "GET /repos/{owner}/{repo}/issues",
    owner = source_owner, repo = source_repo,
    state = "all", since = since_str,
    per_page = 100, .limit = Inf
  )
}

# Merge updated rows into an existing table, deduping by number
merge_by_number <- function(existing, updated) {
  if (nrow(updated) == 0) return(existing)
  # Parquet round-tripping makes list columns typed (list<character>);
  # newly built rows have plain lists. Coerce to plain lists so bind_rows works.
  list_cols <- names(existing)[vapply(existing, is.list, logical(1))]
  for (col in list_cols) {
    existing[[col]] <- as.list(existing[[col]])
  }
  existing |>
    filter(!number %in% updated$number) |>
    bind_rows(updated)
}

# ---------------------------------------------------------------------------
# Download existing assets
# ---------------------------------------------------------------------------

cli_inform("Downloading existing assets from release...")
open_prs      <- download_asset("open_prs.parquet")
closed_prs    <- download_asset("closed_prs.parquet")
open_issues   <- download_asset("open_issues.parquet")
closed_issues <- download_asset("closed_issues.parquet")

all_exist <- !is.null(open_prs) && !is.null(closed_prs) &&
             !is.null(open_issues) && !is.null(closed_issues)

# ---------------------------------------------------------------------------
# Cold start: backfill any missing tables
# ---------------------------------------------------------------------------

if (is.null(open_prs)) {
  cli_inform("Backfilling open_prs...")
  open_prs <- build_open_prs_table()
}

if (is.null(closed_prs)) {
  cli_inform("Backfilling closed_prs...")
  closed_prs <- build_closed_prs_table()
}

if (is.null(open_issues)) {
  cli_inform("Backfilling open_issues...")
  open_issues <- build_open_issues_table()
}

if (is.null(closed_issues)) {
  cli_inform("Backfilling closed_issues...")
  closed_issues <- build_closed_issues_table()
}

# ---------------------------------------------------------------------------
# Delta update: fetch only what changed since last run
# ---------------------------------------------------------------------------

if (all_exist) {
  since <- max(
    max(open_prs$updated_at, na.rm = TRUE),
    max(closed_prs$updated_at, na.rm = TRUE),
    max(open_issues$updated_at, na.rm = TRUE),
    max(closed_issues$updated_at, na.rm = TRUE)
  )

  items <- fetch_updated_since(since)
  cli_inform("Got {length(items)} updated items")

  if (length(items) > 0) {
    is_pr <- map_lgl(items, ~ !is.null(.x$pull_request))

    # --- Process updated PRs ---
    pr_items <- items[is_pr]
    if (length(pr_items) > 0) {
      # Fetch detail for open PRs (for mergeable, additions, etc.)
      # Use list-level projection for closed PRs
      updated_open <- list()
      updated_closed <- list()

      for (i in seq_along(pr_items)) {
        item <- pr_items[[i]]
        if (item$state == "open") {
          Sys.sleep(0.1)
          detail <- fetch_pr_detail(item$number)
          row <- project_pr(utils::modifyList(item, detail))
          updated_open <- c(updated_open, list(row))
        } else {
          # Need to fetch from /pulls endpoint for closed PR fields
          # (the /issues endpoint doesn't have merge_commit_sha, etc.)
          Sys.sleep(0.1)
          pr_data <- gh::gh(
            "GET /repos/{owner}/{repo}/pulls/{number}",
            owner = source_owner, repo = source_repo, number = item$number
          )
          row <- project_closed_pr(pr_data)
          updated_closed <- c(updated_closed, list(row))
        }
        if ((i %% 50) == 0) {
          cli_inform("  ... processed {i}/{length(pr_items)} PRs")
        }
      }

      if (length(updated_open) > 0) {
        new_open_prs <- bind_rows(updated_open)
        # Remove any that moved to closed
        open_prs <- open_prs |> filter(!number %in% closed_prs$number)
        open_prs <- merge_by_number(open_prs, new_open_prs)
      }

      if (length(updated_closed) > 0) {
        new_closed_prs <- bind_rows(updated_closed)
        # Remove from open_prs any that are now closed
        open_prs <- open_prs |> filter(!number %in% new_closed_prs$number)
        closed_prs <- merge_by_number(closed_prs, new_closed_prs)
      }
    }

    # --- Process updated issues ---
    issue_items <- items[!is_pr]
    if (length(issue_items) > 0) {
      updated_open_issues <- list()
      updated_closed_issues <- list()

      for (item in issue_items) {
        row <- project_issue(item)
        if (item$state == "open") {
          updated_open_issues <- c(updated_open_issues, list(row))
        } else {
          updated_closed_issues <- c(updated_closed_issues, list(row))
        }
      }

      if (length(updated_open_issues) > 0) {
        new_open <- bind_rows(updated_open_issues)
        open_issues <- open_issues |> filter(!number %in% new_open$number)
        # Also remove any that were reopened from closed
        closed_issues <- closed_issues |> filter(!number %in% new_open$number)
        open_issues <- merge_by_number(open_issues, new_open)
      }

      if (length(updated_closed_issues) > 0) {
        new_closed <- bind_rows(updated_closed_issues)
        # Remove from open any that are now closed
        open_issues <- open_issues |> filter(!number %in% new_closed$number)
        closed_issues <- merge_by_number(closed_issues, new_closed)
      }
    }
  }
}

# ---------------------------------------------------------------------------
# Write outputs
# ---------------------------------------------------------------------------

write_parquet(open_prs, "artifacts/open_prs.parquet")
cli_inform("Wrote {nrow(open_prs)} rows to open_prs.parquet")

write_parquet(closed_prs, "artifacts/closed_prs.parquet")
cli_inform("Wrote {nrow(closed_prs)} rows to closed_prs.parquet")

write_parquet(open_issues, "artifacts/open_issues.parquet")
cli_inform("Wrote {nrow(open_issues)} rows to open_issues.parquet")

write_parquet(closed_issues, "artifacts/closed_issues.parquet")
cli_inform("Wrote {nrow(closed_issues)} rows to closed_issues.parquet")
