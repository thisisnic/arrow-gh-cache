# GitHub serves at most 300 pages (30,000 rows) of the repository-wide
# issue comment listing per query: page 300 has no "next" link and page 301
# is a 422, so gh::gh() stops there without an error. The cap is per query,
# so the listing is read oldest-updated first in windows of that size, each
# starting where the one before ended.
issue_comments_window_size <- 30000L
issue_comments_page_size <- 100L

# Before any repository on GitHub existed, so the first window starts at the
# oldest comment.
issue_comments_epoch <- as.POSIXct("2008-01-01", tz = "UTC")

# One window: up to `window_size` comments updated at or after `since`,
# oldest update first.
fetch_issue_comments_window <- function(since, owner = source_owner,
                                        repo = source_repo,
                                        window_size = issue_comments_window_size) {
  gh::gh(
    "GET /repos/{owner}/{repo}/issues/comments",
    owner = owner, repo = repo,
    sort = "updated", direction = "asc",
    since = format(since, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    per_page = issue_comments_page_size, .limit = window_size
  )
}

# The latest updated_at in a window, which is where the next one starts.
window_end <- function(window) {
  max(parse_timestamp(vapply(window, function(x) x$updated_at, character(1))))
}

# Where the window after `window` starts, or NULL when `window` was the last.
# Only a window of less than one page is taken as the end of the listing,
# not one shorter than `window_size`: were GitHub to lower its cap, a
# shorter window would otherwise end the walk early without an error.
# `since` is inclusive, so the next window repeats the comments updated in
# the cursor's second; callers drop the repeats by id. A full window whose
# comments all share one second cannot be stepped past, and is an error
# rather than a silent stop.
next_issue_comments_cursor <- function(window, cursor,
                                       window_size = issue_comments_window_size) {
  if (length(window) < issue_comments_page_size) {
    return(NULL)
  }
  end <- window_end(window)
  if (end > cursor) {
    return(end)
  }
  if (length(window) < window_size) {
    return(NULL)
  }
  cli::cli_abort(c(
    "Cannot page past {format(cursor, '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')}.",
    i = "A full window of {window_size} issue comments was updated in that second."
  ))
}

# Every issue comment updated at or after `since` (default: all of them).
# With `until`, stops after the first window that goes past it, so the
# comments updated in that very second are all fetched.
fetch_all_issue_comments <- function(owner = source_owner, repo = source_repo,
                                     since = NULL, until = NULL,
                                     window_size = issue_comments_window_size) {
  cursor <- if (is.null(since)) issue_comments_epoch else since
  out <- list()
  repeat {
    window <- fetch_issue_comments_window(cursor, owner, repo, window_size)
    out <- c(out, window)
    cursor <- next_issue_comments_cursor(window, cursor, window_size)
    if (is.null(cursor) || (!is.null(until) && cursor > until)) {
      break
    }
    cli::cli_inform("  ... {length(out)} issue comments so far")
  }
  ids <- vapply(out, function(x) as.numeric(x$id), numeric(1))
  out[!duplicated(ids)]
}

# Extract issue/PR number from issue_url like
# "https://api.github.com/repos/apache/arrow/issues/12345"
extract_issue_number <- function(url) {
  as.integer(sub(".*/issues/", "", url))
}

project_issue_comment <- function(comment) {
  tibble::tibble(
    id                 = comment$id,
    node_id            = comment$node_id,
    issue_number       = extract_issue_number(comment$issue_url),
    user_login         = comment$user$login %||% NA_character_,
    author_association = comment$author_association %||% NA_character_,
    body               = comment$body %||% NA_character_,
    created_at         = parse_timestamp(comment$created_at),
    updated_at         = parse_timestamp(comment$updated_at),
    html_url           = comment$html_url
  )
}

build_issue_comments_table <- function(since = NULL) {
  listing <- fetch_all_issue_comments(since = since)
  cli::cli_inform("Fetched {length(listing)} issue comments")
  purrr::map_dfr(listing, project_issue_comment)
}
