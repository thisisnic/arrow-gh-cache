`%||%` <- function(x, y) if (is.null(x)) y else x

parse_ts <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

fetch_open_prs_list <- function(owner = source_owner, repo = source_repo) {
  gh::gh(
    "GET /repos/{owner}/{repo}/pulls",
    owner = owner, repo = repo,
    state = "open", per_page = 100, .limit = Inf
  )
}

fetch_pr_detail <- function(number, owner = source_owner, repo = source_repo) {
  gh::gh(
    "GET /repos/{owner}/{repo}/pulls/{number}",
    owner = owner, repo = repo, number = number
  )
}

project_prs <- function(raw) {
  tibble::tibble(
    id                    = purrr::map_dbl(raw, "id"),
    number                = purrr::map_int(raw, "number"),
    node_id               = purrr::map_chr(raw, "node_id"),
    state                 = purrr::map_chr(raw, "state"),
    title                 = purrr::map_chr(raw, "title"),
    body                  = purrr::map_chr(raw, ~ .x$body %||% NA_character_),
    draft                 = purrr::map_lgl(raw, ~ .x$draft %||% NA),
    user_login            = purrr::map_chr(raw, ~ .x$user$login %||% NA_character_),
    author_association    = purrr::map_chr(raw, ~ .x$author_association %||% NA_character_),
    labels                = purrr::map(raw, ~ purrr::map_chr(.x$labels %||% list(), "name")),
    assignees             = purrr::map(raw, ~ purrr::map_chr(.x$assignees %||% list(), "login")),
    requested_reviewers   = purrr::map(raw, ~ purrr::map_chr(.x$requested_reviewers %||% list(), "login")),
    milestone_title       = purrr::map_chr(raw, ~ .x$milestone$title %||% NA_character_),
    milestone_number      = purrr::map_int(raw, ~ .x$milestone$number %||% NA_integer_),
    head_ref              = purrr::map_chr(raw, ~ .x$head$ref %||% NA_character_),
    head_sha              = purrr::map_chr(raw, ~ .x$head$sha %||% NA_character_),
    base_ref              = purrr::map_chr(raw, ~ .x$base$ref %||% NA_character_),
    base_sha              = purrr::map_chr(raw, ~ .x$base$sha %||% NA_character_),
    mergeable             = purrr::map_lgl(raw, ~ as.logical(.x$mergeable %||% NA)),
    mergeable_state       = purrr::map_chr(raw, ~ .x$mergeable_state %||% NA_character_),
    rebaseable            = purrr::map_lgl(raw, ~ as.logical(.x$rebaseable %||% NA)),
    additions             = purrr::map_dbl(raw, ~ .x$additions %||% NA_real_),
    deletions             = purrr::map_dbl(raw, ~ .x$deletions %||% NA_real_),
    changed_files         = purrr::map_int(raw, ~ .x$changed_files %||% NA_integer_),
    commits               = purrr::map_int(raw, ~ .x$commits %||% NA_integer_),
    review_comments_count = purrr::map_int(raw, ~ .x$review_comments %||% NA_integer_),
    comments_count        = purrr::map_int(raw, ~ .x$comments %||% NA_integer_),
    merge_commit_sha      = purrr::map_chr(raw, ~ .x$merge_commit_sha %||% NA_character_),
    merged                = purrr::map_lgl(raw, ~ .x$merged %||% FALSE),
    merged_at             = parse_ts(purrr::map_chr(raw, ~ .x$merged_at %||% NA_character_)),
    merged_by             = purrr::map_chr(raw, ~ .x$merged_by$login %||% NA_character_),
    created_at            = parse_ts(purrr::map_chr(raw, "created_at")),
    updated_at            = parse_ts(purrr::map_chr(raw, "updated_at")),
    closed_at             = parse_ts(purrr::map_chr(raw, ~ .x$closed_at %||% NA_character_)),
    html_url              = purrr::map_chr(raw, "html_url")
  )
}

build_open_prs_table <- function(sleep = 0.1) {
  listing <- fetch_open_prs_list()
  cli::cli_inform("List endpoint returned {length(listing)} open PRs")

  merged <- purrr::imap(listing, function(pr, i) {
    Sys.sleep(sleep)
    if ((i %% 50) == 0) {
      cli::cli_inform("  ... detail fetched for {i}/{length(listing)}")
    }
    utils::modifyList(pr, fetch_pr_detail(pr$number))
  })

  project_prs(merged)
}
