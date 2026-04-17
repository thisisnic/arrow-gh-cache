`%||%` <- function(x, y) if (is.null(x)) y else x

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

project_pr <- function(pr) {
  tibble::tibble(
    id                    = pr$id,
    number                = pr$number,
    node_id               = pr$node_id,
    state                 = pr$state,
    title                 = pr$title,
    body                  = pr$body %||% NA_character_,
    draft                 = pr$draft %||% FALSE,
    user_login            = pr$user$login %||% NA_character_,
    author_association    = pr$author_association %||% NA_character_,
    labels                = list(purrr::map_chr(pr$labels %||% list(), "name")),
    assignees             = list(purrr::map_chr(pr$assignees %||% list(), "login")),
    requested_reviewers   = list(purrr::map_chr(pr$requested_reviewers %||% list(), "login")),
    milestone_title       = pr$milestone$title %||% NA_character_,
    milestone_number      = pr$milestone$number %||% NA_integer_,
    head_ref              = pr$head$ref %||% NA_character_,
    head_sha              = pr$head$sha %||% NA_character_,
    base_ref              = pr$base$ref %||% NA_character_,
    base_sha              = pr$base$sha %||% NA_character_,
    mergeable             = pr$mergeable %||% NA,
    mergeable_state       = pr$mergeable_state %||% NA_character_,
    rebaseable            = pr$rebaseable %||% NA,
    additions             = pr$additions %||% NA_integer_,
    deletions             = pr$deletions %||% NA_integer_,
    changed_files         = pr$changed_files %||% NA_integer_,
    commits               = pr$commits %||% NA_integer_,
    review_comments_count = pr$review_comments %||% NA_integer_,
    comments_count        = pr$comments %||% NA_integer_,
    merge_commit_sha      = pr$merge_commit_sha %||% NA_character_,
    merged                = pr$merged %||% FALSE,
    merged_at             = pr$merged_at %||% NA_character_,
    merged_by             = pr$merged_by$login %||% NA_character_,
    created_at            = pr$created_at,
    updated_at            = pr$updated_at,
    closed_at             = pr$closed_at %||% NA_character_,
    html_url              = pr$html_url
  )
}

build_open_prs_table <- function(sleep = 0.1) {
  listing <- fetch_open_prs_list()
  cli::cli_inform("List endpoint returned {length(listing)} open PRs")

  purrr::map_dfr(seq_along(listing), function(i) {
    pr <- listing[[i]]
    Sys.sleep(sleep)
    detail <- fetch_pr_detail(pr$number)
    if ((i %% 50) == 0) {
      cli::cli_inform("  ... detail fetched for {i}/{length(listing)}")
    }
    project_pr(utils::modifyList(pr, detail))
  })
}
