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

as_chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
as_int <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)
as_dbl <- function(x) if (is.null(x)) NA_real_    else as.double(x)
as_lgl <- function(x) if (is.null(x)) NA          else as.logical(x)

project_pr <- function(pr) {
  tibble::tibble(
    id                    = as_dbl(pr$id),
    number                = as_int(pr$number),
    node_id               = as_chr(pr$node_id),
    state                 = as_chr(pr$state),
    title                 = as_chr(pr$title),
    body                  = as_chr(pr$body),
    draft                 = as_lgl(pr$draft %||% FALSE),
    user_login            = as_chr(pr$user$login),
    author_association    = as_chr(pr$author_association),
    labels                = list(purrr::map_chr(pr$labels %||% list(), "name")),
    assignees             = list(purrr::map_chr(pr$assignees %||% list(), "login")),
    requested_reviewers   = list(purrr::map_chr(pr$requested_reviewers %||% list(), "login")),
    milestone_title       = as_chr(pr$milestone$title),
    milestone_number      = as_int(pr$milestone$number),
    head_ref              = as_chr(pr$head$ref),
    head_sha              = as_chr(pr$head$sha),
    base_ref              = as_chr(pr$base$ref),
    base_sha              = as_chr(pr$base$sha),
    mergeable             = as_lgl(pr$mergeable),
    mergeable_state       = as_chr(pr$mergeable_state),
    rebaseable            = as_lgl(pr$rebaseable),
    additions             = as_dbl(pr$additions),
    deletions             = as_dbl(pr$deletions),
    changed_files         = as_int(pr$changed_files),
    commits               = as_int(pr$commits),
    review_comments_count = as_int(pr$review_comments),
    comments_count        = as_int(pr$comments),
    merge_commit_sha      = as_chr(pr$merge_commit_sha),
    merged                = as_lgl(pr$merged %||% FALSE),
    merged_at             = as_chr(pr$merged_at),
    merged_by             = as_chr(pr$merged_by$login),
    created_at            = as_chr(pr$created_at),
    updated_at            = as_chr(pr$updated_at),
    closed_at             = as_chr(pr$closed_at),
    html_url              = as_chr(pr$html_url)
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
