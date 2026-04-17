fetch_closed_prs_list <- function(owner = source_owner, repo = source_repo) {
  gh::gh(
    "GET /repos/{owner}/{repo}/pulls",
    owner = owner, repo = repo,
    state = "closed", per_page = 100, .limit = Inf
  )
}

project_closed_pr <- function(pr) {
  tibble::tibble(
    id                  = pr$id,
    number              = pr$number,
    node_id             = pr$node_id,
    state               = pr$state,
    title               = pr$title,
    body                = pr$body %||% NA_character_,
    draft               = pr$draft %||% FALSE,
    user_login          = pr$user$login %||% NA_character_,
    author_association  = pr$author_association %||% NA_character_,
    labels              = list(purrr::map_chr(pr$labels %||% list(), "name")),
    assignees           = list(purrr::map_chr(pr$assignees %||% list(), "login")),
    requested_reviewers = list(purrr::map_chr(pr$requested_reviewers %||% list(), "login")),
    milestone_title     = pr$milestone$title %||% NA_character_,
    milestone_number    = pr$milestone$number %||% NA_integer_,
    head_ref            = pr$head$ref %||% NA_character_,
    head_sha            = pr$head$sha %||% NA_character_,
    base_ref            = pr$base$ref %||% NA_character_,
    base_sha            = pr$base$sha %||% NA_character_,
    comments_count      = pr$comments %||% NA_integer_,
    merge_commit_sha    = pr$merge_commit_sha %||% NA_character_,
    merged_at           = parse_timestamp(pr$merged_at %||% NA_character_),
    created_at          = parse_timestamp(pr$created_at),
    updated_at          = parse_timestamp(pr$updated_at),
    closed_at           = parse_timestamp(pr$closed_at %||% NA_character_),
    html_url            = pr$html_url
  )
}

build_closed_prs_table <- function() {
  listing <- fetch_closed_prs_list()
  cli::cli_inform("List endpoint returned {length(listing)} closed PRs")
  purrr::map_dfr(listing, project_closed_pr)
}
