fetch_open_issues_list <- function(owner = source_owner, repo = source_repo) {
  gh::gh(
    "GET /repos/{owner}/{repo}/issues",
    owner = owner, repo = repo,
    state = "open", per_page = 100, .limit = Inf
  )
}

project_issue <- function(issue) {
  tibble::tibble(
    id                 = issue$id,
    number             = issue$number,
    node_id            = issue$node_id,
    state              = issue$state,
    title              = issue$title,
    body               = issue$body %||% NA_character_,
    user_login         = issue$user$login %||% NA_character_,
    author_association = issue$author_association %||% NA_character_,
    labels             = list(purrr::map_chr(issue$labels %||% list(), "name")),
    assignees          = list(purrr::map_chr(issue$assignees %||% list(), "login")),
    milestone_title    = issue$milestone$title %||% NA_character_,
    milestone_number   = issue$milestone$number %||% NA_integer_,
    comments_count     = issue$comments %||% NA_integer_,
    created_at         = parse_timestamp(issue$created_at),
    updated_at         = parse_timestamp(issue$updated_at),
    closed_at          = parse_timestamp(issue$closed_at %||% NA_character_),
    html_url           = issue$html_url
  )
}

build_open_issues_table <- function() {
  listing <- fetch_open_issues_list()

  # /issues returns PRs too — filter them out
  listing <- purrr::discard(listing, ~ !is.null(.x$pull_request))
  cli::cli_inform("List endpoint returned {length(listing)} open issues (PRs excluded)")

  purrr::map_dfr(listing, project_issue)
}
