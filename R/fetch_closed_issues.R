fetch_closed_issues_list <- function(owner = source_owner, repo = source_repo) {
  gh::gh(
    "GET /repos/{owner}/{repo}/issues",
    owner = owner, repo = repo,
    state = "closed", per_page = 100, .limit = Inf
  )
}

build_closed_issues_table <- function() {
  listing <- fetch_closed_issues_list()

  # /issues returns PRs too — filter them out
  listing <- purrr::discard(listing, ~ !is.null(.x$pull_request))
  cli::cli_inform("List endpoint returned {length(listing)} closed issues (PRs excluded)")

  purrr::map_dfr(listing, project_issue)
}
