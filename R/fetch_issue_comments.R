fetch_all_issue_comments <- function(owner = source_owner, repo = source_repo,
                                     since = NULL) {
  params <- list(
    owner = owner, repo = repo,
    sort = "updated", direction = "desc",
    per_page = 100, .limit = Inf
  )
  if (!is.null(since)) {
    params$since <- format(since, "%Y-%m-%dT%H:%M:%SZ")
  }
  do.call(gh::gh, c("GET /repos/{owner}/{repo}/issues/comments", params))
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
