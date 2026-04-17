fetch_all_review_comments <- function(owner = source_owner, repo = source_repo,
                                      since = NULL) {
  params <- list(
    owner = owner, repo = repo,
    sort = "updated", direction = "desc",
    per_page = 100, .limit = Inf
  )
  if (!is.null(since)) {
    params$since <- format(since, "%Y-%m-%dT%H:%M:%SZ")
  }
  do.call(gh::gh, c("GET /repos/{owner}/{repo}/pulls/comments", params))
}

# Extract PR number from pull_request_url like
# "https://api.github.com/repos/apache/arrow/pulls/12345"
extract_pr_number <- function(url) {
  as.integer(sub(".*/pulls/", "", url))
}

project_review_comment <- function(comment) {
  tibble::tibble(
    id                     = comment$id,
    node_id                = comment$node_id,
    pull_request_review_id = comment$pull_request_review_id %||% NA_integer_,
    pr_number              = extract_pr_number(comment$pull_request_url),
    user_login             = comment$user$login %||% NA_character_,
    author_association     = comment$author_association %||% NA_character_,
    body                   = comment$body %||% NA_character_,
    path                   = comment$path %||% NA_character_,
    diff_hunk              = comment$diff_hunk %||% NA_character_,
    commit_id              = comment$commit_id %||% NA_character_,
    created_at             = parse_timestamp(comment$created_at),
    updated_at             = parse_timestamp(comment$updated_at),
    html_url               = comment$html_url
  )
}

build_review_comments_table <- function(since = NULL) {
  listing <- fetch_all_review_comments(since = since)
  cli::cli_inform("Fetched {length(listing)} review comments")
  purrr::map_dfr(listing, project_review_comment)
}
