# arrow-gh-cache

Parquet cache of `apache/arrow` GitHub activity. Published as assets on the
rolling [`cache-latest`](../../releases/tag/cache-latest) release — the latest
snapshot only, no history.

## Consuming

Each table has a stable public URL:

```r
url <- "https://github.com/thisisnic/arrow-gh-cache/releases/download/cache-latest/open_prs.parquet"
open_prs <- arrow::read_parquet(url)
```

No clone, no auth, no LFS required.

## Available tables

| Asset | Contents |
|---|---|
| `open_prs.parquet` | Open pull requests with list-level + detail-endpoint fields |

More tables land in later stages (open issues, then closed PRs/issues, then
comments).

## How it works

A scheduled GitHub Action (`.github/workflows/update_cache.yaml`) runs daily,
calls the GitHub REST API, writes parquet files to `artifacts/`, and uploads
them to the `cache-latest` release with `gh release upload --clobber`.
Replaced assets are deleted — there is no version history.
