# Hardcoded repo list per skill (for now)
R_REPOS <- list(
  docs           = c("tidyverse/dplyr", "tidyverse/ggplot2", "r-lib/roxygen2"),
  viz            = c("tidyverse/ggplot2", "wilkelab/ggridges", "thomasp85/patchwork"),
  tidyverse      = c("tidyverse/dplyr", "tidyverse/tidyr", "tidyverse/purrr"),
  shiny          = c("rstudio/shiny", "rstudio/bslib"),
  modelling      = c("tidymodels/parsnip", "tidymodels/recipes", "paul-buerkner/brms"),
  testing        = c("r-lib/testthat", "r-lib/covr"),
  spatial        = c("r-spatial/sf", "rspatial/terra"),
  bioinformatics = c("Bioconductor/BiocParallel", "Bioconductor/GenomicRanges")
)

fetch_repo_issues <- function(owner, repo, n = 10) {
  tryCatch({
    raw <- gh(
      "GET /repos/{owner}/{repo}/issues",
      owner    = owner,
      repo     = repo,
      state    = "open",
      per_page = n,
      .token   = Sys.getenv("GITHUB_PAT")
    )

    if (length(raw) == 0) return(NULL)

    issues_only <- Filter(function(x) is.null(x$pull_request), raw)
    if (length(issues_only) == 0) return(NULL)

    do.call(rbind, lapply(issues_only, function(x) {
      labels <- if (length(x$labels) > 0) {
        paste(sapply(x$labels, `[[`, "name"), collapse = ", ")
      } else {
        ""
      }

      body_preview <- if (!is.null(x$body) && nchar(x$body) > 0) {
        substr(x$body, 1, 200)
      } else {
        ""
      }

      data.frame(
        repo     = paste0(owner, "/", repo),
        number   = x$number,
        title    = x$title,
        url      = x$html_url,
        labels   = labels,
        comments = x$comments,
        body     = body_preview,
        stringsAsFactors = FALSE
      )
    }))
  }, error = function(e) {
    message("Failed fetching ", owner, "/", repo, ": ", conditionMessage(e))
    NULL
  })
}

# TODO: body column sometimes comes back as a list instead of character
fetch_for_skills <- function(skills) {
  repos <- unique(unlist(R_REPOS[skills], use.names = FALSE))

  results <- lapply(repos, function(r) {
    parts <- strsplit(r, "/")[[1]]
    Sys.sleep(0.3)  # avoid hammering the API
    fetch_repo_issues(parts[1], parts[2])
  })

  combined <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(combined)) return(data.frame())
  rownames(combined) <- NULL
  combined
}
