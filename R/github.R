# R/github.R

R_REPOS <- list(
  docs           = c("tidyverse/dplyr", "tidyverse/ggplot2", "r-lib/roxygen2",
                     "r-lib/cli", "tidyverse/tibble", "r-lib/usethis"),
  viz            = c("tidyverse/ggplot2", "wilkelab/ggridges", "thomasp85/patchwork",
                     "rstudio/gt", "tidyverse/ggplot2"),
  tidyverse      = c("tidyverse/dplyr", "tidyverse/tidyr", "tidyverse/purrr",
                     "tidyverse/forcats", "tidyverse/stringr"),
  shiny          = c("rstudio/shiny", "rstudio/bslib",
                     "rstudio/htmltools", "rstudio/shinyWidgets"),
  modelling      = c("tidymodels/parsnip", "tidymodels/recipes", "paul-buerkner/brms",
                     "tidymodels/tune", "tidymodels/yardstick"),
  testing        = c("r-lib/testthat", "r-lib/covr",
                     "r-lib/pkgdown", "r-lib/usethis"),
  spatial        = c("r-spatial/sf", "rspatial/terra",
                     "rstudio/leaflet", "r-spatial/stars"),
  bioinformatics = c("Bioconductor/BiocParallel", "Bioconductor/GenomicRanges",
                     "Bioconductor/SummarizedExperiment")
)

# fetch repo description and topics
fetch_repo_meta <- function(owner, repo) {
  tryCatch({
    info <- gh(
      "GET /repos/{owner}/{repo}",
      owner  = owner,
      repo   = repo,
      .token = Sys.getenv("GITHUB_PAT")
    )

    list(
      description = if (!is.null(info$description)) info$description else "",
      topics      = if (length(info$topics) > 0) paste(info$topics, collapse = ", ") else ""
    )
  }, error = function(e) {
    message("Failed fetching meta for ", owner, "/", repo, ": ", conditionMessage(e))
    list(description = "", topics = "")
  })
}

fetch_repo_issues <- function(owner, repo, n = 10, since_days = 365) {
  tryCatch({
    since <- paste0(Sys.Date() - since_days, "T00:00:00Z")

    raw <- gh(
      "GET /repos/{owner}/{repo}/issues",
      owner    = owner,
      repo     = repo,
      state    = "open",
      per_page = n,
      since    = since,
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

      body_raw  <- x$body
      body_text <- if (is.character(body_raw) && nchar(body_raw) > 0) body_raw else ""

      data.frame(
        repo        = paste0(owner, "/", repo),
        number      = x$number,
        title       = x$title,
        url         = x$html_url,
        labels      = labels,
        comments    = x$comments,
        body        = substr(body_text, 1, 200),
        body_length = nchar(body_text),
        assigned    = !is.null(x$assignee),
        stringsAsFactors = FALSE
      )
    }))
  }, error = function(e) {
    message("Failed fetching ", owner, "/", repo, ": ", conditionMessage(e))
    NULL
  })
}

fetch_for_skills <- function(skills) {
  repos <- unique(unlist(R_REPOS[skills], use.names = FALSE))

  results <- lapply(repos, function(r) {
    parts  <- strsplit(r, "/")[[1]]
    owner  <- parts[1]
    repo   <- parts[2]

    Sys.sleep(0.3)
    meta   <- fetch_repo_meta(owner, repo)   
    issues <- fetch_repo_issues(owner, repo)

    if (is.null(issues)) return(NULL)

    issues$repo_description <- meta$description 
    issues$repo_topics      <- meta$topics        

    issues
  })

  combined <- do.call(rbind, Filter(Negate(is.null), results))
  if (is.null(combined)) return(data.frame())

  combined <- combined[!duplicated(paste(combined$repo, combined$number)), ]

  rownames(combined) <- NULL
  combined
}
