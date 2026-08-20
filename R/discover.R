# curated fallback
FALLBACK_REPOS <- list(
  docs           = c("tidyverse/dplyr", "tidyverse/ggplot2", "r-lib/cli"),
  viz            = c("tidyverse/ggplot2", "wilkelab/ggridges", "thomasp85/patchwork"),
  tidyverse      = c("tidyverse/dplyr", "tidyverse/tidyr", "tidyverse/purrr"),
  shiny          = c("rstudio/shiny", "rstudio/bslib"),
  modelling      = c("tidymodels/parsnip", "tidymodels/recipes", "paul-buerkner/brms"),
  testing        = c("r-lib/testthat", "r-lib/covr", "r-lib/pkgdown"),
  spatial        = c("r-spatial/sf", "rspatial/terra", "rstudio/leaflet"),
  bioinformatics = c("ropensci/taxize", "ropensci/rentrez", "ropensci/rgbif")
)

# maps each skill to the most relevant GitHub topic
SKILL_TOPICS <- list(
  docs           = "r-package",
  viz            = "ggplot2",
  tidyverse      = "tidyverse",
  shiny          = "shiny",
  modelling      = "tidymodels",
  testing        = "testthat",
  spatial        = "spatial",
  bioinformatics = "bioinformatics"
)

discover_repos_for_skill <- function(skill, n = 6) {
  topic <- SKILL_TOPICS[[skill]]

  tryCatch({
    res <- gh(
      "GET /search/repositories",
      q        = paste0("language:R topic:", topic, " stars:>200 is:public"),
      sort     = "updated",
      order    = "desc",
      per_page = n,
      .token   = Sys.getenv("GITHUB_PAT")
    )

    if (length(res$items) == 0) {
      message("No repos found for skill '", skill, "', using fallback")
      return(FALLBACK_REPOS[[skill]])
    }

    # only keep repos that have open issues
    repos <- Filter(function(x) x$open_issues_count > 2, res$items)

    if (length(repos) == 0) {
      message("No repos with open issues for '", skill, "', using fallback")
      return(FALLBACK_REPOS[[skill]])
    }

    sapply(repos, function(x) x$full_name)
  }, error = function(e) {
    message("Repo discovery failed for '", skill, "': ", conditionMessage(e))
    FALLBACK_REPOS[[skill]]
  })
}

discover_repos <- function(skills, n_per_skill = 6) {
  repos <- lapply(skills, function(s) {
    Sys.sleep(0.2)  # stay inside search rate limit
    discover_repos_for_skill(s, n = n_per_skill)
  })

  unique(unlist(repos, use.names = FALSE))
}