# app.R

library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(gh)
library(httr2)
library(jsonlite)

source("R/github.R")
source("R/heuristic.R")
source("R/classify.R")
source("R/theme.R")

SKILL_CHOICES <- c(
  "docs", "viz", "tidyverse", "shiny",
  "modelling", "testing", "spatial", "bioinformatics"
)

score_dots <- function(n, max_n = 5) {
  dots <- lapply(seq_len(max_n), function(i) {
    tags$span(class = if (!is.na(n) && i <= n) "dot dot-filled" else "dot dot-empty")
  })
  tags$span(class = "score-dots", dots)
}

ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css")
  ),
  useShinyjs(),

  # nav
  tags$nav(
    class = "navbar bg-white border-bottom px-4 mb-4",
    tags$span(class = "navbar-brand mb-0", "fiRstissue"),
    tags$span("for R", class = "text-muted small")
  ),

  div(
    class = "container",
    style = "max-width: 860px;",

    h2("Find your first R issue.", class = "mb-1"),
    p("Select what you know and we'll find matching open issues.", class = "text-muted"),
    br(),
    checkboxGroupButtons(
      inputId  = "skills",
      label    = "What do you know?",
      choices  = SKILL_CHOICES,
      selected = NULL,
      status   = "outline-primary",
      size     = "sm"
    ),
    br(),

    div(
      class = "filter-row d-flex align-items-center gap-4",
      div(
        style = "width: 260px;",
        sliderInput(
          inputId = "min_score",
          label   = "Minimum score",
          min     = 1,
          max     = 5,
          value   = 2,
          step    = 1,
          ticks   = TRUE,
          width   = "100%"
        )
      ),
      div(
        class = "mt-2",
        checkboxInput(
          inputId = "labelled_only",
          label   = "Labelled issues only",
          value   = FALSE
        )
      )
    ),

    actionButton("find_btn", "Find Issues", class = "btn-primary"),
    br(), br(),
    uiOutput("results_ui")
  )
)

server <- function(input, output, session) {

  issues <- reactiveVal(NULL)

  observeEvent(input$find_btn, {
    req(input$skills)

    shinyjs::disable("find_btn")               
    on.exit(shinyjs::enable("find_btn"))       

    withProgress(message = "Fetching and scoring issues...", value = 0.1, {
      dat <- fetch_for_skills(input$skills)
      dat <- score_issues(dat)

      setProgress(value = 0.5, message = "Classifying... (0/?)")

      dat <- classify_issues(
        dat,
        skills      = input$skills,
        progress_cb = function(i, n) {
          setProgress(
            value   = 0.5 + 0.45 * (i / n),
            message = paste0("Classifying... (", i, "/", n, ")")
          )
        }
      )

      issues(dat)
    })
  })

  output$results_ui <- renderUI({
    df <- issues()
    if (is.null(df)) return(NULL)

    if (nrow(df) == 0)
      return(div(class = "empty-state", p("No issues found. Try different skills.")))

    df <- df[df$display_score >= input$min_score, ]

    if (input$labelled_only) {
      df <- df[nchar(df$labels) > 0, ]
    }

    if (nrow(df) == 0)
      return(div(class = "empty-state", p("No issues match the current filters.")))

    p_count <- p(
      paste(nrow(df), "issues found"),
      class = "text-muted mb-3"
    )

    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]

      div(
        class = "issue-card",
        card(
          card_body(
            div(
              class = "d-flex justify-content-between align-items-start",
              tags$small(row$repo, class = "text-muted"),
              score_dots(row$display_score)
            ),
            p(
              tags$a(row$title, href = row$url, target = "_blank"),
              class = "mb-1 mt-1"
            ),
            if (nchar(row$body) > 0)
              tags$small(row$body, class = "text-muted d-block mb-1"),
            if (nchar(row$reason) > 0)
              tags$small(tags$em(row$reason), class = "text-muted d-block mb-2"),
            tags$small(
              if (nchar(row$labels) > 0) paste0("labels: ", row$labels, " · "),
              paste(row$comments, "comments")
            )
          )
        )
      )
    })

    tagList(p_count, cards)
  })
}

shinyApp(ui, server)