# app.R

library(shiny)
library(bslib)
library(shinyWidgets)
library(gh)
library(httr2)
library(jsonlite)

source("R/github.R")
source("R/heuristic.R")
source("R/classify.R")

SKILL_CHOICES <- c(
  "docs", "viz", "tidyverse", "shiny",
  "modelling", "testing", "spatial", "bioinformatics"
)

ui <- page_fluid(
  h2("Find your first R issue."),
  p("Select what you know and we'll find matching open issues."),
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
  actionButton("find_btn", "Find Issues"),
  br(), br(),
  uiOutput("results_ui")
)

server <- function(input, output, session) {

  issues <- reactiveVal(NULL)

  observeEvent(input$find_btn, {
    req(input$skills)

    withProgress(message = "Fetching and scoring issues...", value = 0.1, {
      dat <- fetch_for_skills(input$skills)
      dat <- score_issues(dat)

      setProgress(value = 0.5, message = "Classifying...")
      dat <- classify_issues(dat)

      issues(dat)
    })
  })

  output$results_ui <- renderUI({
    df <- issues()
    if (is.null(df)) return(NULL)

    if (nrow(df) == 0)
      return(p("No issues found. Try different skills."))

    p_count <- p(
      paste(nrow(df), "issues found"),
      class = "text-muted"
    )

    cards <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]

      card(
        card_body(
          tags$small(row$repo, class = "text-muted"),
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
            paste(row$comments, "comments"),
            paste0(" · score: ", row$display_score, "/5")
          )
        )
      )
    })

    tagList(p_count, cards)
  })
}

shinyApp(ui, server)