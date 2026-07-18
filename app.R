# app.R

library(shiny)
library(bslib)
library(shinyWidgets)
library(gh)

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

    withProgress(message = "Fetching issues from GitHub...", {
      dat <- fetch_for_skills(input$skills)
      issues(dat)
    })
  })

  output$results_ui <- renderUI({
    df <- issues()
    if (is.null(df)) return(NULL)
    if (nrow(df) == 0) return(p("No issues found. Try different skills."))
    # rough table for now
    tableOutput("issues_table")
  })

  output$issues_table <- renderTable({
    df <- issues()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df[, c("repo", "number", "title", "comments")]
  })
}

shinyApp(ui, server)