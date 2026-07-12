# app.R

library(shiny)
library(bslib)
library(shinyWidgets)

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

  output$results_ui <- renderUI({
    p("Select skills above and click Find Issues.")
  })

  observeEvent(input$find_btn, {
    req(input$skills)
    # placeholder
    message("Skills selected: ", paste(input$skills, collapse = ", "))
    showNotification(
      paste0("Selected: ", paste(input$skills, collapse = ", ")),
      type     = "message",
      duration = 3
    )
  })
}

shinyApp(ui, server)