library(shiny)
library(gridlayout)
library(plotly)

overviewUI <- function(id) {
  ns <- NS(id)
  grid_page(
    layout = c(
      "header  header",
      "sidebar plot"
    ),
    row_sizes = c("100px", "1fr"),
    col_sizes = c("500px", "1fr"),
    gap_size = "1rem",
    
    # Header for the Overview tab
    grid_card_text(
      area = "header",
      content = "Overview",
      alignment = "start",
      is_title = TRUE
    ),
    
    # Sidebar with two selection inputs:
    # - Select Drug (for data retrieval)
    # - Select Target or Feature (for highlighting)
    grid_card(
      area = "sidebar",
      card_header("Settings"),
      card_body(
        selectizeInput(
          inputId = ns("selected_drug"),
          label = "Select Drug",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Search or select a drug...", maxItems = 5),
          width = "100%"
        ),
        selectizeInput(
          inputId = ns("selected_target_feature"),
          label = "Select Target or Feature",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Search or select a target or feature"),
          width = "100%"
        ),
        actionButton(
          inputId = ns("plot_button"),
          label = "Plot",
          width = "100%"
        )
      )
    ),
    
    # Plot output: dynamically generated tabs for each selection
    grid_card(
      area = "plot",
      card_header("Volcano Plot"),
      card_body(
        uiOutput(ns("plot_tabs"))
      )
    )
  )
}
