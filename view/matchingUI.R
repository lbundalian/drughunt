library(shiny)
library(gridlayout)
library(plotly)

matchingUI <- function(id) {
  ns <- NS(id)
  grid_page(
    layout = c(
      "header  header",
      "sidebar plot"
    ),
    row_sizes = c("100px", "1fr"),
    col_sizes = c("300px", "1fr"),
    gap_size = "1rem",
    
    # Header
    grid_card_text(
      area = "header",
      content = "Matching",
      alignment = "start",
      is_title = TRUE
    ),
    
    # Sidebar: one dropdown for Target or Feature selection and a Plot button
    grid_card(
      area = "sidebar",
      card_header("Settings"),
      card_body(
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
    
    # Main panel: Flip coordinate column chart (horizontal bar chart)
    grid_card(
      area = "plot",
      card_header("Composite Score Chart"),
      card_body(
        plotlyOutput(ns("flip_chart"), height = "600px")
      )
    )
  )
}
