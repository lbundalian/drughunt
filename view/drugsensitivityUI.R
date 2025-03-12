library(shiny)
library(bslib)
library(plotly)
library(gridlayout)


# Sensitivity Module UI
drugsensitivityUI <- function(id) {
  ns <- NS(id)
  grid_page(
    layout = c(
      "header  header  ",
      "sidebar plot "
    ),
    row_sizes = c("100px", "1fr"),
    col_sizes = c("500px", "1fr"),
    gap_size = "1rem",
    
    # Header
    grid_card_text(
      area = "header",
      content = "Drug Sensitivity",
      alignment = "start",
      is_title = TRUE
    ),
    
    # Sidebar for selections
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
        # selectizeInput(
        #   inputId = ns("selected_target"),
        #   label = "Select Target",
        #   choices = NULL,
        #   multiple = TRUE,
        #   options = list(placeholder = "Search or select a target"),
        #   width = "100%"
        # ),
        # selectizeInput(
        #   inputId = ns("selected_feature"),
        #   label = "Select Cancer Feature",
        #   choices = NULL,
        #   multiple = TRUE,
        #   options = list(placeholder = "Search or select a feature"),
        #   width = "100%"
        # ),
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