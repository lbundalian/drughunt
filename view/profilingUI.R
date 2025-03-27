library(shiny)
library(gridlayout)
library(plotly)

profilingUI <- function(id) {
  ns <- NS(id)
  grid_page(
    # The layout now has 3 rows and 2 columns:
    #   Row 1: header across both columns
    #   Row 2: sidebar (col 1), plot (col 2)
    #   Row 3: sidebar (col 1), table (col 2)
    layout = c(
      "header  header",
      "sidebar plot",
      "sidebar table"
    ),
    row_sizes = c("100px", "1fr", "1fr"),
    col_sizes = c("300px", "1fr"),
    gap_size = "1rem",
    
    # Header
    grid_card_text(
      area = "header",
      content = "Composite Compound Ranking",
      alignment = "start",
      is_title = TRUE
    ),
    
    # Sidebar (spans rows 2 and 3 in column 1)
    grid_card(
      area = "sidebar",
      card_header("Settings"),
      card_body(
        selectizeInput(
          inputId = ns("selected_drug"),
          label = "Select Compound",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Search or select a compound"),
          width = "100%"
        ),
        
        actionButton(
          inputId = ns("plot_button"),
          label = "Plot",
          width = "100%"
        )
      )
    ),
    
    # Plot area (top-right)
    grid_card(
      area = "plot",
      card_header("Composite Score Chart"),
      card_body(
        plotlyOutput(ns("flip_chart"), height = "600px")
      )
    ),
    
    # Table area (bottom-right)
    grid_card(
      area = "table",
      card_header("Composite Table"),
      card_body(
        tableOutput(ns("composite_table"))
      )
    )
  )
}
