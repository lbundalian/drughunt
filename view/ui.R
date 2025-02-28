library(shiny)
library(bslib)
library(plotly)
library(gridlayout)

ui <- grid_page(
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
    content = "Pancancer Drug Sensitivity Analysis",
    alignment = "start",
    is_title = TRUE
  ),
  
  # Sidebar for Drug Selection and other settings
  grid_card(
    area = "sidebar",
    card_header("Settings"),
    card_body(
      selectizeInput(
        inputId = "selected_drug",
        label = "Select Drug",
        choices = NULL,  # Update dynamically as needed
        multiple = TRUE,
        options = list(placeholder = "Search or select a drug...",maxItems = 5),
        width = "100%"
      ),
      selectizeInput(
        inputId = "selected_target",
        label = "Select Target",
        choices = NULL,  # Update dynamically as needed
        multiple = TRUE,
        options = list(placeholder = "Search or select a target"),
        width = "100%"
      ),
      selectizeInput(
        inputId = "selected_feature",
        label = "Select Cancer Feature",
        choices = NULL,  # Update dynamically as needed
        multiple = TRUE,
        options = list(placeholder = "Search or select a feature"),
        width = "100%"
      ),
      actionButton(
        inputId = "plot_button",
        label = "Plot",
        width = "100%"
      )
    )
  ),
  
  # Plot Output: dynamically generated tabsetPanel for each drug
  grid_card(
    area = "plot",
    card_header("Volcano Plot"),
    card_body(
      uiOutput("plot_tabs")
    )
  )
)