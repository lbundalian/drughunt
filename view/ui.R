library(shiny)
library(bslib)
library(plotly)
library(gridlayout)

# Define UI
ui <- fluidPage(
  titlePanel("Pancancer Drug Sensitivity Dashboard"),
  
  # Add a div around the tabsetPanel with a custom class for styling
  tags$head(
    tags$style(
      HTML("
        .right-aligned-tabs .nav-tabs {
          display: flex;
          justify-content: flex-end;
        }
      ")
    )
  ),
  
  # Apply the custom class to the tabsetPanel
  div(class = "right-aligned-tabs",
      tabsetPanel(
        # First tab: Pancancer Analysis (original UI)
        tabPanel("Sensitivity",
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
                     content = "Sensitivity",
                     alignment = "start",
                     is_title = TRUE
                   ),
                   
                   # Sidebar for Drug Selection
                   grid_card(
                     area = "sidebar",
                     card_header("Settings"),
                     card_body(
                       selectizeInput(
                         inputId = "selected_drug",
                         label = "Select Drug",
                         choices = NULL,
                         multiple = TRUE,
                         options = list(placeholder = "Search or select a drug...", maxItems = 5),
                         width = "100%"
                       ),
                       selectizeInput(
                         inputId = "selected_target",
                         label = "Select Target",
                         choices = NULL,
                         multiple = TRUE,
                         options = list(placeholder = "Search or select a target"),
                         width = "100%"
                       ),
                       selectizeInput(
                         inputId = "selected_feature",
                         label = "Select Cancer Feature",
                         choices = NULL,
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
                   
                   # Plot Output: dynamically generated tabs
                   grid_card(
                     area = "plot",
                     card_header("Volcano Plot"),
                     card_body(
                       uiOutput("plot_tabs")
                     )
                   )
                 )
        ),
        
        # Second tab: Data Summary (Placeholder)
        # Third tab: Settings (Placeholder)
        tabPanel("Settings",
                 fluidPage(
                   h3("User Settings"),
                   p("This section will allow users to configure preferences for the dashboard.")
                 )
        ),
        
        # Third tab: Settings (Placeholder)
        tabPanel("Settings",
                 fluidPage(
                   h3("User Settings"),
                   p("This section will allow users to configure preferences for the dashboard.")
                 )
        )
      )
  )
)
