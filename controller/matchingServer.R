library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

matchingServer <- function(id, dbcontext) {
  moduleServer(id, function(input, output, session) {
    
    # Update the dropdown with the union of target and feature choices
    observe({
      combined_choices <- c(dbcontext$get_target_list(), dbcontext$get_feature_list())
      updateSelectizeInput(session, "selected_target_feature", choices = combined_choices, server = TRUE)
    })
    
    # Helper function: generate a horizontal bar chart (flip coordinate column chart)
    generate_flip_chart <- function(data, title) {

      
      data <-  data %>% mutate(DRUG_NAME = reorder(DRUG_NAME, COMPOSITE_SCORE))
      
      # Create a horizontal bar chart: x axis = composite score, y axis = drug name.
      p <- ggplot(data, aes(x = DRUG_NAME, y = COMPOSITE_SCORE)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(x = "Drug Name", y = "Composite Score", title = title) +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
        )
      
      ggplotly(p)
    }
    
    # When the Plot button is clicked, retrieve and combine data for the selected target/feature(s)
    observeEvent(input$plot_button, {
      req(input$selected_target_feature)
      selected_items <- input$selected_target_feature
      
      # For each selected item, choose the appropriate data retrieval function:
      # If the item is in the target list, use get_target_data;
      # otherwise, assume it is a feature and use get_feature_data.
      data <- dbcontext$find_best_drug(selected_items)
      print(data)
      validate(need(nrow(data) > 0, "No data available for this selection."))
      
      output$flip_chart <- renderPlotly({
        generate_flip_chart(data, paste("Composite Score Chart for", paste(selected_items, collapse = ", ")))
      })
    })
  })
}
