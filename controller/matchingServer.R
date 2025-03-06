library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

matchingServer <- function(id, dbcontext) {
  moduleServer(id, function(input, output, session) {
    
    # Update the dropdown with the union of target and feature choices
    observe({
      # combined_choices <- c(dbcontext$get_target_list(), dbcontext$get_feature_list())
      # updateSelectizeInput(session, "selected_target_feature", choices = combined_choices, server = TRUE)
      updateSelectizeInput(session, "selected_target", choices = dbcontext$get_target_list()  , server = TRUE)
      updateSelectizeInput(session, "selected_feature", choices = dbcontext$get_feature_list(), server = TRUE)
    })
    
    # Helper function: generate a horizontal bar chart (flip coordinate column chart)
    generate_flip_chart <- function(data, title) {

      
      
      
      # Create a horizontal bar chart: x axis = composite score, y axis = drug name.
      # Create a new ordering variable independent of Drug Name
      data$ORDERED_FACTOR <- factor(seq_len(nrow(data)), levels = order(data$COMPOSITE_SCORE, decreasing = TRUE))
      
      p <- ggplot(data, aes(
        x = reorder(ORDERED_FACTOR,COMPOSITE_SCORE),  # Independent ordering
        y = COMPOSITE_SCORE,
        group = interaction(DRUG_NAME, FEATURE_NAME),
        text = paste0(
          "Drug Name: ", DRUG_NAME, "\n",
          "Feature or Target: ", ifelse(grepl("PANCAN", FEATURE_NAME), DRUG_TARGET, FEATURE_NAME), "\n",
          "Pathway: ", TARGET_PATHWAY, "\n",
          "IC50 Effect Size: ", round(IC50_EFFECT_SIZE, 2), "\n",
          "P-Value: ", round(TISSUE_PVAL, 4), "\n",
          "Sample Size: ", N_FEATURE_POS
        )
      )) +
        geom_col(fill = "indianred", position = position_dodge(width = 0.9)) +
        coord_flip() +
        scale_x_discrete(labels = data$DRUG_NAME) +  # Keep Drug Names in labels
        labs(x = "", y = "Composite Score", title = title) +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 0),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
        )
      
      ggplotly(p)
    }
    
    # When the Plot button is clicked, retrieve and combine data for the selected target/feature(s)
    observeEvent(input$plot_button, {
      
      if (length(input$selected_target) > 0) {
        selected_items <- input$selected_target
        print(selected_items)
        data <- dbcontext$find_best_drug(target = selected_items)
        title_prefix <- "Plot for"
      } else if (length(input$selected_feature) > 0) {
        selected_items <- input$selected_feature
        data <- dbcontext$find_best_drug(feature = selected_items)
        title_prefix <- "Plot for"
      } else {
        selected_items <- NULL
      }
      
      validate(need(nrow(data) > 0, "No data available for this selection."))
      
      # req(input$selected_target_feature)
      # selected_items <- input$selected_target_feature
      # 
      # # For each selected item, choose the appropriate data retrieval function:
      # # If the item is in the target list, use get_target_data;
      # # otherwise, assume it is a feature and use get_feature_data.
      # data <- dbcontext$find_best_drug(selected_items)
      # print(data)
      # validate(need(nrow(data) > 0, "No data available for this selection."))
      # 
      print(data)
      output$flip_chart <- renderPlotly({
        generate_flip_chart(data, paste("Composite Score Chart for", paste(selected_items, collapse = ", ")))
      })
    })
  })
}
