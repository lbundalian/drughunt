library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

overviewServer <- function(id, dbcontext) {
  moduleServer(id, function(input, output, session) {
    
    # Update the first dropdown with drug choices and the second with a union of targets and features
    observe({
      updateSelectizeInput(session, "selected_drug", choices = dbcontext$get_drug_list(), server = TRUE)
      combined_choices <- c(dbcontext$get_target_list(), dbcontext$get_feature_list())
      updateSelectizeInput(session, "selected_target_feature", choices = combined_choices, server = TRUE)
    })
    
    # Helper function to generate the volcano plot.
    # Points are gray by default, and if any item is selected in the combined dropdown,
    # points with a matching "target_match" are highlighted in blue.
    generate_volcano_plot <- function(data, title) {
      data <- data %>%
        mutate(
          SIGNED_IC50_EFFECT_SIZE = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)),
          target_match = ifelse(grepl("PANCAN", FEATURE_NAME), DRUG_TARGET, FEATURE_NAME)
        )
      
      if (length(input$selected_target_feature) > 0) {
        data <- data %>%
          mutate(color_group = ifelse(target_match %in% input$selected_target_feature, "sienna2", "snow2"))
      } else {
        data <- data %>% mutate(color_group = "snow2")
      }
      
      p <- ggplot(data, 
                  aes(x = SIGNED_IC50_EFFECT_SIZE, 
                      y = -log10(TISSUE_PVAL),
                      size = as.integer(N_FEATURE_POS) * 5,
                      color = color_group,
                      text = paste0(
                        "Compound: ", DRUG_NAME, "\n",
                        "Feature or Target: ", target_match, "\n",
                        "Pathway: ", TARGET_PATHWAY, "\n",
                        "IC50 Effect Size: ", round(SIGNED_IC50_EFFECT_SIZE, 2), "\n",
                        "P-Value: ", round(TISSUE_PVAL, 4), "\n",
                        "Sample Size: ", N_FEATURE_POS
                      ))) +
        geom_point(alpha = 0.5) +
        scale_color_identity() +
        # geom_text(
        #   data = subset(data, target_match %in% input$selected_target_feature),
        #   aes(label = target_match),
        #   vjust = -1,
        #   check_overlap = TRUE,
        #   size = 3
        # ) +
        theme_minimal() +
        geom_vline(xintercept = 0, linetype = "dotdash") +
        scale_x_continuous(limits = c(-2, 2)) +
        labs(
          x = "IC50 Effect",
          y = "log10(p-value)",
          title = title
        ) +
        theme(
          axis.title = element_text(size = 14, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none"
        )
      
      ggplotly(p, tooltip = "text")
    }
    
    # Determine which selection(s) to use for data retrieval:
    # If any drugs are selected, use them.
    # Otherwise, if target/feature selections are made, decide which data to fetch based on the selection.
    observeEvent(input$plot_button, {
      if (length(input$selected_drug) > 0) {
        selected_items <- input$selected_drug
        get_data_function <- dbcontext$get_drug_data
        title_prefix <- "Volcano Plot for"
      } else if (length(input$selected_target_feature) > 0) {
        selected_items <- input$selected_target_feature
        title_prefix <- "Volcano Plot for"
        get_data_function <- function(item) {
          if (item %in% dbcontext$get_target_list()) {
            dbcontext$get_target_data(item)
          } else {
            dbcontext$get_feature_data(item)
          }
        }
      } else {
        selected_items <- NULL
      }
      
      if (!is.null(selected_items)) {
        # Create a tab for each selected item
        output$plot_tabs <- renderUI({
          tabs <- lapply(selected_items, function(item) {
            plot_id <- paste0("volcano_plot_", gsub("[^[:alnum:]]", "", item))
            tabPanel(title = item, plotlyOutput(session$ns(plot_id), height = "600px"))
          })
          do.call(tabsetPanel, tabs)
        })
        
        # Render the volcano plot in each tab
        lapply(selected_items, function(item) {
          plot_id <- paste0("volcano_plot_", gsub("[^[:alnum:]]", "", item))
          output[[plot_id]] <- renderPlotly({
            data <- get_data_function(item)
            validate(need(nrow(data) > 0, "No data available for this selection."))
            generate_volcano_plot(data, paste(title_prefix, item))
          })
        })
      }
    })
  })
}
