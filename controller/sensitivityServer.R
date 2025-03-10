library(shiny)
library(plotly)
library(ggplot2)
library(gridlayout)
library(tidyverse)



# Sensitivity Module Server
sensitivityServer <- function(id, dbcontext) {
  moduleServer(id, function(input, output, session) {

    # Update dropdowns from database context
    observe({
      updateSelectizeInput(session, "selected_drug", choices = dbcontext$get_drug_list(), server = TRUE)
      updateSelectizeInput(session, "selected_target", choices = dbcontext$get_target_list()  , server = TRUE)
      updateSelectizeInput(session, "selected_feature", choices = dbcontext$get_feature_list(), server = TRUE)
    })
    
    # Helper function to generate volcano plot
    generate_volcano_plot <- function(data, title) {
      data <- data %>%
        mutate(SIGNED_IC50_EFFECT_SIZE = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)),
               highlight = FEATURE_PVAL == min(FEATURE_PVAL),
               color_group = ifelse(SIGNED_IC50_EFFECT_SIZE < 0, "green", "red"))
      
      p <- ggplot(data, 
                  aes(x = SIGNED_IC50_EFFECT_SIZE, 
                      y = -log10(FEATURE_PVAL + 2e-16),
                      size = as.integer(N_FEATURE_POS) * 5,
                      color = color_group,
                      text = paste0(
                        "Compound: ", DRUG_NAME, "\n",
                        "Feature or Target: ", ifelse(grepl("PANCAN", FEATURE_NAME), DRUG_TARGET, FEATURE_NAME), "\n",
                        "Pathway: ", TARGET_PATHWAY, "\n",
                        "IC50 Effect Size: ", round(SIGNED_IC50_EFFECT_SIZE, 2), "\n",
                        "P-Value: ", round(FEATURE_PVAL, 4), "\n",
                        "Sample Size: ", N_FEATURE_POS
                      ))) +
        geom_point(alpha = 0.5) +
        scale_color_manual(values = c("green" = "palegreen3", "red" = "indianred")) +
        geom_text(
          data = subset(data, highlight == TRUE),
          aes(label = ifelse(grepl("PANCAN", FEATURE_NAME), DRUG_TARGET, FEATURE_NAME)),
          vjust = -1,
          check_overlap = TRUE,
          size = 3
        ) +
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
    
    # When the Plot button is clicked, generate the plot(s)
    observeEvent(input$plot_button, {
      if (length(input$selected_drug) > 0) {
        selected_items <- input$selected_drug
        get_data_function <- dbcontext$get_drug_data
        title_prefix <- "Volcano Plot for"
      } else if (length(input$selected_target) > 0) {
        selected_items <- input$selected_target
        get_data_function <- dbcontext$get_target_data
        title_prefix <- "Volcano Plot for"
      } else if (length(input$selected_feature) > 0) {
        selected_items <- input$selected_feature
        get_data_function <- dbcontext$get_feature_data
        title_prefix <- "Volcano Plot for"
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
