library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

server <- function(input, output, session) {
  dbcontext <- DbContext$new()
  
  # Populate the drug and target dropdowns dynamically
  observe({
    updateSelectizeInput(session, "selected_drug", choices = dbcontext$get_drug_list(), server = TRUE)
    updateSelectizeInput(session, "selected_target", choices = dbcontext$get_target_list(), server = TRUE)
    # updateSelectizeInput(session, "selected_feature", choices = dbcontext$get_feature_list(), server = TRUE)
  })
  
  # When the Plot button is clicked, create a dynamic tab for each selected item
  observeEvent(input$plot_button, {
    
    # When drugs are selected, show volcano plots (as before)
    if (length(input$selected_drug) > 0) {
      selected_drugs <- input$selected_drug
      
      # Create a tabset panel with one tab per drug
      output$plot_tabs <- renderUI({
        tabs <- lapply(selected_drugs, function(drug) {
          plot_id <- paste0("volcano_plot_", gsub("[^[:alnum:]]", "", drug))
          tabPanel(
            title = drug,
            plotlyOutput(plot_id, height = "600px")
          )
        })
        do.call(tabsetPanel, tabs)
      })
      
      lapply(selected_drugs, function(drug) {
        plot_id <- paste0("volcano_plot_", gsub("[^[:alnum:]]", "", drug))
        output[[plot_id]] <- renderPlotly({
          drug_data <- dbcontext$get_drug_data(drug)
          validate(
            need(nrow(drug_data) > 0, "No data available for this drug.")
          )
          
          drug_data <- drug_data %>% 
            mutate(SIGNED_IC50_EFFECT_SIZE = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50))) %>%
            mutate(highlight = TISSUE_PVAL == min(TISSUE_PVAL))
          
          p <- ggplot(drug_data, 
                      aes(x = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)),
                          y = -log10(TISSUE_PVAL),
                          size = as.integer(N_FEATURE_POS) * 5,
                          text = paste0(
                            "Feature: ", ifelse(grepl("PANCAN", FEATURE_NAME), DRUG_TARGET, FEATURE_NAME), "\n",
                            "Pathway: ", TARGET_PATHWAY, "\n",
                            "IC50 Effect Size: ", round(SIGNED_IC50_EFFECT_SIZE, 2), "\n",
                            "P-Value: ", round(TISSUE_PVAL, 4), "\n",
                            "Sample Size: ", N_FEATURE_POS
                          ))) +
            geom_point(aes(color = highlight, size = as.integer(N_FEATURE_POS) * 5), alpha = 0.5) +
            geom_text(
              data = subset(drug_data, highlight == TRUE),
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
              title = paste("Volcano Plot for", drug)
            ) +
            theme(
              axis.title = element_text(size = 14, face = "bold"),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              legend.position = "none"
            )
          
          ggplotly(p, tooltip = "text")
        })
      })
      
      # When targets are selected, show a violin plot and table for each target  
    } else if (length(input$selected_target) > 0) {
      selected_targets <- input$selected_target
      
      # Create a tabset panel with one tab per target; each tab contains a violin plot and a table
      output$plot_tabs <- renderUI({
        tabs <- lapply(selected_targets, function(target) {
          plot_id <- paste0("violin_plot_", gsub("[^[:alnum:]]", "", target))
          table_id <- paste0("target_table_", gsub("[^[:alnum:]]", "", target))
          tabPanel(
            title = target,
            plotlyOutput(plot_id, height = "400px"),
            br(),
            tableOutput(table_id)
          )
        })
        do.call(tabsetPanel, tabs)
      })
      
      lapply(selected_targets, function(target) {
        plot_id <- paste0("violin_plot_", gsub("[^[:alnum:]]", "", target))
        table_id <- paste0("target_table_", gsub("[^[:alnum:]]", "", target))
        
        output[[plot_id]] <- renderPlotly({
          target_data <- dbcontext$get_target_data(target)
          validate(
            need(nrow(target_data) > 0, "No data available for this target.")
          )
          
          target_data <- target_data %>% 
            mutate(SIGNED_IC50_EFFECT_SIZE = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50))) %>%
            mutate(highlight = TISSUE_PVAL == min(TISSUE_PVAL))
          
          # For this example, we create a violin plot showing the distribution of IC50_EFFECT_SIZE for each drug
          p <- ggplot(target_data, aes(x = DRUG_NAME, y = IC50_EFFECT_SIZE)) +
            geom_violin(trim = FALSE, fill = "lightblue") +
            geom_jitter(width = 0.2, alpha = 0.5) +
            theme_minimal() +
            labs(
              title = paste("Violin Plot for", target),
              x = "Drug Name",
              y = "IC50 Effect Size"
            )
          
          ggplotly(p)
        })
        
        output[[table_id]] <- renderTable({
          target_data <- dbcontext$get_target_data(target)
          validate(
            need(nrow(target_data) > 0, "No data available for this target.")
          )
          target_data
        })
      })
    }
  })
}
