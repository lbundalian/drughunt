library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

server <- function(input, output, session) {
  dbcontext <- DbContext$new()
  
  # Populate the drug, target, and feature dropdowns dynamically
  observe({
    updateSelectizeInput(session, "selected_drug", choices = dbcontext$get_drug_list(), server = TRUE)
    updateSelectizeInput(session, "selected_target", choices = dbcontext$get_target_list(), server = TRUE)
    # updateSelectizeInput(session, "selected_feature", choices = dbcontext$get_feature_list(), server = TRUE)
  })
  
  # When the Plot button is clicked, create a dynamic tab for each selected drug
  observeEvent(input$plot_button, {
    
    if (length(input$selected_drug) > 0){
      selected_drugs <- input$selected_drug
      
      # Dynamically generate a tabsetPanel with one tab per drug
      output$plot_tabs <- renderUI({
        tabs <- lapply(selected_drugs, function(drug) {
          # Generate a safe plot output ID by removing non-alphanumeric characters
          plot_id <- paste0("volcano_plot_", gsub("[^[:alnum:]]", "", drug))
          tabPanel(
            title = drug,
            plotlyOutput(plot_id, height = "600px")
          )
        })
        do.call(tabsetPanel, tabs)
      })
      
      # For each selected drug, render a volcano plot in its corresponding tab
      lapply(selected_drugs, function(drug) {
        plot_id <- paste0("volcano_plot_", gsub("[^[:alnum:]]", "", drug))
        output[[plot_id]] <- renderPlotly({
          # Retrieve the data for the specific drug
          drug_data <- dbcontext$get_drug_data(drug)
          
          # If no data is returned, display a message instead of a plot
          validate(
            need(nrow(drug_data) > 0, "No data available for this drug.")
          )
          
          # Mark the feature with the minimum TISSUE_PVAL for highlighting
          
          
          # Calculate the signed IC50 effect size (assuming FEATURE_DELTA_MEAN_IC50 is non-zero)
          drug_data <- drug_data %>% 
            mutate(SIGNED_IC50_EFFECT_SIZE = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)))
          
          
          drug_data <- drug_data %>% 
            mutate(highlight = TISSUE_PVAL == min(TISSUE_PVAL))
          
          
          
          if (nrow(drug_data)>0){
            
            # Build the ggplot object with custom hover text
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
                legend.position = "none"   # This line removes the legend
              )
            
            ggplotly(p, tooltip = "text")
          } 
        })
      })      
    }
    else if(length((input$selected_target) > 0)){
      selected_targets <- input$selected_target
      
      # Dynamically generate a tabsetPanel with one tab per drug
      output$plot_tabs <- renderUI({
        tabs <- lapply(selected_targets, function(target) {
          # Generate a safe plot output ID by removing non-alphanumeric characters
          plot_id <- paste0("volcano_plot_", gsub("[^[:alnum:]]", "", target))
          tabPanel(
            title = target,
            plotlyOutput(plot_id, height = "600px")
          )
        })
        do.call(tabsetPanel, tabs)
      })
      
      # For each selected drug, render a volcano plot in its corresponding tab
      lapply(selected_targets, function(target) {
        plot_id <- paste0("volcano_plot_", gsub("[^[:alnum:]]", "", target))
        output[[plot_id]] <- renderPlotly({
          # Retrieve the data for the specific drug
          target_data <- dbcontext$get_target_data(target)
          
          # If no data is returned, display a message instead of a plot
          validate(
            need(nrow(target_data) > 0, "No data available for this drug.")
          )
          
          # Mark the feature with the minimum TISSUE_PVAL for highlighting
          
          
          # Calculate the signed IC50 effect size (assuming FEATURE_DELTA_MEAN_IC50 is non-zero)
          target_data <- target_data %>% 
            mutate(SIGNED_IC50_EFFECT_SIZE = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)))
          
          
          target_data <- target_data %>% 
            mutate(highlight = TISSUE_PVAL == min(TISSUE_PVAL))
          
          
          
          if (nrow(target_data)>0){
            # Build the ggplot object with custom hover text
            
            hist(target_data$TISSUE_PVAL)
            p <- ggplot(target_data, 
                        aes(x = IC50_EFFECT_SIZE * (FEATURE_DELTA_MEAN_IC50 / abs(FEATURE_DELTA_MEAN_IC50)),
                            y = -log10(TISSUE_PVAL),
                            size = as.integer(N_FEATURE_POS),
                            text = paste0(
                              "Compound: ", DRUG_NAME, "\n",
                              "Pathway: ", TARGET_PATHWAY, "\n",
                              "IC50 Effect Size: ", round(SIGNED_IC50_EFFECT_SIZE, 2), "\n",
                              "P-Value: ", round(TISSUE_PVAL, 4), "\n",
                              "Sample Size: ", N_FEATURE_POS
                            ))) +
              geom_point(aes(color = highlight, size = as.integer(N_FEATURE_POS) * 5), alpha = 0.5) +
              geom_text(
                data = subset(target_data, highlight == TRUE),
                aes(label = DRUG_NAME),
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
                title = paste("Volcano Plot for", target)
              ) +
              theme(
                axis.title = element_text(size = 14, face = "bold"),
                plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                legend.position = "none"   # This line removes the legend
              )
            
            ggplotly(p, tooltip = "text")
          } 
        })
      })
    }

    

  })
}
