library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

profilingServer <- function(id, dbcontext) {
  moduleServer(id, function(input, output, session) {
    
    # Update the dropdown with the union of target and feature choices
    observe({
      # combined_choices <- c(dbcontext$get_target_list(), dbcontext$get_feature_list())
      # updateSelectizeInput(session, "selected_drug_feature", choices = combined_choices, server = TRUE)
      # updateSelectizeInput(session, "selected_drug", choices = dbcontext$get_target_list()  , server = TRUE)
      updateSelectizeInput(session, "selected_drug", choices = dbcontext$get_drug_list(), server = TRUE)
    })
    
    # Helper function: generate a horizontal bar chart (flip coordinate column chart)
    generate_flip_chart <- function(data, title) {

      ## flip the sign
      library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

profilingServer <- function(id, dbcontext) {
  moduleServer(id, function(input, output, session) {
    
    # Update the dropdown with the union of target and feature choices
    observe({
      # combined_choices <- c(dbcontext$get_target_list(), dbcontext$get_feature_list())
      # updateSelectizeInput(session, "selected_drug_feature", choices = combined_choices, server = TRUE)
      # updateSelectizeInput(session, "selected_drug", choices = dbcontext$get_target_list()  , server = TRUE)
      updateSelectizeInput(session, "selected_drug", choices = dbcontext$get_drug_list(), server = TRUE)
    })
    
    # Helper function: generate a horizontal bar chart (flip coordinate column chart)
    generate_flip_chart <- function(data, title) {

      ## flip the sign
      data <- data %>% arrange(RANK_SCORE) %>% head(15)
      data <- data %>% mutate(RANK_SCORE = -1*RANK_SCORE)
      
      p <- ggplot(data, aes(
        x = reorder(CANCER_FEATURE,-rank),  # Independent ordering
        y = RANK_SCORE,
        # group = interaction(DRUG_NAME, FEATURE_NAME),
        text = paste0(
          "Rank: ", rank, "\n",
          "Score: ", RANK_SCORE
        )
      )) +
        geom_col(fill = "indianred", position = position_dodge(width = 0.9)) +
        coord_flip() +
        labs(x = "", y = "Rank", title = "Compound Ranking") +
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
      
      if (length(input$selected_drug) > 0) {
        selected_items <- input$selected_drug
        print(selected_items)
        data <- dbcontext$find_mutation(drugs = selected_items)
        title_prefix <- "Plot for"
      } else {
        selected_items <- NULL
      }
      
      validate(need(nrow(data) > 0, "No data available for this selection."))
      
      data <- data %>% arrange(RANK_SCORE) 
      
      data_ranked <- data %>%
        mutate(rank = rank(RANK_SCORE, ties.method = "average")) %>% # rank descending
        ungroup()
      
      
      # Calculate average rank of each drug across FEATURE_NAME
      # average_rank <- data_ranked %>%
      #   group_by(DRUG_NAME) %>%
      #   summarize(avg_rank = mean(rank, na.rm = TRUE)) %>%
      #   arrange(avg_rank)
      # 
      # average_scores <- data %>%
      #   group_by(DRUG_NAME) %>%
      #   summarize(avg_scores = median(RANK_SCORE, na.rm = TRUE)) %>%
      #   arrange(avg_scores)
      # 
      # data <- inner_join(average_scores,average_rank,by='DRUG_NAME')
      # 
      
      data <- data_ranked
      # write.csv(data_ranked,"ranked_data.csv")
      # write.csv(average_rank,"average_rank.csv")
      
      output$flip_chart <- renderPlotly({
        generate_flip_chart(data, paste("Composite Score Chart for", paste(selected_items, collapse = ", ")))
      })
    })
  })
}

      data <- data %>% mutate(RANK_SCORE = -1*RANK_SCORE)
      
      p <- ggplot(data, aes(
        x = reorder(CANCER_FEATURE,-rank),  # Independent ordering
        y = RANK_SCORE,
        # group = interaction(DRUG_NAME, FEATURE_NAME),
        text = paste0(
          "Rank: ", rank, "\n",
          "Score: ", RANK_SCORE
        )
      )) +
        geom_col(fill = "indianred", position = position_dodge(width = 0.9)) +
        coord_flip() +
        labs(x = "", y = "Rank", title = "Compound Ranking") +
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
      
      if (length(input$selected_drug) > 0) {
        selected_items <- input$selected_drug
        print(selected_items)
        data <- dbcontext$find_mutation(drugs = selected_items)
        title_prefix <- "Plot for"
      } else {
        selected_items <- NULL
      }
      
      validate(need(nrow(data) > 0, "No data available for this selection."))
      
      data <- data %>% arrange(RANK_SCORE) 
      
      data_ranked <- data %>%
        mutate(rank = rank(RANK_SCORE, ties.method = "average")) %>% # rank descending
        ungroup()
      
      
      # Calculate average rank of each drug across FEATURE_NAME
      # average_rank <- data_ranked %>%
      #   group_by(DRUG_NAME) %>%
      #   summarize(avg_rank = mean(rank, na.rm = TRUE)) %>%
      #   arrange(avg_rank)
      # 
      # average_scores <- data %>%
      #   group_by(DRUG_NAME) %>%
      #   summarize(avg_scores = median(RANK_SCORE, na.rm = TRUE)) %>%
      #   arrange(avg_scores)
      # 
      # data <- inner_join(average_scores,average_rank,by='DRUG_NAME')
      # 
      
      data <- data_ranked
      # write.csv(data_ranked,"ranked_data.csv")
      # write.csv(average_rank,"average_rank.csv")
      
      output$flip_chart <- renderPlotly({
        generate_flip_chart(data, paste("Composite Score Chart for", paste(selected_items, collapse = ", ")))
      })
      
      output$composite_table <- renderTable({
        data_ranked
      })
    })
  })
}
