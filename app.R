# 
# source("global.R")
# source("view/ui.R")
# source("controller/server.R")
# 
# shinyApp(ui = ui, server = server)
# 
# 
# 


library(shiny)
library(bslib)
library(plotly)
library(gridlayout)

# Source the module UI and server files
source("view/drugsensitivityUI.R")
source("controller/drugsensitivityServer.R")
source("view/targetsensitivityUI.R")
source("controller/targetsensitivityServer.R")
source("view/featuresensitivityUI.R")
source("controller/featuresensitivityServer.R")

# source("view/overviewUI.R")
# source("controller/overviewServer.R")

source("view/matchingUI.R")
source("controller/matchingServer.R")

ui <- fluidPage(
  titlePanel("Pancancer Drug Sensitivity Dashboard"),
  
  # Custom style to right-align tabs
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
  
  div(class = "right-aligned-tabs",
      tabsetPanel(
        # tabPanel("Sensitivity", sensitivityUI("sensitivity")),
        # tabPanel("Overview", overviewUI("overview")),
        tabPanel("Drug/Compound", drugsensitivityUI("drugsensitivity")),
        tabPanel("Drug Target", targetsensitivityUI("targetsensitivity")),
        tabPanel("Cancer Feature", featuresensitivityUI("featuresensitivity")),
        tabPanel("Compound Ranking", matchingUI("matching"))
      )
  )
) 

server <- function(input, output, session) {
  
  dbcontext <- DbContext$new()
  
  # Call the modules’ server functions
  drugsensitivityServer("drugsensitivity", dbcontext)
  targetsensitivityServer("targetsensitivity", dbcontext)
  featuresensitivityServer("featuresensitivity", dbcontext)
  # sensitivityServer("sensitivity", dbcontext)
  # overviewServer("overview", dbcontext)
  matchingServer("matching",dbcontext)
}

shinyApp(ui, server)


