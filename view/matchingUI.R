library(shiny)
library(bslib)
library(plotly)
library(gridlayout)



matchingUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    h3("User Settings"),
    p("This section will allow users to configure preferences for the dashboard.")
  )
}

