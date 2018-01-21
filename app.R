library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Marshall Project Dashboard"),
  h2("US Crime Data"),
  
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    mainPanel(
      
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)