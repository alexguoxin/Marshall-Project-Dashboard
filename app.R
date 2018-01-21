library(shiny)
library(tidyverse)

marshall <- read.csv("ucr_crime_1975_2015.csv")

ui <- fluidPage(
  titlePanel("Marshall Project Dashboard"),
  h2("US Crime Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cityInput", "City",
                  sort(unique(marshall$department_name)),
                  multiple = TRUE),
      sliderInput("yearInput", "Year",
                  min = 1975, max = 2015, value = c(1975, 2015)),
      h4("What to plot with Year?"),
      selectInput("crimeYearRawInput", label = "Raw Data", 
                  choices = c("Homicides" = "homs_sum", 
                              "Rape" = "rape_sum", 
                              "Robbery" = "rob_sum", 
                              "Aggravated Assault" = "agg_ass_sum", 
                              "Sum of All" = "violent_crime"),
                  selected = "Homicides"),
      selectInput("crimeYearNormInput", label = "Normalized Data (per 100k population)", 
                  choices = c("Homicides" = "homs_per_100k", 
                              "Rape" = "rape_per_100k", 
                              "Robbery" = "rob_per_100k", 
                              "Aggravated Assault" = "agg_ass_per_100k", 
                              "Sum of All" = "violent_per_100k"),
                  selected = "Homicides"),
      sliderInput("popInput", "Population",
                  min = 100000, max = 9000000, value = c(100000, 9000000)),
      h4("What to plot with Population?"),
      selectInput("crimePopRawInput", label = "Raw Data", 
                  choices = c("Homicides" = "homs_sum", 
                              "Rape" = "rape_sum", 
                              "Robbery" = "rob_sum", 
                              "Aggravated Assault" = "agg_ass_sum", 
                              "Sum of All" = "violent_crime"),
                  selected = "Homicides"),
      selectInput("crimePopNormInput", label = "Normalized Data (per 100k population)", 
                  choices = c("Homicides" = "homs_per_100k", 
                              "Rape" = "rape_per_100k", 
                              "Robbery" = "rob_per_100k", 
                              "Aggravated Assault" = "agg_ass_per_100k", 
                              "Sum of All" = "violent_per_100k"),
                  selected = "Homicides")
    ),
    
    mainPanel(
      fluidRow(
        column(6,
               plotOutput("rawYearPlot")
        ),
        
        column(6, 
               plotOutput("normalizedYearPlot")
        )
      ),
      
      fluidRow(
        column(6,
               plotOutput("rawPopPlot")
        ),
        
        column(6, 
               plotOutput("normalizedPopPlot")
        )
      ),
      
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    if (is.null(input$cityInput)) {
      return(NULL)
    }    
    
    marshall %>%
      filter(year >= input$yearInput[1],
             year <= input$yearInput[2],
             total_pop >= input$popInput[1],
             total_pop <= input$popInput[2],
             department_name %in% input$cityInput
      )
  })
  
  output$rawYearPlot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes_string("year", input$crimeYearRawInput, group = "department_name", color = "department_name")) +
      geom_line() +
      scale_color_discrete(name = "City") +
      labs(x = "Year", y = "Number of Crime Reported", title = "Raw Crime Count vs. Year") +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  output$normalizedYearPlot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes_string("year", input$crimeYearNormInput, group = "department_name", color = "department_name")) +
      geom_line() +
      scale_color_discrete(name = "City") +
      labs(x = "Year", y = "Number of Crime Reported per 100k", title = "Normalized Crime Count vs. Year") +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  output$rawPopPlot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes_string("total_pop", input$crimePopRawInput, group = "department_name", color = "department_name")) +
      geom_point() +
      scale_color_discrete(name = "City") +
      labs(x = "Population", y = "Number of Crime Reported", title = "Raw Crime Count vs. Population") +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  output$normalizedPopPlot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    ggplot(filtered(), aes_string("total_pop", input$crimePopNormInput, group = "department_name", color = "department_name")) +
      geom_point() +
      scale_color_discrete(name = "City") +
      labs(x = "Population", y = "Number of Crime Reported per 100k", title = "Normalized Crime Count vs. Population") +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)