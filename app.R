library(shiny)
library(tidyverse)
library(DT)

marshall <- read.csv("ucr_crime_1975_2015.csv") %>% 
  select(Year = year,
         City = department_name,
         Population = total_pop,
         Homicides = homs_sum,
         Rape = rape_sum,
         Robbery = rob_sum,
         Aggravated_Assault = agg_ass_sum,
         Total_Crime = violent_crime,
         Homicides_per_100k = homs_per_100k,
         Rape_per_100k = rape_per_100k,
         Robbery_per_100k = rob_per_100k,
         Aggravated_Assault_per_100k = agg_ass_per_100k,
         Total_Crime_per_100k = violent_per_100k
         )


ui <- fluidPage(
  titlePanel("Marshall Project Dashboard"),
  h2("US Crime Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cityInput", "City",
                  sort(unique(marshall$City)),
                  multiple = TRUE),
      sliderInput("yearInput", "Year",
                  min = 1975, max = 2015, value = c(1975, 2015)),
      h4("What to plot with Year?"),
      selectInput("crimeYearRawInput", label = "Raw Data", 
                  choices = c("Homicides" = "Homicides", 
                              "Rape" = "Rape", 
                              "Robbery" = "Robbery", 
                              "Aggravated Assault" = "Aggravated_Assault", 
                              "Total Crime" = "Total_Crime"),
                  selected = "Homicides"),
      selectInput("crimeYearNormInput", label = "Normalized Data (per 100k population)", 
                  choices = c("Homicides" = "Homicides_per_100k", 
                              "Rape" = "Rape_per_100k", 
                              "Robbery" = "Robbery_per_100k", 
                              "Aggravated Assault" = "Aggravated_Assault_per_100k", 
                              "Total Crime" = "Total_Crime_per_100k"),
                  selected = "Homicides"),
      sliderInput("popInput", "Population",
                  min = 100000, max = 9000000, value = c(100000, 9000000)),
      h4("What to plot with Population?"),
      selectInput("crimePopRawInput", label = "Raw Data", 
                  choices = c("Homicides" = "Homicides", 
                              "Rape" = "Rape", 
                              "Robbery" = "Robbery", 
                              "Aggravated Assault" = "Aggravated_Assault", 
                              "Total Crime" = "Total_Crime"),
                  selected = "Homicides"),
      selectInput("crimePopNormInput", label = "Normalized Data (per 100k population)", 
                  choices = c("Homicides" = "Homicides_per_100k", 
                              "Rape" = "Rape_per_100k", 
                              "Robbery" = "Robbery_per_100k", 
                              "Aggravated Assault" = "Aggravated_Assault_per_100k", 
                              "Total Crime" = "Total_Crime_per_100k"),
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
      )
    )
  ),
  
  dataTableOutput("results")
)


server <- function(input, output) {
  filtered <- reactive({
    if (is.null(input$cityInput)) {
      return(NULL)
    }  
    marshall %>%
      filter(Year >= input$yearInput[1],
             Year <= input$yearInput[2],
             Population >= input$popInput[1],
             Population <= input$popInput[2],
             City %in% input$cityInput
      )
  })
  
  output$rawYearPlot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes_string("Year", input$crimeYearRawInput, group = "City", color = "City")) +
      geom_line() +
      scale_color_discrete(name = "City") +
      labs(x = "Year", y = input$crimeYearRawInput, title = "Raw Crime Count vs. Year") +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  output$normalizedYearPlot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes_string("Year", input$crimeYearNormInput, group = "City", color = "City")) +
      geom_line() +
      scale_color_discrete(name = "City") +
      labs(x = "Year", y = input$crimeYearNormInput, title = "Normalized Crime Count vs. Year") +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  output$rawPopPlot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes_string("Population", input$crimePopRawInput, group = "City", color = "City")) +
      geom_point() +
      scale_color_discrete(name = "City") +
      scale_x_continuous(labels = scales::comma) +
      labs(x = "Population", y = input$crimePopRawInput, title = "Raw Crime Count vs. Population") +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  output$normalizedPopPlot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes_string("Population", input$crimePopNormInput, group = "City", color = "City")) +
      geom_point() +
      scale_color_discrete(name = "City") +
      scale_x_continuous(labels = scales::comma) +
      labs(x = "Population", y = input$crimePopNormInput, title = "Normalized Crime Count vs. Population") +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  })
  
  output$results <- renderDataTable({
    if (is.null(filtered())) {
      return()
    }
    datatable(filtered(), colnames = c("Year" = "Year",
                                       "City" = "City",
                                       "Population" = "Population",
                                       "Homicides" = "Homicides", 
                                       "Rape" = "Rape", 
                                       "Robbery" = "Robbery", 
                                       "Aggravated Assault" = "Aggravated_Assault", 
                                       "Total Crime" = "Total_Crime",
                                       "Homicides per 100k" = "Homicides_per_100k", 
                                       "Rape per 100k" = "Rape_per_100k", 
                                       "Robbery per 100k" = "Robbery_per_100k", 
                                       "Aggravated Assault per 100k" = "Aggravated_Assault_per_100k", 
                                       "Total Crime per 100k" = "Total_Crime_per_100k"
                                       )
              )
  })
}

shinyApp(ui = ui, server = server)