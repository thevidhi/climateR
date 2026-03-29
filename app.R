library(shiny)
library(dplyr)
library(ggplot2)

# Load your functions
source("R_scripts/fetch_data.R")
source("R_scripts/clean_data.R")
source("R_scripts/analysis.R")
source("R_scripts/visualize.R")

# UI
ui <- fluidPage(
  titlePanel("🌍 Climate Data Analysis Toolkit"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Select City:",
                  choices = c("Rajkot", "Delhi", "Mumbai")),
      
      dateRangeInput("dates",
                     "Select Date Range:",
                     start = "2023-01-01",
                     end = "2023-03-01"),
      
      actionButton("go", "Analyze")
    ),
    
    mainPanel(
      plotOutput("climatePlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  get_coords <- function(city) {
    if (city == "Rajkot") return(c(22.3, 70.8))
    if (city == "Delhi") return(c(28.6, 77.2))
    if (city == "Mumbai") return(c(19.0, 72.8))
  }
  
  data_reactive <- eventReactive(input$go, {
    
    coords <- get_coords(input$city)
    
    df <- fetch_climate_data(
      coords[1],
      coords[2],
      input$dates[1],
      input$dates[2]
    )
    
    df <- clean_climate_data(df)
    df <- detect_anomalies(df)
    
    return(df)
  })
  
  output$climatePlot <- renderPlot({
    df <- data_reactive()
    
    ggplot(df, aes(x = date)) +
      geom_line(aes(y = temp_avg), color = "blue") +
      
      geom_bar(aes(y = rainfall),
               stat = "identity",
               fill = "skyblue",
               alpha = 0.4) +
      
      geom_point(
        data = subset(df, temp_anomaly == TRUE),
        aes(y = temp_avg),
        color = "red",
        size = 2
      ) +
      
      labs(
        title = paste("Climate Data for", input$city),
        x = "Date",
        y = "Value"
      ) +
      theme_minimal()
  })
}

# Run App
shinyApp(ui = ui, server = server)