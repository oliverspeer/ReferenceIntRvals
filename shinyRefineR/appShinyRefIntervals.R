library(shiny)
library(refineR)
library(tidyverse)
library(shinythemes)
library(memoise)
library(cachem)

# set working directory ----------------------------------------------------
#setwd("C:/R_local/labStat")


# import rds data ---------------------------------------------------------

#combined_data_kc <- readRDS("Combined_Data_KC.rds")

shinyOptions(cache = cache_mem(max_size = 5000e6))

# Define UI
ui <- fluidPage(
  navbarPage(
    title = div(img(src="logo_pos.png",  
                    height = 28, 
                    width = 130, 
                    style = "margin:1px 3px", "  Klinische Chemie ")
    ), 
    theme = shinytheme("paper"), 
    collapsible = TRUE,
    fluid = TRUE,
    
#  tabPanel  ------------------------------------------------
    
    tabPanel("Reference Interval Calculator", "Clinical Chemistry",
  sidebarLayout(
    sidebarPanel(
      selectInput("analyte", "Select Analyte:", choices = unique(Combined_Data_KC$Bezeichnung))
    ),
    mainPanel(
      fluidRow(
        column(6, verbatimTextOutput("refIntervalOutput")),
        column(6, plotOutput("refIntervalPlot"))
      )
      
      
          )
      )
    )
  )
)
# Define server
server <- function(input, output) {
  output$refIntervalOutput <- renderPrint({
    # Filter the dataframe based on the selected analyte
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
    
    # Filter the filteredData to retain unique rows based on "b_Fallnummer"
    uniqueData <- filteredData[!duplicated(filteredData$b_Fallnummer),]
    
    # Extract the values for the reference interval calculation
    values <- uniqueData$Werte
    
    # Calculate the reference interval using refineR
    refInterval <- findRI(Data = values)
    
    # Output the reference interval
    paste0("Reference Interval for", input$analyte, ":")
    print(refInterval)
  })
  
  output$refIntervalPlot <- renderPlot({
    # Filter the dataframe based on the selected analyte
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
    
    # Filter the filteredData to retain unique rows based on "b_Fallnummer"
    uniqueData <- filteredData[!duplicated(filteredData$b_Fallnummer),]
    
    # Extract the values for the reference interval calculation
    values <- uniqueData$Werte
    
    # Calculate the reference interval using refineR
    refInterval <- findRI(Data = values)
    
    # Plot the reference interval
    plot(refInterval)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
