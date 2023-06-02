library(shiny)
library(refineR)
library(tidyverse)
library(shinythemes)
library(memoise)
library(cachem)


# set working directory ----------------------------------------------------
setwd("C:/R_local/ReferenceIntRvals")


# import rds data ---------------------------------------------------------

#combined_data_kc <- readRDS("Combined_Data_KC.rds")

shinyOptions(cache = cache_mem(max_size = 5000e6))

# Define UI
ui <- fluidPage(navbarPage(
  title = div(
    img(
      src = "logo_pos.png",
      height = 28,
      width = 130,
      style = "margin:1px 3px",
      "  Reference Interval Calculator "
    )
  ),
  theme = shinytheme("paper"),
  collapsible = TRUE,
  fluid = TRUE,
  
  #  tabPanel  ------------------------------------------------
  
  tabPanel(
    "Clinical Chemistry",
    "",
    sidebarLayout(sidebarPanel(
      selectInput(
        "analyte",
        "Select Analyte:",
        choices = unique(Combined_Data_KC$Bezeichnung)
      )
    ),
    mainPanel(fluidRow(
      column(9, verbatimTextOutput("refIntervalOutput"))
    ),
    fluidRow(
      column(9, plotOutput("refIntervalPlot"))
    )))
  )
))


# Define server
server <- function(input, output, session) {
  session$cache <- cache_mem(max_size = 4000e6)
  refInterval <- reactive({
    # Filter the dataframe based on the selected analyte
    filteredData <-
      Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte,]
    
    # Filter the filteredData to retain unique rows based on "b_Fallnummer"
    uniqueData <-
      filteredData[!duplicated(filteredData$b_Fallnummer), ]
    
    # Extract the values for the reference interval calculation
    values <- uniqueData$Werte
    
    # Calculate the reference interval using refineR
    findRI(Data = values)
  })
  
  output$refIntervalOutput <- renderPrint({
    # Output the reference interval
    print(refInterval())
  })
  
  output$refIntervalPlot <- renderPlot({
    # Plot the reference interval
    plot(refInterval())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
