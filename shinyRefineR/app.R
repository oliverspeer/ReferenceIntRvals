library(shiny)
library(refineR)

# Define UI
ui <- fluidPage(
  titlePanel("Reference Interval Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("analyte", "Select Analyte:", choices = unique(Combined_Data_KC$Bezeichnung))
    ),
    mainPanel(
      verbatimTextOutput("refIntervalOutput")
    )
  )
)

# Define server
server <- function(input, output) {
  output$refIntervalOutput <- renderPrint({
    # Filter the dataframe based on the selected analyte
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
    
    # Extract the values for the reference interval calculation
    values <- filteredData$Werte
    
    # Calculate the reference interval using refineR
    refInterval <- findRI(Data = values)
    
    # Output the reference interval
    paste("Reference Interval for", input$analyte, ":")
    print(refInterval)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
