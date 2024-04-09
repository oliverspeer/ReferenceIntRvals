library(shiny)
library(refineR)
library(shinycssloaders)
library(memoise)
library(cachem)

# Set up a cache for memoise to use
cache <- cachem::cache_mem(max_size = 1e9) # 1 GB

# Define UI
ui <- fluidPage(
  titlePanel("Reference Interval Calculator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("analyte", "Select Analyte:", choices = unique(Combined_Data_KC$Bezeichnung))
    ),
    mainPanel(
      fluidRow(
        column(4, withSpinner(verbatimTextOutput("refIntervalOutputBoth"))),
        column(4, withSpinner(verbatimTextOutput("refIntervalOutputM"))),
        column(4, withSpinner(verbatimTextOutput("refIntervalOutputF")))
      ),
      fluidRow(
        column(4, withSpinner(plotOutput("refIntervalPlotBoth"))),
        column(4, withSpinner(plotOutput("refIntervalPlotM"))),
        column(4, withSpinner(plotOutput("refIntervalPlotF")))
      )
    )
  )
)

# Define functions to calculate reference intervals
calculateRefInterval <- function(data, analyte, gender) {
  if (gender == "both") {
    filteredData <- data |> subset(Bezeichnung == analyte)
  } else {
    filteredData <- data |> subset(Bezeichnung == analyte & f_Geschl. == gender)
  }
  
  uniqueData <- filteredData |> subset(!duplicated(b_Fallnummer))
  values <- uniqueData$Werte
  findRI(Data = values)
}

# Memoise the function
calculateRefInterval <- memoise(calculateRefInterval, cache = cache)

# Define server
server <- function(input, output) {
  
  refIntervalBoth <- reactive({
    calculateRefInterval(Combined_Data_KC, input$analyte, "both")
  })
  
  refIntervalM <- reactive({
    calculateRefInterval(Combined_Data_KC, input$analyte, "M")
  })
  
  refIntervalF <- reactive({
    calculateRefInterval(Combined_Data_KC, input$analyte, "F")
  })
  
  output$refIntervalOutputBoth <- renderPrint({
    cat(input$analyte, "(all genders)\n -------------------------------------")
    print(refIntervalBoth())
  })
  output$refIntervalPlotBoth <- renderPlot({
    plot(refIntervalBoth())
  })
  
  output$refIntervalOutputM <- renderPrint({
    cat(input$analyte, "(male)\n -------------------------------------")
    print(refIntervalM())
  })
  output$refIntervalPlotM <- renderPlot({
    plot(refIntervalM())
  })
  
  output$refIntervalOutputF <- renderPrint({
    cat(input$analyte, "(female)\n -------------------------------------")
    print(refIntervalF())
  })
  output$refIntervalPlotF <- renderPlot({
    plot(refIntervalF())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
