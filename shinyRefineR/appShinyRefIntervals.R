library(shiny)
library(refineR)
library(tidyverse)
library(shinythemes)
library(shinycssloaders)
library(memoise)
library(cachem)
library(vroom)


# set working directory ----------------------------------------------------
setwd("C:/R_local/ReferenceIntRvals")


# import rds data ---------------------------------------------------------

#Combined_Data_KC <- readRDS("C:/R_local/labStat/Combined_Data_KC.rds")

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
      ),
      textOutput("dateRangeOutput"),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      fluidRow(
        column(4, verbatimTextOutput("UnitOutput")),
        column(4, verbatimTextOutput("URIOutputM")),
        column(4, verbatimTextOutput("URIOutputF"))
      ),
     fluidRow(
        column(4, verbatimTextOutput("refIntervalOutputBoth")),
        column(4, verbatimTextOutput("refIntervalOutputM")),
        column(4, verbatimTextOutput("refIntervalOutputF"))
      ),
      fluidRow(
        column(4, withSpinner(plotOutput("refIntervalPlotBoth"),type = 2, 
                              color = "red", color.background = "white",  
                              size = 2, hide.ui = FALSE)),
        column(4, plotOutput("refIntervalPlotM")),
        column(4, plotOutput("refIntervalPlotF"))
    )))
  )
))


# Define server
server <- function(input, output, session) {
  session$cache <- cache_mem(max_size = 4000e6)
  
  dateRange <- reactive({
    Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
  })
  
  output$dateRangeOutput <- renderText({
    minDate <- min(dateRange()$Datum)
    maxDate <- max(dateRange()$Datum)
    paste("Date Range:", minDate, "to", maxDate, "\n unique FID")
  })
  
  output$UnitOutput <- renderText({
    Unit <- unique(dateRange()$EINHEIT)
    paste(input$analyte,"(",Unit,")\nall genders\n \n \n ")
  })
  
  output$URIOutputM <- renderText({
    low <- unique(dateRange()$`REF_UNTEN M`)
    high <- unique(dateRange()$`REF_OBEN M`)
    Unit <- unique(dateRange()$EINHEIT)
    paste(input$analyte,"(",Unit,")\nmale\nrecent ZLM ref. int.:\nlower limit",low,"\nupper limit",high)

  })
  
  output$URIOutputF <- renderText({
    lowf <- unique(dateRange()$`REF_UNTEN W`)
    highf <- unique(dateRange()$`REF_OBEN W`)
    Unit <- unique(dateRange()$EINHEIT)
    paste(input$analyte,"(",Unit,")\nfemale\nrecent ZLM ref. int.:\nlower limit",lowf,"\nupper limit",highf)
    
  })
  
  
  refIntervalBoth <- reactive({
    # Filter the dataframe based on the selected analyte
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
    # Filter the filteredData to retain unique rows based on "b_Fallnummer"
    uniqueData <- filteredData[!duplicated(filteredData$b_Fallnummer),]
    # Extract the values for the reference interval calculation
    values <- uniqueData$Werte
    # Calculate the reference interval using refineR
    findRI(Data = values)
  })
  
  selected.data <- reactive({  
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
    uniqueData <- filteredData[!duplicated(filteredData$b_Fallnummer), ]
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$analyte, ".tsv")
    },
    content = function(file) {
     vroom_write(selected.data(), file)
    }
  )
  
  refIntervalM <- reactive({
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte & Combined_Data_KC$f_Geschl. == "M", ]
    uniqueDataM <- filteredData[!duplicated(filteredData$b_Fallnummer),]
    values <- uniqueDataM$Werte
    findRI(Data = values)
  })
  
  refIntervalF <- reactive({
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte & Combined_Data_KC$f_Geschl. == "F", ]
    uniqueData <- filteredData[!duplicated(filteredData$b_Fallnummer),]
    values <- uniqueData$Werte
    findRI(Data = values)
  })
  
  output$refIntervalOutputBoth <- renderPrint({
    print(refIntervalBoth())
  })
  
  output$refIntervalOutputM <- renderPrint({
    print(refIntervalM())
  })
  
  output$refIntervalOutputF <- renderPrint({
    print(refIntervalF())
  })
  
  output$refIntervalPlotBoth <- renderPlot({
    plot(refIntervalBoth(), Nhist = 10, showPathol = TRUE)
  })
  
  output$refIntervalPlotM <- renderPlot({
    plot(refIntervalM())
  })
  
  output$refIntervalPlotF <- renderPlot({
    plot(refIntervalF())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
