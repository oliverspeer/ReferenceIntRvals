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

shinyOptions(cache = cache_mem(max_size = 4000e6))

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
        column(4, verbatimTextOutput("UnitOutput") |> withSpinner()),
        column(4, verbatimTextOutput("URIOutputM") |> withSpinner()),
        column(4, verbatimTextOutput("URIOutputF") |> withSpinner())
      ),
      fluidRow(
        column(4, verbatimTextOutput("refIntervalOutputBoth") |> withSpinner()),
        column(4, verbatimTextOutput("refIntervalOutputM") |> withSpinner()),
        column(4, verbatimTextOutput("refIntervalOutputF") |> withSpinner())
      ),
      fluidRow(
        column(4, plotOutput("refIntervalPlotBoth") |> withSpinner()),
        column(4, plotOutput("refIntervalPlotM") |> withSpinner()),
        column(4, plotOutput("refIntervalPlotF") |> withSpinner())
      )
    ))
  )
))


# Define server
server <- function(input, output, session) {
  
  # Define a memoized version of the expensive operation
  findRI_memo <- memoise(findRI, cache = shinyOptions()$cache)
  
  dateRange <- reactive({
    Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
  }) |> bindCache(input$analyte)
  
  output$dateRangeOutput <- renderText({
    minDate <- min(dateRange()$Datum)
    maxDate <- max(dateRange()$Datum)
    paste("Date Range:", minDate, "to", maxDate, "\n unique FID")
  }) |> bindCache(input$analyte)
  
  output$UnitOutput <- renderText({
    Unit <- unique(dateRange()$EINHEIT)
    paste(input$analyte,"(",Unit,")\nall genders\n \n \n ")
  }) |> bindCache(input$analyte)
  
  output$URIOutputM <- renderText({
    low <- unique(dateRange()$`REF_UNTEN M`)
    high <- unique(dateRange()$`REF_OBEN M`)
    Unit <- unique(dateRange()$EINHEIT)
    paste(input$analyte,"(",Unit,")\nmale\nrecent ZLM ref. int.:\nlower limit",low,"\nupper limit",high)
  }) |> bindCache(input$analyte)
  
  output$URIOutputF <- renderText({
    lowf <- unique(dateRange()$`REF_UNTEN W`)
    highf <- unique(dateRange()$`REF_OBEN W`)
    Unit <- unique(dateRange()$EINHEIT)
    paste(input$analyte,"(",Unit,")\nfemale\nrecent ZLM ref. int.:\nlower limit",lowf,"\nupper limit",highf)
  }) |> bindCache(input$analyte)
  
  refIntervalBoth <- reactive({
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
    uniqueData <- filteredData[!duplicated(filteredData$b_Fallnummer),]
    values <- uniqueData$Werte
    findRI_memo(Data = values)
  }) |> bindCache(input$analyte)
  
  selected.data <- reactive({  
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte, ]
    uniqueData <- filteredData[!duplicated(filteredData$b_Fallnummer), ]
  }) |> bindCache(input$analyte)
  
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
    findRI_memo(Data = values)
  }) |> bindCache(input$analyte)
  
  refIntervalF <- reactive({
    filteredData <- Combined_Data_KC[Combined_Data_KC$Bezeichnung == input$analyte & Combined_Data_KC$f_Geschl. == "F", ]
    uniqueData <- filteredData[!duplicated(filteredData$b_Fallnummer),]
    values <- uniqueData$Werte
    findRI_memo(Data = values)
  }) |> bindCache(input$analyte)
  
  output$refIntervalOutputBoth <- renderPrint({
    print(refIntervalBoth())
  }) |> bindCache(input$analyte)
  
  output$refIntervalOutputM <- renderPrint({
    print(refIntervalM())
  }) |> bindCache(input$analyte)
  
  output$refIntervalOutputF <- renderPrint({
    print(refIntervalF())
  }) |> bindCache(input$analyte)
  
  output$refIntervalPlotBoth <- renderPlot({
    plot(refIntervalBoth(), Nhist = 10, showPathol = TRUE)
  }) |> bindCache(input$analyte)
  
  output$refIntervalPlotM <- renderPlot({
    plot(refIntervalM())
  }) |> bindCache(input$analyte)
  
  output$refIntervalPlotF <- renderPlot({
    plot(refIntervalF())
  }) |> bindCache(input$analyte)
}

# Run the application
shinyApp(ui = ui, server = server)
