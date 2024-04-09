# prepare libraries ---------------------------------------------------------
# Vector of libraries
packages <- c(
  "shiny", 
  "shinythemes", 
  "shinyjs", 
  "shinycssloaders", 
  "DT", 
  "memoise", 
  "readxl", 
  "data.table", 
  "tidyverse", 
  "zoo", 
  "plotly", 
  "lubridate", 
  "DBI", 
  "RSQLite", 
  "refineR", 
  "rstudioapi", 
  "flextable", 
  "cachem", 
  "scales",
  "dtplyr",
  "shinydashboard"
)

# Loop to check if libraries are installed and install them if not and load them
for (package in packages) {
  if (! require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# setting ggplot theme------------------------------------------------------
theme_set(
  theme_grey() +
    theme( text = element_text(size = 14),
           axis.title = element_text(size = 16),
           axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
           axis.text.y = element_text(size = 14))
)

# connect to database ------------------------------------------------------
# Function to detect the operating system and return the corresponding database path
getDatabasePath <- function() {
  # Detect operating system
  os <- Sys.info()["sysname"]
  
  # Set the path based on the operating system
  if (os == "Linux") {
    # Path for Ubuntu
    path <- "/home/olli/R_local/labStat/ClinicalChemistry_test.db"
  } else if (os == "Windows") {
    # Path for Windows
    path <- "C:/R_local/labStat/ClinicalChemistry_test.db"
  } else {
    stop("Operating system not supported")
  }
  
  return(path)
}

# set database directory
db.wd <- getDatabasePath()

# Connect to the database
db <- dbConnect(SQLite(), dbname = db.wd)



# set working directory ----------------------------------------------------
# Use rstudioapi to get the path of the current project
project_directory <- rstudioapi::getActiveProject()

# if running in an RStudio project, set the working directory to the project directory
# If not running in an RStudio project, print a message
if (!is.null(project_directory)) {
  setwd(project_directory)
} else {
  print("This R session is not running within an RStudio Project.")
}


# shiny app ----------------------------------------------------------------


# Define UI------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Dynamic Analyte Selection Based on Device"),
  sidebarLayout(
    sidebarPanel(
      # Device selection dropdown, updated from the database
      selectInput("device", "Choose a Device:", choices = NULL),
      
      # Analyte (Bezeichnung) selection dropdown, dynamically updated based on device selection
      selectInput("method", "Choose an Analyte:", choices = NULL)
    ),
    mainPanel(
      # Placeholder for any output or further UI elements
      textOutput("selectedMethod")
    )
  )
)



# Define server logic---------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Update Device choices based on the database
  observe({
    dbDevices <- dbGetQuery(db, "SELECT DISTINCT Gerät FROM MethodData WHERE Gerät IS NOT NULL ORDER BY Gerät ASC")
    updateSelectInput(session, "device", choices = dbDevices$Gerät)
  })
  
  # Update Method (Bezeichnung) choices based on selected Device
  observeEvent(input$device, {
    req(input$device) # Require a device to be selected
    methods <- dbGetQuery(db, sprintf("SELECT DISTINCT MeasurementData.Bezeichnung 
                                      FROM MeasurementData 
                                      JOIN MethodData ON MeasurementData.Methode = MethodData.Methode 
                                      WHERE MethodData.Gerät = '%s' 
                                      ORDER BY MeasurementData.Bezeichnung ASC", input$device))
    updateSelectInput(session, "method", choices = methods$Bezeichnung)
  })
  
  # Placeholder for displaying the selected method - can be expanded based on requirements
  output$selectedMethod <- renderText({
    paste("Selected Analyte:", input$method)
  })
  
  # Add here the logic to execute the query and further processing based on the selected method
  # Note: This is where you'd incorporate the R-routine provided, making sure to adapt it 
  #       to use `input$method` for dynamic analyte selection.
 observe({
   req(input$method)
  query <-  "SELECT  a.\"Alter\", a.sex, a.Methode, a.Bezeichnung, a.Werte, m.EINHEIT, a.KundenID
              FROM MeasurementData a
                JOIN (
                SELECT DISTINCT Fallnummer
                FROM MeasurementData
                 WHERE (Bezeichnung = 'Calcium' AND Werte > 2 AND Werte < 2.6)
                 OR (Bezeichnung = 'GFR(CKD-EPI)' AND Werte > 60)
                 OR (Bezeichnung = '25-OH-Vitamin D Total' AND Werte > 17)
                   GROUP BY Tagesnummer
                   HAVING COUNT (DISTINCT Bezeichnung) = 3
                ) b ON a.Fallnummer = b.Fallnummer 
               JOIN MethodData m ON a.Methode = m.Methode
                WHERE a.Bezeichnung = ?;"
  stmt <- dbSendQuery(db, query)
  dbBind(stmt, list(input$method))
  queryResult <- dbFetch(stmt)
  dbClearResult(stmt)
  
  if(!is.null(queryResult)){
    queryResult$Werte <- as.numeric(queryResult$Werte)
    return(queryResult)
  }
 
  
 })
}

# Run the application
shinyApp(ui = ui, server = server)
