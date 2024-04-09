# Import necessary libraries
library(shiny)
library(refineR)
library(tidyverse)
library(readxl)
library(shinythemes)
library(shinycssloaders)
library(memoise)
library(cachem)
library(vroom)

# set working directory-------------------------------------------------------
setwd("C:/R_local/ReferenceIntRvals")
# load data------------------------------------------------------------------
Combined_Data_KC <- readRDS("C:/R_local/labStat/Combined_Data_KC.rds")
oldLightChain.Data <- read_excel("C:\\R_local\\labStat\\FLK&FLL_2010_2017.xlsx")
# select light chain data------------------------------------------------------------------
lightChain_Data <- Combined_Data_KC [Combined_Data_KC$Bezeichnung == "FLL" | 
                                       Combined_Data_KC$Bezeichnung == "FLK" 
                                       #&
                                       #!duplicated(Combined_Data_KC$b_Fallnummer)
                                     , 
                                     ]
values <- lightChain_Data$Werte
findRI(Data = values)

# Function to get values based on 'Bezeichnung' and 'Year'
getValuesByYear <- function(data, bezeichnung, year) {
  filteredData <- data[data$Bezeichnung == bezeichnung & 
                         data$Year == year, "Werte"]
  filteredValue <- filteredData$Werte
  return(filteredValue)
}

# Example usage
valuesFLK.2018 <- getValuesByYear(lightChain_Data, "FLK", 2018)
valuesFLK.2019 <- getValuesByYear(lightChain_Data, "FLK", 2019)
valuesFLK.2020 <- getValuesByYear(lightChain_Data, "FLK", 2020)
valuesFLK.2020 <- getValuesByYear(lightChain_Data, "FLK", 2021)
valuesFLK.2020 <- getValuesByYear(lightChain_Data, "FLK", 2022)
RI.FLK.18 <- findRI(Data = valuesFLK.2018)
findRI(valuesFLK.2019)
findRI(valuesFLK.2020)
findRI(valuesFLK.2021)
findRI(valuesFLK.2022)

# Function to iterate through years and find Reference Interval
findRI_overYears <- function(data, bezeichnung) {
  # Find unique years in the dataset
  unique_years <- unique(data$Year)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  for(year in unique_years) {
    # Use the helper function to get values for each year
    values <- getValuesByYear(data, bezeichnung, year)
    
    # Calculate the Reference Interval if there are sufficient data points
    if (length(values) > 10) {
      result <- findRI(Data = values)
      results_list[[as.character(year)]] <- list(Year = year, Result = result)
    } else {
      results_list[[as.character(year)]] <- list(Year = year, Result = "Insufficient data")
    }
  }
  
  return(results_list)
}



# Example usage:
results.FLK <- findRI_overYears(lightChain_Data, "FLK")
results.FLL <- findRI_overYears(lightChain_Data, "FLL")
print(results.FLK)
print(results.FLL)

# This will store the Reference Interval for each year in the results_list,
# which you can then examine to see the results for each year.
