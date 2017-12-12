#-----------------
# Prepare Session
#-----------------

# Load Libraries

library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(here)

# Set Working Directory

setwd(here())

# Set Directory for data files

dataPath <- file.path(
  "V:",
  "Economic Intelligence",
  "Shaw Conference Centre",
  "Projects",
  "Convention Lead Times"
)

# Read in Data

USI <- read_csv(file.path(dataPath,
                         "Conventions from USI - Update Dec 8.csv"))
SV <- read_csv(file.path(dataPath,
                         "Meeting Sales Leads from Simpleview - All Dates.csv"))

# Fix column names

fixName <- function(string) {
  newName <- unlist(strsplit(string, "-"))[1]
  newName <- trimws(tolower(newName))
  newName <- str_replace_all(newName, " ", "_")
  newName <- str_replace_all(newName, "/", "_")
#  newName <- str_replace_all(newName, "?", "_")
  
  return(newName)
}

names(USI) <- sapply(names(USI), fixName)
names(SV) <- sapply(names(SV), fixName)

# Remove any events that were entered on January 31, 2015 (system switch)

USI <- USI[!(USI$date_booked == 31-01-15),]

# Convert dates from class character to class date

USI$date_booked <- as.Date(USI$date_booked, "%y-%m-%d")

USI$start_date <- as.Date(USI$start_date, "%y-%m-%d")

SV$tentative_lead_date <- as.Date(SV$tentative_lead_date, "%m/%d/%Y")

SV$`meeting_dates_(preferred_start)` <- as.Date(SV$`meeting_dates_(preferred_start)`, "%m/%d/%Y")

# Create USI data subsets

leadsource_subset <- subset(USI, USI$lead_source == "Edmonton Tourism")

#class_subset <- subset(USI, USI$class)

newbusiness_subset <- subset(USI, USI$`is_this_event_new_business?` == "yes")

# Create SV data subset

convcentre_subset <- subset(SV, SV$convention_center == "yes")

# Create comparison data set

compare_data <- merge(USI, SV, by = c("lead_id"))

compare_data$SVtoUSI_leadtime <- compare_data$date_booked - compare_data$tentative_lead_date

compare_data$USI_leadtime <- compare_data$start_date - compare_data$date_booked

compare_data$SV_leadtime <- compare_data$start_date - compare_data$tentative_lead_date

# Lead time calculations

leadsource_subset$leadtime <-
  leadsource_subset$start_date - leadsource_subset$date_booked

convcentre_subset$leadtime <-
  convcentre_subset$`meeting_dates_(preferred_start)` - convcentre_subset$tentative_lead_date








