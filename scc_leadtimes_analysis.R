#-----------------
# Prepare Session
#-----------------

# Load Libraries

library(tidyverse)
library(ggplot2)
library(lubridate)
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

d1 <- read_csv(file.path(dataPath,
                         "Conventions from USI - Update Dec 8.csv"))
d2 <- read_csv(file.path(dataPath,
                         "Meeting Sales Leads from Simpleview - All Dates.csv"))

# Create Subsets

lead_source_subset <- subset(d1, d1$`Lead Source`)





