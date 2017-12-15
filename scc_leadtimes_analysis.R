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
  newName <- str_replace_all(newName, "\\(", "")
  newName <- str_replace_all(newName, "\\)", "")
  newName <- str_replace_all(newName, "\\?", "")
  
  return(newName)
}

names(USI) <- sapply(names(USI), fixName)
names(SV) <- sapply(names(SV), fixName)

# Convert dates from class character to class date

USI$date_booked <- as.Date(USI$date_booked, "%y-%m-%d")
USI$start_date <- as.Date(USI$start_date, "%y-%m-%d")
SV$tentative_lead_date <- as.Date(SV$tentative_lead_date, "%m/%d/%Y")
SV$meeting_dates_preferred_start <- as.Date(SV$meeting_dates_preferred_start, "%m/%d/%Y")
SV$created <- as.Date(substr(SV$created, 0, 10), "%m/%d/%Y")

# Remove test events and any events that were entered on January 31, 2015 (system switch)

USI <- USI[!(USI$date_booked == as.Date("2015-01-31")),]
USI <- USI[! grepl(" test ", USI$post_as, ignore.case = TRUE), ]

USI$lead_time <- USI$start_date - USI$date_booked
SV$lead_time <- SV$meeting_dates_preferred_start - SV$created

# Create USI data subsets

leadsource_subset <- subset(USI, USI$lead_source == "Edmonton Tourism")

#class_subset <- subset(USI, USI$class)

newbusiness_subset <- subset(USI, USI$is_this_event_new_business == "Yes")

# Create SV data subset

convcentre_subset <- subset(SV, SV$convention_center == "Yes")

# Create comparison data set

compare_data <- merge(USI, SV, by = c("lead_id"), suffixes = c("_USI", "_SV"))
compare_data$SVtoUSI_leadtime <- compare_data$date_booked - compare_data$tentative_lead_date

# Summary Stats

USIleadtime_summary <- leadsource_subset %>%
  summarise(Lead_Time = "USI Lead Time",
            Mean = mean(lead_time),
            Median = median(lead_time),
            Number_of_Events = n())

SVleadtime_summary <- convcentre_subset %>%
  summarise(Lead_Time = "SV Lead Time",
            Mean = mean(lead_time),
            Median = median(lead_time),
            Number_of_Events = n())

compareleadtime_summary <- compare_data %>%
  summarise(Lead_Time = "Internal Lead Time",
            Mean = mean(SVtoUSI_leadtime),
            Median = median(SVtoUSI_leadtime),
            Number_of_Events = n())

summary_leadtime <- rbind(USIleadtime_summary, SVleadtime_summary, compareleadtime_summary)

# Breakdowns by event year and event month

leadsource_subset$event_year <- year(leadsource_subset$start_date)
leadsource_subset$year_booked <- year(leadsource_subset$date_booked)

convcentre_subset$event_year <- year(convcentre_subset$meeting_dates_preferred_start)
convcentre_subset$lead_year <- year(convcentre_subset$tentative_lead_date)

# Average Lead Time by Year Booked Bar Chart ----------------------------------------------------------

leadsource_subset %>%
  group_by(year_booked) %>%
  summarise(mean_lead_time = mean(lead_time)) %>%
  ggplot(aes(x = year_booked, y = mean_lead_time)) +
  geom_bar(aes(), stat = "identity") +
  ggtitle("Lead Time by Year Booked (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Lead Time by Date Entered in System Scatterplots ---------------------------------------------------------------------------

leadsource_subset %>%
  ggplot(aes(x = date_booked, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Date Booked (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

convcentre_subset %>%
  ggplot(aes(x = created, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Created Date (SV)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

compare_data %>%
  ggplot(aes(x = start_date, y = SVtoUSI_leadtime)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time between SV and USI by Event Date") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Lead Time by Year Entered in System Scatterplots ---------------------------------------------------------------------------

leadsource_subset %>%
  ggplot(aes(x = year_booked, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Year Booked (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

convcentre_subset %>%
  ggplot(aes(x = lead_year, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Lead Year (SV)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Lead Time by Event Date Scatterplots ---------------------------------------------------------------------------

leadsource_subset %>%
  ggplot(aes(x = start_date, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Start Date (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

convcentre_subset %>%
  ggplot(aes(x = meeting_dates_preferred_start, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Preferred Start Date (SV)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Average Lead Time Bar Chart ---------------------------------------------------------------------------

convcentre_subset %>%
  group_by(created) %>%
  summarise(mean_lead_time = mean(lead_time)) %>%
  ggplot(aes(x = created, y = mean_lead_time)) +
  geom_bar(aes(), stat = "identity") +
  ggtitle("Lead Time by Created Date") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Line Charts ---------------------------------------------------------------------------

leadsource_subset %>%
  ggplot(aes(x = date_booked)) +
  geom_line(aes(y = lead_time), stat = "identity") +
  ggtitle("Lead Time by Date Booked (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

convcentre_subset %>%
  ggplot(aes(x = created)) +
  geom_line(aes(y = lead_time), stat = "identity") +
  ggtitle("Lead Time by Created Date (SV)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

compare_data %>%
  ggplot(aes(x = start_date)) +
  geom_line(aes(y = SVtoUSI_leadtime), stat = "identity") +
  ggtitle("Internal Lead Time by Event Date") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

compare_data %>%
  ggplot(aes(x = created)) +
  geom_line(aes(y = SVtoUSI_leadtime), stat = "identity") +
  ggtitle("Internal Lead Time by Created Date") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

compare_data %>%
  ggplot(aes(x = date_booked)) +
  geom_line(aes(y = SVtoUSI_leadtime), stat = "identity") +
  ggtitle("Internal Lead Time by Date Booked") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

compare_data %>%
  ggplot(aes()) +
  geom_line(aes(x = date_booked, y = SVtoUSI_leadtime), stat = "identity", colour = "blue") +
  geom_line(aes(x = created, y = SVtoUSI_leadtime), stat = "identity", colour = "red") +
  ggtitle("Internal Lead Time by Date Booked") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))










