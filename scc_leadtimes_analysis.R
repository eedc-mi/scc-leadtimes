#-----------------
# Prepare Session
#-----------------

# Load Libraries

library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(officer)
library(rvg)
library(flextable)
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

# USI lead source ET or not

USI$leadsource_ET <- USI$lead_source == "Edmonton Tourism"

# Remove NA lead source events from USI data

USI <- USI %>% drop_na(leadsource_ET)

# Year booked and year created variables

USI$year_booked <- year(USI$date_booked)

SV$created_year <- year(SV$created)

# Remove any events in SV that have no preferred start date

SV <- SV %>% drop_na(meeting_dates_preferred_start)

# Create USI data subsets

leadsource_subset <- subset(USI, USI$lead_source == "Edmonton Tourism")

#class_subset <- subset(USI, USI$class)

newbusiness_subset <- subset(USI, USI$is_this_event_new_business == "Yes")

# Create SV data subset

convcentre_subset <- subset(SV, SV$convention_center == "Yes")

# Create comparison data set

compare_data <- merge(USI, SV, by = c("lead_id"), suffixes = c("_USI", "_SV"))
compare_data$SVtoUSI_leadtime <- compare_data$date_booked - compare_data$tentative_lead_date

# Overall Summary Stats

USIleadtime_all <- USI %>%
  summarise(Lead_Time = "USI Lead Time",
            Mean = round(mean(lead_time), digits = 0),
            Median = round(median(lead_time), digits = 0),
            Number_of_Events = n())

SVleadtime_all <- SV %>%
  summarise(Lead_Time = "SV Lead Time",
            Mean = round(mean(lead_time), digits = 0),
            Median = round(median(lead_time), digits = 0),
            Number_of_Events = n())

compareleadtime_all <- compare_data %>%
  summarise(Lead_Time = "Internal Lead Time",
            Mean = round(mean(SVtoUSI_leadtime), digits = 0),
            Median = round(median(SVtoUSI_leadtime), digits = 0),
            Number_of_Events = n())

all_leadtime <- rbind(USIleadtime_all, SVleadtime_all, compareleadtime_all)

# Flextable

ft_leadtimeall <- flextable(all_leadtime) %>%
  add_header(top = TRUE, Lead_Time = "Lead Time Summary Statistics (All Data)",
             Mean = "", Median = "", Number_of_Events = "") %>%
  merge_at(i = 1, j = 1:4, part = "header") %>%
  fontsize(part = "header", size = 20) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 1.5) %>%
  width(j = 3, width = 1.5) %>%
  width(j = 4, width = 3)

# Summary Stats (lead_source = ET, conv_center = yes)

USIleadtime_summary <- leadsource_subset %>%
  summarise(Lead_Time = "USI Lead Time",
            Mean = round(mean(lead_time), digits = 0),
            Median = median(lead_time),
            Number_of_Events = n())

SVleadtime_summary <- convcentre_subset %>%
  summarise(Lead_Time = "SV Lead Time",
            Mean = round(mean(lead_time), digits = 0),
            Median = median(lead_time),
            Number_of_Events = n())

compareleadtime_summary <- compare_data %>%
  summarise(Lead_Time = "Internal Lead Time",
            Mean = round(mean(SVtoUSI_leadtime), digits = 0),
            Median = median(SVtoUSI_leadtime),
            Number_of_Events = n())

summary_leadtime <- rbind(USIleadtime_summary, SVleadtime_summary, compareleadtime_summary)

# Flextable

ft_leadtimesummary <- flextable(summary_leadtime) %>%
  add_header(top = TRUE, Lead_Time = "Lead Source Edmonton Tourism Summary Statistics",
             Mean = "", Median = "", Number_of_Events = "") %>%
  merge_at(i = 1, j = 1:4, part = "header") %>%
  fontsize(part = "header", size = 20) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 1.5) %>%
  width(j = 3, width = 1.5) %>%
  width(j = 4, width = 3)

# Breakdowns by event year and event month

leadsource_subset$event_year <- year(leadsource_subset$start_date)
leadsource_subset$year_booked <- year(leadsource_subset$date_booked)
USI$year_booked <- year(USI$date_booked)

convcentre_subset$event_year <- year(convcentre_subset$meeting_dates_preferred_start)
convcentre_subset$lead_year <- year(convcentre_subset$tentative_lead_date)

# Tentative Counts from SV

convcentre_subset$tld <- is.na(convcentre_subset$tentative_lead_date)
convcentre_subset$created_year <- year(convcentre_subset$created)

tentative_counts <- subset(convcentre_subset, convcentre_subset$created_year == 2017)

event_counts_year <- convcentre_subset %>%
  group_by(lead_year) %>%
  summarise(number_of_events = n())

# Average Lead Time by Class

leadtime_by_class <- USI %>%
  group_by(class) %>%
  summarise(Mean = round(mean(lead_time), digits = 0),
            Median = round(median(lead_time), digits = 0),
            Number_of_Events = n())

# Flextable

ft_leadtimebyclass <- flextable(leadtime_by_class) %>%
  add_header(top = TRUE, class = "Lead Time by Class (USI)",
             Mean = "", Median = "", Number_of_Events = "") %>%
  merge_at(i = 1, j = 1:4, part = "header") %>%
  fontsize(part = "header", size = 20) %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  padding(padding = 3, part = "all") %>%
  autofit() %>%
  width(j = 1, width = 3) %>%
  width(j = 2, width = 1.5) %>%
  width(j = 3, width = 1.5) %>%
  width(j = 4, width = 3)

# Average Lead Time by Year Booked Bar Chart ----------------------------------------------------------

USI_yearbookedmean <- USI %>%
  group_by(year_booked, leadsource_ET) %>%
  summarise(mean_lead_time = mean(lead_time)) %>%
  ggplot(aes(x = year_booked, y = mean_lead_time, fill = leadsource_ET)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Year Booked", y = "Average Lead Time", fill = "Lead Source: Edmonton Tourism") +
  ggtitle("Average Lead Time by Year Booked (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "top", legend.direction = "horizontal")

# Lead Time by Date Entered in System Scatterplots ---------------------------------------------------------------------------

USI_datebooked <- USI %>%
  filter(year_booked == 2015 | year_booked == 2016 | year_booked == 2017) %>%
  ggplot(aes(x = date_booked, y = lead_time,
             colour = leadsource_ET, stat = "identity")) +
  geom_point() +
  labs(x = "Year Booked", y = "Lead Time", colour = "Lead Source: Edmonton Tourism") +
  ggtitle("Lead Time by Date Booked (USI)") +
  geom_smooth(method = lm, se = FALSE) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "top", legend.direction = "horizontal")

SV_datecreated <- SV %>%
  filter(created_year == 2015 | created_year == 2016 | created_year == 2017) %>%
  ggplot(aes(x = created, y = lead_time,
             colour = convention_center, stat = "identity")) +
  geom_point() +
  labs(x = "Created Date", y = "Lead Time", colour = "Asked for a convention center") +
  ggtitle("Lead Time by Created Date (SV)") +
  geom_smooth(method = lm, se = FALSE) +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "top", legend.direction = "horizontal")

matched_eventdate <- compare_data %>%
  ggplot(aes(x = start_date, y = SVtoUSI_leadtime)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time between SV and USI by Event Date") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Lead Time by Year Entered in System Scatterplots ---------------------------------------------------------------------------

USI_yearbooked <- leadsource_subset %>%
  ggplot(aes(x = year_booked, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Year Booked (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

SV_leadyear <- convcentre_subset %>%
  ggplot(aes(x = lead_year, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Lead Year (SV)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Lead Time by Event Date Scatterplots ---------------------------------------------------------------------------

USI_startdate <- leadsource_subset %>%
  ggplot(aes(x = start_date, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Start Date (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

SV_startdate <- convcentre_subset %>%
  ggplot(aes(x = meeting_dates_preferred_start, y = lead_time)) +
  geom_point(aes(), stat = "identity") +
  ggtitle("Lead Time by Preferred Start Date (SV)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Average Lead Time Bar Chart ---------------------------------------------------------------------------

SV_datecreatedmean <- convcentre_subset %>%
  group_by(created) %>%
  summarise(mean_lead_time = mean(lead_time)) %>%
  ggplot(aes(x = created, y = mean_lead_time)) +
  geom_bar(aes(), stat = "identity") +
  ggtitle("Lead Time by Created Date") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Line Charts ---------------------------------------------------------------------------

USI_datebookedline <- leadsource_subset %>%
  ggplot(aes(x = date_booked)) +
  geom_line(aes(y = lead_time), stat = "identity") +
  ggtitle("Lead Time by Date Booked (USI)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

SV_datecreatedline <- convcentre_subset %>%
  ggplot(aes(x = created)) +
  geom_line(aes(y = lead_time), stat = "identity") +
  ggtitle("Lead Time by Created Date (SV)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

matched_startdateline <- compare_data %>%
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

matched_dateentered <- compare_data %>%
  ggplot(aes()) +
  geom_line(aes(x = date_booked, y = SVtoUSI_leadtime), stat = "identity", colour = "blue") +
  geom_line(aes(x = created, y = SVtoUSI_leadtime), stat = "identity", colour = "red") +
  ggtitle("Comparison of Internal Lead Time") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

# Important Insights ---------------------------------------------------------------------------

# Set font style

text_prop <- fp_text(font.size = 16)
level_1 <- fp_text(font.size = 14, bold = TRUE)
level_2 <- fp_text(font.size = 14)
level_3 <- fp_text(font.size = 12)

# Build Slide Deck

ppt <- read_pptx(file.path("V:", "Economic Intelligence", "Shaw Conference Centre",
                            "Projects", "Discount Analysis", "template.pptx"))

ppt <- ppt %>%
  
  # Title Slide
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  ph_with_text(type = "ctrTitle", str = "Lead Times Analysis") %>% 
  ph_with_text(type = "subTitle", str = "USI & Simple View Comparison") %>%
  
  # Definitions Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Mean - the average of all observation values",
              style = level_1) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Median - the value that is the midpoint of the observations when they are sorted from smallest to largest",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Can be helpful to show if the data is skewed to one side, meaning that more of the data is closer to the maximum value or the minimum value in the data set",
              style = level_2) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "If median is close to the max observation value, most of the observations appear closer to the maximum observation value in the data set.",
              style = level_3) %>%
  ph_add_par(level = 3) %>%
  ph_add_text(str = "If median is close to the min observation value, most of the observations appear closer to the minimum observation value in the data set.",
              style = level_3) %>%
  ph_with_text(type = "title", index = 1, str = "Definitions") %>%
  ph_with_text(type = "sldNum", str = "1" ) %>%
  
  # Data Description Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Data from USI",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "493 conventions (of which 117 have a lead source of Edmonton Tourism)",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Any events with a date booked of January 31, 2015 (the date of the system switch) were eliminated from analysis",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Data from Simple View",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "1295 events (of which 223 asked for a convention center)",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Any events with no preferred start date were eliminated from analysis",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Data that could be matched by event start date and account name between USI and Simple View",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "37 events",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Important Note:",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Data from 2015 may not be reliable due to the system and process changes that occured during this year",
              style = level_2) %>%
  ph_with_text(type = "title", index = 1, str = "Data Utilized") %>%
  ph_with_text(type = "sldNum", str = "2" ) %>%
  
  # Variables created/added for analysis Slide
  
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "USI lead time",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= start_date - date_booked",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Number of days between being entered in USI and the date of the event",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Simple View lead time",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= meeting_dates_preferred_start - created",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Number of days between being entered into Simple View and date of the event",
              style = level_2) %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "Time between being entered into Simple View and being entered into USI",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "= date_booked - created",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Number of days between being a tentative lead in Simple View and being booked in USI",
              style = level_2) %>%
  ph_with_text(type = "title", index = 1, str = "Variables Added for Analysis") %>%
  ph_with_text(type = "sldNum", str = "3" ) %>%
  
  # Expectations Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 1) %>%
  ph_add_text(str = "A decrease in lead times",
              style = level_1) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "Could be the result of the process change that has Edmonton Tourism managing the lead creation process, could be that overall lead times are shrinking in the market, or could be a combination of factors",
              style = level_2) %>%
  ph_with_text(type = "title", index = 1, str = "Data Expectations") %>%
  ph_with_text(type = "sldNum", str = "4" ) %>%
  
  # Summary Slide
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "body") %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "From the analysis completed, there is no clear decrease in lead times over the years for which data is available in either USI or Simple View.",
              style = level_2) %>%
  ph_add_par(level = 2) %>%
  ph_add_text(str = "This does not necessarily mean that lead times are not decreasing, but it may be too early to view such an effect in the data.",
              style = level_2) %>%
  ph_with_text(type = "title", index = 1, str = "Summary of Results") %>%
  ph_with_text(type = "sldNum", str = "5" ) %>%
  
  # Summary lead times (All Data)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = "This table shows lead times for all conventions from USI and all events from Simple View. The average and median lead time for USI is greater than those for Simple View, which may indicate that many of the conventions in USI were not leads from Edmonton Tourism. If the majority of leads were from Edmonton Tourism, it would be expected that Simple View lead times would be longer than USI lead times since the leads would be entered into Simple View first.",
              type = "title", style = text_prop) %>%
  ph_with_flextable(value = ft_leadtimeall, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "6" ) %>%
  
  # Summary lead times (Subsets)
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = "This table shows only conventions from USI with lead source Edmonton Tourism, and all events from Simple View that asked for a convention center. Mean and median USI lead times are higher than those in Simple View; the opposite of what is expected.",
              type = "title", style = text_prop) %>%
  ph_with_flextable(value = ft_leadtimesummary, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "7" ) %>%
  
  # Summary USI lead times by class
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = "This table shows USI lead times by class. Convention lead times vary between 537 days and 770 days.",
              type = "title", style = text_prop) %>%
  ph_with_flextable(value = ft_leadtimebyclass, type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "8" ) %>%
  
  # USI average yearly lead times bar chart
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = "Average lead times in USI have not shown the expected drop over the past three years.",
              type = "title", style = text_prop) %>%
  ph_with_vg(code = print(USI_yearbookedmean), type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "9" ) %>%
  
  # USI lead times scatterplot
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = "Plotting all USI convention lead times also shows no significant decline in lead times whether the lead source was Edmonton Tourism or not.",
              type = "title", style = text_prop) %>%
  ph_with_vg(code = print(USI_datebooked), type = "body", index = 1) %>%
  ph_with_text(type = "sldNum", str = "10" ) %>%
  
  # Simple View lead times scatterplot
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_empty(type = "title") %>%
  ph_add_par() %>%
  ph_add_text(str = "Using the same method to plot all Simple View event lead times, there is no significant decline in lead times whether the event asked for a convention center or not.",
              type = "title", style = text_prop) %>%
  ph_with_vg(code = print(SV_datecreated), type = "body") %>%
  ph_with_text(type = "sldNum", str = "11" ) %>%
  
print(ppt, target = "lead_times.pptx") %>%
  invisible()

