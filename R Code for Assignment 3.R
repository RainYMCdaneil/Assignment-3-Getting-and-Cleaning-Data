getwd()
setwd("F:/github local/Assignment-3-Getting-and-Cleaning-Data")
#data import
library(tidyverse)
data <- read_csv("StormEvents_details-ftp_v1.0_d1999_c20220425.csv")
head(data)
#Limit the dataframe
newdata <- data[c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE")]
head(newdata)
#Arrange the data by the state name
library(dplyr)
sorted_data <- arrange(newdata, STATE)
head(sorted_data)
#Change state and county names to title case
library(stringr)
sorted_data$STATE <- str_to_title(sorted_data$STATE)
sorted_data$CZ_NAME <- str_to_title(sorted_data$CZ_NAME)
head(sorted_data)
#Limit to the events listed by county FIPS
library(dplyr)
filtered_data <- filter(sorted_data, CZ_TYPE == "C")
#Remove the CZ_TYPE column
final_data <- select(filtered_data, -CZ_TYPE)
head(final_data)
#Pad the state and county FIPS with a “0” at the beginning and unite the two columns to make one FIPS column with the new state-county FIPS code
final_data$STATE_FIPS <- str_pad(final_data$STATE_FIPS, width = 2, side = "left", pad = "0")
final_data$CZ_FIPS <- str_pad(final_data$CZ_FIPS, width = 3, side = "left", pad = "0")
final_data <- unite(final_data, "fips", STATE_FIPS, CZ_FIPS, sep = "", remove = TRUE)
head(final_data)
#Change all the column names to lower case
final_data <- rename_all(final_data, tolower)
head(final_data)
#create a dataframe with these three columns
data("state")
us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)
head(us_state_info)
#Create a dataframe with the number of events per state. 
birth_year_data <- filter(final_data, begin_yearmonth %/% 100 == 1999)
state_counts <- data.frame(table(birth_year_data$state))
names(state_counts) <- c("state", "event_count")
head(state_counts)
#Merge in the state information dataframe in last question
merged_data <- merge(x = state_counts, y = us_state_info, by = "state")
head(merged_data)
#Remove any states that are not in the state information dataframe
final_data2 <- semi_join(merged_data, us_state_info, by = "state")
head(final_data2)
#Create the plot
storm_plot <-ggplot(final_data2,aes(x=area,y=event_count))+geom_point(aes(color = region))+labs(x ="Land area (square miles)",y =" # of storm events in 2017")
storm_plot