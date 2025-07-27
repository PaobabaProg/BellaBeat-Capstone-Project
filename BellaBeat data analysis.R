#Setting up the packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")

#loading up the packaged
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)

#importing the CSV files into R enviroment
daily_activities <- read.csv("Bellabeat Data - dailyActivity_Cleaned.csv")
sleep_info <- read.csv("sleepDay_merged.csv")
Intensities <- read.csv("dailyIntensities_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")

#looking at the data to ensure correct import
#View each data set
View(daily_activities)
View(Intensities)
View(sleep_info)
View(weight)

#sleep_info and weight have the wrong format where date is mixed with time stamp, this help clears it up

sleep_info$SleepDay <- as.Date(sleep_info$SleepDay, format = "%m/%d/%Y %I:%M:%S %p")
sleep_info$SleepDay <- format(sleep_info$SleepDay, "%m/%d/%Y")
weight$Date <- as.POSIXct(weight$Date, format = "%m/%d/%Y %I:%M:%S %p")
weight$Date <- format(weight$Date, "%m/%d/%Y")

#finding the summary in our data set

daily_activities %>%
  select(TotalSteps, TotalDistance, VeryActiveMinutes, SedentaryMinutes) %>%
  summary()

ggplot(data=daily_activities, aes(x=TotalSteps, y = SedentaryMinutes)) %>%
  + geom_point() %>%
  + geom_smooth(method = "lm", se = FALSE)

cor.test(daily_activities$TotalSteps, daily_activities$SedentaryMinutes)

sleep_info <- sleep_info %>%
  rename(ActivityDate = SleepDay)
str(daily_activities$ActivityDate) #chr Array
str(sleep_info$ActivityDate) #chr Array
sleep_info$ActivityDate <- as.Date(sleep_info$ActivityDate, format = "%m/%d/%Y")
daily_activities$ActivityDate <- as.Date(daily_activities$ActivityDate, format = "%m/%d/%Y")

#Merging the data on Activitydate using Inner Join on Sleep
merged_data <- inner_join(daily_activities, sleep_info, by = c("Id", "ActivityDate"))
length(unique(merged_data$Id)) # there's a total of 24 unique participant

# No Visible Relationship suggesting towards a longer sleep 
merged_data %>%
  filter(VeryActiveMinutes > 0) %>%
  ggplot(aes(x=VeryActiveMinutes, y = TotalMinutesAsleep)) + geom_point() + geom_smooth(method = "lm", se=TRUE)
cor.test(merged_data$VeryActiveMinutes, merged_data$TotalMinutesAsleep)
#Relationship between Staying still and sleeping minutes
ggplot(data=merged_data, aes(x=SedentaryMinutes, y = TotalMinutesAsleep))+ geom_point() + geom_smooth(method = "loess", se=TRUE)+ labs( title = "Total Minutes Asleep against Sedentary Minute", x = "Sedentary Minutes", y = "Total Minutes Asleep")
cor.test(merged_data$TotalMinutesAsleep, merged_data$SedentaryMinutes)
#Deeper look
longSedentaryMinutes <- merged_data %>% 
  filter(SedentaryMinutes > 800)

longSedentaryMinutes %>%
  summarise(mean_sleep = mean(TotalMinutesAsleep, na.rm = FALSE))

merged_data %>% 
  summarise(mean_sleep = mean(TotalMinutesAsleep, na.rm = FALSE))