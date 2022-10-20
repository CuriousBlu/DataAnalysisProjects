## Install needed packages and libraries
install.packages('tidyverse')
library(tidyverse)

install.packages("skimr")
library(skimr)

install.packages("janitor")
library(janitor)

install.packages("readr")
library(readr)

install.packages("lubridate")
library(lubridate)


## Load tables from the 1st dataset and create data frames 
daily_activity <- read.csv("dailyActivity_merged.csv")
daily_sleep <- read.csv("sleepDay_merged.csv")
daily_calories <- read.csv("dailyCalories_merged.csv")

## Look at the data sets to see what similarities,differences, structure, and data types they have
## Looking at the daily_activity data
head(daily_activity)
glimpse(daily_activity)
colnames(daily_activity) ## 15 Columns


## Looking at the daily_sleep data
head(daily_sleep)
glimpse(daily_sleep)
colnames(daily_sleep) ## 5 Columns


## Looking at the daily_calories data
head(daily_calories)
glimpse(daily_calories) ## 3 Columns
colnames(daily_calories)

## Next, I ask some summary statistic questions to further explore the data

## Summary For the daily activity data

daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()


## Summary For the sleep data

daily_sleep %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()


##How many unique participants are there in each data frame? 
## -It looks like there may be more participants in the daily activity dataset than the sleep data set.
n_distinct(daily_activity$Id) ##33
n_distinct(daily_sleep$Id) ##24

##How many observations are there in each data frame?
nrow(daily_sleep) ##413
nrow(daily_calories) ##940
nrow(daily_activity)  ##940

## What are the min and max date of the tracking data for activity?

mindate_activity <- min(daily_activity$ActivityDate)
maxdate_activity <- max(daily_activity$ActivityDate)

## What are the min and max date of the tracking data for sleep?
mindate_sleep <- min(daily_sleep$SleepDay)
maxdate_sleep <- max(daily_sleep$SleepDay)


## What is the min and max and avg total calories earned per day?
min_calories <- min(daily_calories$Calories)
max_calories <- max(daily_calories$Calories)
avg_calories <- mean(daily_calories$Calories)


## Some quick preliminary observations about the data
## Participants have a unique ID (Id)
## This data is in Long format
## Each record represents the activity for a particular day, each participants have multiple days recorded
## There's not as much Sleep data here compared to the records for Daily Activity and Daily Calories and some participants have more sleep data recorded than others


## What's the relationship between Total Active distance and Total Activity?
## Going to create 2 columns and save them to a new data frame called summed_dailyActive
summed_dailyActive <- mutate(daily_activity,TotalActivity=LightlyActiveMinutes+VeryActiveMinutes+FairlyActiveMinutes) %>% 
mutate(daily_activity,TotalActiveDistance=VeryActiveDistance+ModeratelyActiveDistance+LightActiveDistance)

ggplot(data=summed_dailyActive, aes(x=TotalActiveDistance, y=TotalActivity))+
  geom_point(mapping=aes(color=Calories))+geom_smooth(formula = y~x,method=lm,se=FALSE)+
  scale_colour_gradient(low="blue", high="orange", na.value= "white")+
  labs(title="Daily Total Activity Compared to Total Distance", caption=paste0("Data from: ", mindate_activity, " to ",maxdate_activity), x ="Total Distance", y="Total Activity Minutes")

ggsave('TotalDistance_TotalActivity.png', width=16,
       height=8)


## What's the relationship between steps taken in a day and sedentary minutes? 
## How could this help inform the customer segments that we can market to? 

ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes))+
  geom_point(mapping=aes(color=TotalDistance))+geom_smooth(formula = y~x,method=lm,se=FALSE)+
  scale_colour_gradient(low="blue", high="orange", na.value= "white")+
  labs(title="Daily Total Steps Compared to Total Sedentary time", caption=paste0("Data from: ", mindate_activity, " to ",maxdate_activity), x ="Total Steps", y="Sedentary Minutes")

ggsave('Steps_vs_Sedentary.png', width=16,
       height=8)

## This denotes a negative trend where as the Total Steps increases, the Total Sedentary Minutes Decrease. 
## However, this sample is not big enough to represent a statistical significance


## What's the relationship between total time asleep and total time in bed? 
## Plotting the Total time in Bed to Total Minutes Asleep
## Need to combine the Calorie and Sleep table

## Outer join on summed daily and sleep
sleep_calories= daily_sleep %>% 
  left_join(daily_calories,by="Id", "ActivityDate")
nrow(sleep_calories)
n_distinct(sleep_calories$Id)

ggplot(data=sleep_calories, aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+ 
  geom_point(mapping=aes(color=Calories))+
  geom_smooth(formula = y~x,method=lm, se=FALSE)+
  scale_colour_gradient(low="blue", high="orange", na.value= "white")+
  labs(title="Total Minutes Alsleep vs Total Time in Bed", caption ="Data captured from worn activity tracker (fitbit)", x="Total Minutes Asleep", y= "Total Time In Bed")

ggsave('Time_In_Bed_Minutes_Sleep.png', width=16,
       height=8)

## What is the relationship between total activity and amount of calories burned?
## Plotting the relationship between activity and calories

ggplot(data=summed_dailyActive, aes(x=TotalActivity, y=Calories))+
  geom_point(mapping=aes(color=TotalActiveDistance))+
  geom_smooth(formula = y~x,method=lm, se=FALSE)+
  scale_colour_gradient(low="blue", high="orange", na.value= "white")+
  labs(title="Calories burned compared to Total time Active", caption=paste0("Data from: ", mindate_activity, " to ",maxdate_activity), x ="Total Active Minutes", y="Total Calories")

ggsave('Calories_vs_TotalActivity.png', width=16,
       height=8)

## What is the relationship between Total Activity Distance and Total Minutes Asleep?
## Need to combine the sleep and activity data sets
## Need to change the name of the SleepDay column in daily_sleep to ActivityDate
## Need to change the date format of daily_sleep so that each data type is similar 🙄 here goes nothing

## After 30 minutes of frustration 😤I take a break for the night 🤞
## The next afternoon, I do a quick search, refresh my memory on the problem and decide to 
## reload the sleep data set to see what the original datetime format was: it's chr *deep sigh* of course 🤦
## SUCCESS!!!! BOOYAH!! 🙌

## Used as.Date for the Date/time conversion issue
daily_sleep$SleepDay <- format(as.Date(daily_sleep$SleepDay, format ="%m/%d/%Y %H:%M:%S"), "%m/%d/%Y") 

## Need to change the name of the SleepDay column in daily_sleep to ActivityDate
colnames(daily_sleep)[2]<- "ActivityDate"


## Using Excel, I create a new data set csv of total summed sleep info for each Id and total summed activity info for each Id

## Importing the created csv that has the info I need
total_sleep <- read.csv("total_sleep.csv")
glimpse(total_sleep)


total_activity <- read.csv("total_activity.csv")
glimpse(total_activity)

##Left joining the data sets to create a new data frame
active_sleep= total_sleep %>% 
  left_join(total_activity,by=("Id"))    ##Finally, but I'd like to figure out how to solve this problem using R, in the future


##Plotting the information

ggplot(data=active_sleep, aes(x=TotalActiveMinutes, y=TotalMinutesAsleepSum))+
  geom_point(mapping=aes(color=CaloriesSum))+
  geom_smooth(formula = y~x,method=lm)+
  scale_colour_gradient(low="blue", high="orange", na.value= "white")+
  labs(title="Total sleep time compared to Total active time", caption=paste0("Data from: ", mindate_activity, " to ",maxdate_activity), x ="Total Active Minutes", y="Total Minutes Asleep")

ggsave('TotalSleep_vs_TotalActivity.png', width=16,
       height=8)


## Next I want to see how the Total Active Time and Total Sedentary minutes are related

ggplot(data=active_sleep, aes(x=TotalActiveMinutes, y=SedentaryMinutesSum))+
  geom_point(mapping=aes(color=TotalMinutesAsleepSum))+geom_smooth(formula = y~x,method=lm)+
  scale_colour_gradient(low="blue", high="orange", na.value= "white")+
  labs(title="Total Activity Compared with Total Sedentary time", caption=paste0("Data from: ", mindate_activity, " to ",maxdate_activity), x ="Total Active Minutes", y="Total Sedentary Minutes")

ggsave('Active Time_vs_Sedentary.png', width=16,
       height=8)


## Total Steps vs Total Time Sleep??

ggplot(data=active_sleep, aes(x=TotalStepsSum, y=TotalMinutesAsleepSum))+
  geom_point(mapping=aes(color=TotalActiveMinutes))+
  geom_smooth(formula = y~x,method=lm)+
  scale_colour_gradient(low="blue", high="orange", na.value= "white")+
  labs(title="Total Steps vs Total Sleep", caption=paste0("Data from: ", mindate_activity, " to ",maxdate_activity), x ="Total Steps", y="Total Sleep")

ggsave('TotalSteps_TotalSleep.png', width=16,
       height=8)


##OMG FINALLY!!!!
## I learned so much!!