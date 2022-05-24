library(tidyverse)
library(janitor)
library(skimr)
library(here)
library(hablar)
library(readxl)
library(data.table)
library(chron)
library(readr)
library(lubridate)
library(magrittr)
library(DescTools)
library(metR)

setwd("C:/Users/FlasH/Desktop/202101-divvy-tripdata")

data_01 <- read_csv("202101-divvy-tripdata.csv")
data_02 <- read_csv("202102-divvy-tripdata.csv")
data_03 <- read_csv("202103-divvy-tripdata.csv")
data_04 <- read_csv("202104-divvy-tripdata.csv")
data_05 <- read_csv("202105-divvy-tripdata.csv")
data_06 <- read_csv("202106-divvy-tripdata.csv")
data_07 <- read_csv("202107-divvy-tripdata.csv")
data_08 <- read_csv("202108-divvy-tripdata.csv")
data_09 <- read_csv("202109-divvy-tripdata.csv")
data_10 <- read_csv("202110-divvy-tripdata.csv")
data_11 <- read_csv("202111-divvy-tripdata.csv")
data_12 <- read_csv("202112-divvy-tripdata.csv")

colnames(data_01)
colnames(data_02)
colnames(data_04)
colnames(data_05)
colnames(data_06)
colnames(data_07)
colnames(data_08)
colnames(data_09)
colnames(data_10)
colnames(data_11)
colnames(data_12)

all_trips <- bind_rows(data_01, data_02, data_03, data_04, data_05, data_06, data_07, data_08, data_09, data_10, data_11, data_12)

colnames(all_trips)  #List of column names
dim(all_trips)  #Dimensions of the data frame
head(all_trips)  #See the first 6 rows of data frame
str(all_trips)  #See list of columns and data types (numeric, character, etc)

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%u") #"%A" would deliver names of weekdays

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips$ride_length_m <- (difftime(all_trips$ended_at,all_trips$started_at))/60

str(all_trips)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
all_trips$ride_length_m <- as.numeric(as.character(all_trips$ride_length_m))
all_trips$month <- as.numeric(all_trips$month)
all_trips$day <- as.numeric(all_trips$day)
is.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length_m)
is.numeric(all_trips$month)
is.numeric(all_trips$day)

all_trips_v1 <- all_trips[!( all_trips$ride_length < 0),]

all_trips_v1 %>% 
  summarise(max(ride_length_m),min(ride_length_m),mean(ride_length_m))

all_trips_v1 %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(mapping = aes(x = day_of_week, y = number_of_rides)) + geom_col()

all_trips_v1 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_m)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v1 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length_m)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

all_trips_v1 %>%
  group_by(member_casual) %>%
  summarise(max(ride_length_m), min(ride_length_m),avg_ride_length = mean(ride_length_m)) %>% 
  ggplot(aes(x = member_casual, y = avg_ride_length,fill=member_casual)) +
  geom_col()+ scale_y_continuous(breaks = seq(0, 40, by = 5))

all_trips_v1 %>%
  group_by(member_casual) %>%
  summarise(rider_count = n()) %>% 
  ggplot(aes(x = member_casual, y = rider_count,fill=member_casual )) +
  geom_col()

all_trips_v1$season <- season(all_trips_v1$month)

all_trips_v1%>%
  group_by(season, day_of_week, member_casual) %>%   
  summarise(number_of_rides = n()						 
            ,avg_ride_length = mean(ride_length_m)) %>% 
  ggplot() + geom_col(mapping = aes(x = day_of_week, y = number_of_rides, fill = member_casual), position = "dodge") + facet_wrap(~season) + scale_y_continuous(breaks = seq(0, 400000, by = 50000))

all_trips_v1%>%
  group_by(season, day_of_week, member_casual) %>%   
  summarise(number_of_rides = n()						 
            ,avg_ride_length = mean(ride_length_m)) %>% 
  ggplot() + geom_col(mapping = aes(x = day_of_week, y = avg_ride_length, fill = member_casual), position = "dodge") + facet_wrap(~season) + scale_y_continuous(breaks = seq(0, 50, by = 10))

all_trips_v1%>%
  group_by(month, member_casual) %>%   
  summarise(number_of_rides = n()						 
            ,avg_ride_length = mean(ride_length_m)) %>% 
  ggplot() + geom_line(mapping = aes(x = month, y = number_of_rides, color = member_casual)) + scale_x_continuous(breaks = seq(1, 12, by = 1))