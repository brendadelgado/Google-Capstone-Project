# Install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

#Load packages
library(tidyverse) 
library(lubridate) 
library(ggplot2)

# Collect Data 
q2_2019 <-read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <-read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <-read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <-read.csv("Divvy_Trips_2020_Q1.csv")

colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

# Rename columns to make them consistent with q1_2020
(q4_2019 <- rename(q4_2019, ride_id = trip_id, rideable_type = bikeid, started_at = start_time, ended_at = end_time, start_station_name = from_station_name, start_station_id = from_station_id, end_station_name = to_station_name, end_station_id = to_station_id, member_casual = usertype ))
(q3_2019 <- rename(q3_2019, ride_id = trip_id, rideable_type = bikeid, started_at = start_time, ended_at = end_time, start_station_name = from_station_name, start_station_id = from_station_id, end_station_name = to_station_name, end_station_id = to_station_id, member_casual = usertype ))
(q2_2019 <- rename(q2_2019, ride_id = Rental_Details_Rental_ID, rideable_type = Rental_Details_Bike_ID, started_at = Rental_Details_Local_Start_Time , ended_at = Rental_Details_Local_End_Time, start_station_name = Rental_Start_Station_Name, start_station_id = Rental_Start_Station_ID, end_station_name = Rental_End_Station_Name, end_station_id = Rental_End_Station_ID, member_casual = User_Type ))

# Inspect the data frames and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

#Convert
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, Rental_Details_Duration_In_Seconds_Uncapped, Member_Details_Member_Birthday_Year, Member_Gender, tripduration))

#CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
colnames(all_trips)
nrow(all_trips)
dim(all_trips) 
head(all_trips)
str(all_trips)
summary(all_trips)

#FIX PROBLEMS
table(all_trips$member_casual)

# Reassign to the desired values 
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual ,"Subscriber" = "member","Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>%  mutate(weekday = wday(started_at, label = TRUE)) %>%   group_by(member_casual, weekday) %>%    summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%   arrange(member_casual, weekday) 

# visualize the number of rides by rider type
all_trips_v2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>%  group_by(member_casual, weekday) %>%  summarise(number_of_rides = n()  ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>%  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")

# visualization for average duration
all_trips_v2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% group_by(member_casual, weekday) %>% summarise(number_of_rides = n()  ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge")

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
