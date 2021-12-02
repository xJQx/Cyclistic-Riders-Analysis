# libraries used
library(tidyverse)
library(lubridate)
library(ggplot2)

# Step 1: Collect Data
# Upload Divvy Datasets
q2_2019 <- read.csv("Dataset/Raw/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Dataset/Raw/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Dataset/Raw/Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Dataset/Raw/Divvy_Trips_2020_Q1.csv")

# Step 2: Wrangle data and combine into a single file
# compare column names of each dataset
colnames(q1_2020)
colnames(q4_2019)
colnames(q3_2019)
colnames(q2_2019)

# There is inconsistent column names across each dataset
# Rename column names to make it consistent with q1_2020
q2_2019 <- rename(q2_2019
                  ,ride_id = "X01...Rental.Details.Rental.ID"
                  ,rideable_type = "X01...Rental.Details.Bike.ID" 
                  ,started_at = "X01...Rental.Details.Local.Start.Time"  
                  ,ended_at = "X01...Rental.Details.Local.End.Time"  
                  ,start_station_name = "X03...Rental.Start.Station.Name" 
                  ,start_station_id = "X03...Rental.Start.Station.ID"
                  ,end_station_name = "X02...Rental.End.Station.Name" 
                  ,end_station_id = "X02...Rental.End.Station.ID"
                  ,member_casual = "User.Type"
                  ,tripduration = "X01...Rental.Details.Duration.In.Seconds.Uncapped"
                  ,gender = "Member.Gender"
                  ,birthyear = "X05...Member.Details.Member.Birthday.Year")
q3_2019 <- rename(q3_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)
q4_2019 <- rename(q4_2019
                  ,ride_id = trip_id
                  ,rideable_type = bikeid 
                  ,started_at = start_time  
                  ,ended_at = end_time  
                  ,start_station_name = from_station_name 
                  ,start_station_id = from_station_id 
                  ,end_station_name = to_station_name 
                  ,end_station_id = to_station_id 
                  ,member_casual = usertype)

# compare column names of each dataset again
# column names of are consistent now
colnames(q1_2020)
colnames(q4_2019)
colnames(q3_2019)
colnames(q2_2019)

# Inspect the data sets and look for inconsistency
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character
# so that they are consistent across data sets
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

# combine individual quarter's data sets into one big data set
divvy_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)
head(divvy_trips)

# remove birthyear, gender and tripduration
# as this data was not included in q1 2020
# remove lat and long
# as it was only included in q1 2020
divvy_trips <- divvy_trips %>%
  select(-c(birthyear, gender, tripduration
            , start_lat, start_lng, end_lat, end_lng))

# Step 3: Clean up and Add Data to prepare for analysis
# Inspect the new table that has been created
# column names
colnames(divvy_trips)
# number of rows
nrow(divvy_trips)
# dimension of data frame
dim(divvy_trips)
# first/last 6 rows of data fram
head(divvy_trips)
tail(divvy_trips)
#See list of columns and data types (numeric, character, etc)
str(divvy_trips)

# There are a few problems we will need to fix:

# (1) In the "member_casual" column, 
# there are two names for members ("member" and "Subscriber") 
# and two names for casual riders ("Customer" and "casual"). 
# We will need to consolidate that from four to two labels

# Begin by seeing how many observations fall under each usertype
table(divvy_trips$member_casual)

# In the "member_casual" column, 
# replace "Subscriber" with "member" and "Customer" with "casual"

# Reassign member_casual to the desired values
divvy_trips <- divvy_trips %>%
  mutate(member_casual = recode(member_casual
                                , "Subscriber" =  "member"
                                , "Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(divvy_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year
# Date in yyyy-mm-dd
divvy_trips$date <- as.Date(divvy_trips$started_at)
# year, month, day, day of week
divvy_trips$year <- format(as.Date(divvy_trips$date), "%Y")
divvy_trips$month <- format(as.Date(divvy_trips$date), "%m")
divvy_trips$day <- format(as.Date(divvy_trips$date), "%d")
divvy_trips$day_of_week <- format(as.Date(divvy_trips$date), "%A")

# order day_of_week
divvy_trips$day_of_week <- ordered(divvy_trips$day_of_week
                                   , levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Add "ride_duration" column for duration of the rides
divvy_trips$ride_duration = difftime(divvy_trips$ended_at, divvy_trips$started_at)
# Inspect structure of "ride_duration" column
# "ride_duration" is in secs
str(divvy_trips$ride_duration)
# Convert "ride_duration" to numeric
# so that calculation can be performed on it
divvy_trips$ride_duration <- as.numeric(divvy_trips$ride_duration)
is.numeric(divvy_trips$ride_duration)

# Clean the "bad" data
# number of negative ride duration
length(divvy_trips$ride_id[divvy_trips$ride_duration < 0])
# number of bikes taken out of docks and checked for quality by Divvy
length(divvy_trips$ride_id[divvy_trips$start_station_name == "HQ QR"])
# Remove rows with bikes taken out of docks and negative ride duration
divvy_trips_cleaned <- divvy_trips[!(divvy_trips$start_station_name == "HQ QR"
                                     | divvy_trips$ride_duration < 0), ]

# data ready for analysis
head(divvy_trips_cleaned)
dim(divvy_trips_cleaned)

# Step 4: Conduct Descriptive Analysis
# statistical summary on "ride_duration" (secs)
summary(divvy_trips_cleaned$ride_duration)

# compare members and casual users' ride duration (secs)
setNames(aggregate(divvy_trips_cleaned$ride_duration ~ divvy_trips_cleaned$member_casual
                   , FUN = mean), c("member/casual", "ride_duration_mean"))
setNames(aggregate(divvy_trips_cleaned$ride_duration ~ divvy_trips_cleaned$member_casual
                   , FUN = median), c("member/casual", "ride_duration_median"))
setNames(aggregate(divvy_trips_cleaned$ride_duration ~ divvy_trips_cleaned$member_casual
                   , FUN = max), c("member/casual", "ride_duration_max"))
setNames(aggregate(divvy_trips_cleaned$ride_duration ~ divvy_trips_cleaned$member_casual
                   , FUN = min), c("member/casual", "ride_duration_min"))

# res1: compare average number of rides and average ride duration between casual and member riders
res1 <- divvy_trips_cleaned %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n()
            , mean_ride_duration = mean(ride_duration))
res1 %>%
  ggplot(aes(x = member_casual, y = number_of_rides)) +
  geom_col(aes(fill = number_of_rides)) +
  scale_fill_gradient2(low = "orange", 
                       high = "darkblue", 
                       midpoint = median(res1$number_of_rides)) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Number of Rides") +
  labs(title = "Number of Rides", subtitle = "Casual vs Member")

res1 %>%
  ggplot(aes(x = member_casual, y = mean_ride_duration)) +
  geom_col(aes(fill = mean_ride_duration)) +
  scale_fill_gradient2(low = "darkblue", 
                       high = "orange", 
                       midpoint = median(res1$mean_ride_duration)) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Average Ride Duration") +
  labs(title = "Average Ride Duration", subtitle = "Casual vs Member")

# See the average ride duration by each day for members vs casual users
setNames(aggregate(divvy_trips_cleaned$ride_duration
                   ~ divvy_trips_cleaned$member_casual
                   + divvy_trips_cleaned$day_of_week
                   , FUN = mean)
         , c("member/casual", "day_of_week", "ride_duration_mean"))


# res2: analyse ridership data by type and weekday
res2 <- divvy_trips_cleaned %>%
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n()
            , mean_ride_duration = mean(ride_duration)) %>%
  arrange(member_casual, day_of_week)
# visualize res2 as mean_ride_duration
res2 %>%
  ggplot(aes(x = day_of_week, y = mean_ride_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  xlab("") +
  ylab("Average Ride Duration") +
  labs(title = "Average Ride Duration Throughout the Week"
       , subtitle = "Casual vs Member")

# visualize res2 as number_of_rides
res2 %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  xlab("") +
  ylab("Number of Rides") +
  labs(title = "Number of Rides Throughout the Week"
       , subtitle = "Casual vs Member")

# visualize trend of number of rides of casual riders throughout the week
res2 %>%
  filter(member_casual == "casual") %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, group = 1)) +
  geom_point(color = "orange") +
  geom_line(color = "orange") +
  xlab("") +
  ylab("Number of Rides") +
  labs(title = "Number of Rides of Casual Riders") 


# visualize trend of number of rides of members throughout the week
res2 %>%
  filter(member_casual == "member") %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, group = 1)) +
  geom_point(color = "darkblue") +
  geom_line(color = "darkblue") +
  xlab("") +
  ylab("Number of Rides") +
  labs(title = "Number of Rides of Members")

# res3: analyse ridership data by type and month
res3 <- divvy_trips_cleaned %>%
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n()
            , mean_ride_duration = mean(ride_duration)) %>%
  arrange(member_casual, month)

# visualize res3 as mean_ride_duration of casual riders and members
res3 %>%
  ggplot(aes(x = month, y = mean_ride_duration, color = member_casual)) +
  geom_col() +
  xlab("Months") +
  ylab("Average Ride Duration") +
  labs(title = "Average Ride Duration (Months)")
res3 %>%
  filter(member_casual == "casual") %>%
  ggplot(aes(x = month, y = mean_ride_duration)) +
  geom_col(fill = "orange") +
  xlab("Months") +
  ylab("Average Ride Duration") +
  labs(title = "Average Ride Duration of Casual Riders (Months)")
res3 %>%
  filter(member_casual == "member") %>%
  ggplot(aes(x = month, y = mean_ride_duration)) +
  geom_col(fill = "darkblue") +
  xlab("Months") +
  ylab("Average Ride Duration") +
  labs(title = "Average Ride Duration of Members (Months)")

# export divvy_trips_cleaned and res as excel for further analysis and visualisation
write.csv(divvy_trips_cleaned, 'Dataset/divvy_trips_cleaned.csv')
write.csv(res1, 'Dataset/res1.csv')
write.csv(res2, 'Dataset/res2.csv')
write.csv(res3, 'Dataset/res3.csv')

