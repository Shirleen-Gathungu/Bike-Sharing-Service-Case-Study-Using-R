riders_data2019 <- read.csv('Divvy_Trips_2019_Q1.csv')
View(riders_data2019)

riders_data2020 <- read.csv('Divvy_Trips_2020_Q1.csv')
View(riders_data2020)


# making columns consistent by renaming column in 2019 dataset

(riders_data2019 <- rename(riders_data2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

str(riders_data2019)
str(riders_data2020)


# Convert ride_id and rideable_type to character

riders_data2019 <- mutate(riders_data2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame

all_trips <- bind_rows(riders_data2019, riders_data2020)
View(all_trips)


all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

table(all_trips$member_casual)


all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))


# consolidating labels in members_casual column from four to two labels.

all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# adding some additional columns of data -- such as day, month, year

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# adding a calculated field for length of ride
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Converting "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


#Removing bad data

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
missing_values <- sum(is.na(all_trips_v2))

# Print the total number of missing values
print(missing_values)

# Descriptive analysis on ride_length
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)



# 1. Average number of rides per month and week for each group
# analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() #calculates the number of rides and average duration
            ,average_duration = mean(ride_length)) %>% # calculates the average duration
arrange(member_casual, weekday)

#visualizing the number of rides by rider type

all_trips_v2 %>%
 mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(y = "Number of Rides") +
  scale_y_continuous(labels = scales::comma)


# Filter the dataset to include relevant columns
rides_data <- all_trips_v2 %>%
  select(started_at, member_casual)

# Convert started_at to Date format if it's not already
rides_data$started_at <- as.Date(rides_data$started_at)

# Group by member_casual and month
rides_summary <- rides_data %>%
  mutate(month = format(started_at, "%Y-%m")) %>%
  group_by(member_casual, month) %>%
  summarise(total_rides = n()) %>%
  ungroup()

# Calculate the average number of rides per month for each group
average_rides <- rides_summary %>%
  group_by(member_casual) %>%
  summarise(average_rides_per_month = mean(total_rides))

# Print the results
print(average_rides)

#visualizing the data to show average number of rides per month for each group
ggplot(rides_summary, aes(x = month, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Number of Rides per Month",
       x = "Month",
       y = "Total Rides") +
  theme_minimal()


# 2. Duration of bike rides taken by annual members and casual riders
# Calculate the duration of each ride
all_trips_v2 <- all_trips_v2 %>%
  mutate(started_at = as.POSIXct(started_at),
         ended_at = as.POSIXct(ended_at),
         ride_length = difftime(ended_at, started_at, units = "mins"))

# Filter relevant columns
duration_data <- all_trips_v2 %>%
  select(member_casual, ride_length)

# Calculate the average ride duration for each user type
average_duration <- duration_data %>%
  group_by(member_casual) %>%
  summarise(average_ride_duration = mean(ride_length, na.rm = TRUE))

# Create a bar plot to visualize the average ride duration for each user type
ggplot(average_duration, aes(x = member_casual, y = average_ride_duration, fill = member_casual)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("casual" = "skyblue", "member" = "red")) +  # Set fill colors
  labs(title = "Average Duration of Bike Rides by User Type",
       x = "User Type",
       y = "Average Ride Duration (minutes)") +
  theme_minimal()


# 3. Identify the most frequently used starting and ending stations for both annual members and casual riders

# Group by member type and start station name, and count the number of rides for each combination
start_station_counts <- all_trips_v2 %>%
  group_by(member_casual, start_station_name) %>%
  summarise(start_count = n()) %>%
  arrange(member_casual, desc(start_count)) %>%
  slice_head(n = 1)  # Select the top station for each user type

# Group by member type and end station name, and count the number of rides for each combination
end_station_counts <- all_trips_v2 %>%
  group_by(member_casual, end_station_name) %>%
  summarise(end_count = n()) %>%
  arrange(member_casual, desc(end_count)) %>%
  slice_head(n = 1)  # Select the top station for each user type

# Combine the start and end station counts
station_counts <- bind_rows(start_station_counts, end_station_counts)

# Print the results
print(station_counts)

#visualizing the most frequently used starting and ending stations

# Create a bar plot for the most frequently used starting stations
ggplot(station_counts, aes(x = start_station_name, y = start_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Most Frequently Used Starting Stations",
       x = "Station Name",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for the most frequently used ending stations
ggplot(station_counts, aes(x = end_station_name, y = end_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Most Frequently Used Ending Stations",
       x = "Station Name",
       y = "Number of Rides",
       fill = "User Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 5. Examining the time of day and day of the week when annual members and casual riders prefer to use the bikes.

# Time of Day Preference for Annual Members
annual_members_time <- all_trips_v2 %>%
  filter(member_casual == "member") %>%
  mutate(hour_of_day = lubridate::hour(started_at)) %>%
  group_by(hour_of_day) %>%
  summarise(total_trips = n())

# Time of Day Preference for Casual Riders
casual_riders_time <- all_trips_v2 %>%
  filter(member_casual == "casual") %>%
  mutate(hour_of_day = lubridate::hour(started_at)) %>%
  group_by(hour_of_day) %>%
  summarise(total_trips = n())

# Day of the Week Preference for Annual Members
annual_members_day <- all_trips_v2 %>%
  filter(member_casual == "member") %>%
  group_by(day_of_week) %>%
  summarise(total_trips = n())

# Day of the Week Preference for Casual Riders
casual_riders_day <- all_trips_v2 %>%
  filter(member_casual == "casual") %>%
  group_by(day_of_week) %>%
  summarise(total_trips = n())

#visualizing preferred time and day data for the two groups

# Time Series Plot for Annual Members (Red)
ggplot(annual_members_time, aes(x = hour_of_day, y = total_trips)) +
  geom_line(color = "red") +
  labs(title = "Time of Day Preference for Annual Members",
       x = "Hour of Day",
       y = "Total Trips")

# Time Series Plot for Casual Riders (Blue)
ggplot(casual_riders_time, aes(x = hour_of_day, y = total_trips)) +
  geom_line(color = "blue") +
  labs(title = "Time of Day Preference for Casual Riders",
       x = "Hour of Day",
       y = "Total Trips")

# Day of the Week Preference for Annual Members (Red)
ggplot(annual_members_day, aes(x = day_of_week, y = total_trips)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Day of the Week Preference for Annual Members",
       x = "Day of Week",
       y = "Total Trips") +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Day of the Week Preference for Casual Riders (Blue)
ggplot(casual_riders_day, aes(x = day_of_week, y = total_trips)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Day of the Week Preference for Casual Riders",
       x = "Day of Week",
       y = "Total Trips") +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
