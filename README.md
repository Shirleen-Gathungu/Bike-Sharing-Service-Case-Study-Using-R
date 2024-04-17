
# Introduction

This analysis examines Divvy bike trip data from 2019 and 2020 to gain insights into rider behavior, preferences, and trends. The dataset includes information on ride duration, user type (annual member or casual rider), starting and ending stations, and timestamps.

## Data Sources
The analysis utilizes trip data from two quarters:
```
2019 Q1: Retrieved from Divvy_Trips_2019_Q1.csv
2020 Q1: Retrieved from Divvy_Trips_2020_Q1.csv
```

## Loading and Preprocessing Data

```
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
```
### Data Preparation
Renamed columns in the 2019 dataset to make them consistent with the 2020 dataset.
Converted ride_id and rideable_type to character format.
Combined both datasets into one.

## Analysis Highlights
### Ride Duration Analysis
Descriptive Statistics:
Summary statistics revealed the distribution and characteristics of ride durations.
Comparison:
Compared the average, median, maximum, and minimum ride durations for annual members and casual riders.

### Number of Rides by User Type and Weekday
Visualization:
Plotted the number of rides by rider type and weekday to identify usage patterns.
Showcased the difference in ride frequency between casual riders and annual members.

### Most Frequently Used Stations
Identification:
Identified the most frequently used starting and ending stations for casual riders and annual members.
Visualization:
Visualized station usage with bar plots, highlighting the top stations for each user type.

### Time Preference Analysis
Time of Day Preference:
Analyzed the preferred time of day for bike rides by annual members and casual riders.
Visualized time series plots showcasing ride patterns throughout the day.

### Day of the Week Preference:
Explored the preferred days of the week for bike rides by user type.
Presented day of the week preference using bar plots.
