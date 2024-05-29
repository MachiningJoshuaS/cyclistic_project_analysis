# Data Manipulation and Cleaning

# Setting up my environment
install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("readr")
install.packages("skimr")
install.packages("ggplot2")
library("tidyverse")
library("janitor")
library("lubridate")
library("readr")
library("skimr")
library("dplyr")
library("ggplot2")
````

#Loading in 12 months of data per the business task

march_2024 <- read.csv("original_copies/202403-divvy-tripdata.csv")
feb_2024 <- read.csv("original_copies/202402-divvy-tripdata.csv")
jan_2024 <- read.csv("original_copies/202401-divvy-tripdata.csv")
dec_2023 <- read.csv("original_copies/202312-divvy-tripdata.csv")
nov_2023 <- read.csv("original_copies/202311-divvy-tripdata.csv")
oct_2023 <- read.csv("original_copies/202310-divvy-tripdata.csv")
sept_2023 <- read.csv("original_copies/202309-divvy-tripdata.csv")
aug_2023 <- read.csv("original_copies/202308-divvy-tripdata.csv")
july_2023 <- read.csv("original_copies/202307-divvy-tripdata.csv")
june_2023 <- read.csv("original_copies/202306-divvy-tripdata.csv")
may_2023 <- read.csv("original_copies/202305-divvy-tripdata.csv")
april_2023 <- read.csv("original_copies/202304-divvy-tripdata.csv")


# Making sure all the column names and data types match.

colnames(march_2024)
colnames(feb_2024)
colnames(jan_2024)
colnames(dec_2023)
colnames(nov_2023)
colnames(oct_2023)
colnames(sept_2023)
colnames(aug_2023)
colnames(july_2023)
colnames(june_2023)
colnames(may_2023)
colnames(april_2023)

str(march_2024)
str(feb_2024)
str(jan_2024)
str(dec_2023)
str(nov_2023)
str(oct_2023)
str(sept_2023)
str(aug_2023)
str(july_2023)
str(june_2023)
str(may_2023)
str(april_2023)

compare_df_cols(march_2024, feb_2024, jan_2024, dec_2023, nov_2023, oct_2023, sept_2023, aug_2023, july_2023, june_2023, may_2023, april_2023, return = "mismatch", bind_method = "bind_rows")

# Combining all 12 data frames into one

all_trips <- bind_rows(march_2024, feb_2024, jan_2024, dec_2023, nov_2023, oct_2023, sept_2023, aug_2023, july_2023, june_2023, may_2023, april_2023)

# Checking out an overview of the complete data set. 5,750,177 rows!!!

str(all_trips)

skim_without_charts(all_trips)

# Importing from a csv, R classified my dates as a character string,
# which is not optimal for analysis. I'm just going to convert them
# into a data object.

all_trips$started_at <- ymd_hms(all_trips$started_at)
all_trips$ended_at <- ymd_hms(all_trips$ended_at)

skim_without_charts(all_trips)

# I'm going to create a column calculating the duration of each ride, and then I
# will also have a column showing which day of the week it is.

all_trips$trip_duration <- difftime(all_trips$ended_at, all_trips$started_at, units = "min")

all_trips$trip_duration <- as.numeric(as.character(all_trips$trip_duration))
all_trips$trip_duration <- round(all_trips$trip_duration, 2)

all_trips$day_of_week <-  weekdays(all_trips$started_at)
all_trips$month <- month(all_trips$started_at, label = TRUE)

# I want to make a back-up of my dataframe all_trips. I'm going to save it to
# a CSV, just in case something happens I can have access to it later.

write.csv(all_trips, "cyclistic_all_trips_v1.csv")

# Now that I have my data wrangled in and a back-up, I'm going to start
# Cleaning the data up a bit and starting an exploratory analysis.
# I'm going to check for duplicates on the ride_id, to make sure no trips have been duplicated.

sum(duplicated(all_trips$ride_id))

# I see some null values that seem to be centered in the geographic
# parts of the data. I'm going to see if any other columns have nulls.

null_counts <- colSums(is.na(all_trips))

columns_with_null <- names(null_counts[null_counts > 0])

print(columns_with_null)

# I know there are other columns with null values, so I'm going
# to make sure white space is trimmed.

char_cols <- sapply(all_trips, is.character)

all_trips[, char_cols] <- lapply(all_trips[, char_cols], function(x) ifelse(x == "", NA, x))

null_counts <- colSums(is.na(all_trips))

columns_with_null <- names(null_counts[null_counts > 0])

print(columns_with_null)


# I dont want to just dump any rows with null values. I fear that might skew my data
# especially in the rideable_type column.

rows_with_null <- which(rowSums(is.na(all_trips)) > 0)

categories_with_null <- all_trips$rideable_type[rows_with_null]

category_counts <- table(categories_with_null)

print(category_counts)

null_count <- sum(rowSums(is.na(all_trips)) > 0)

cat("Number of rows with at least one null value:", null_count, "\n")

# So in this situation I would ask Lily Moreno how she would like me to proceed.
# In this exercise that isn't an option. 99% of the null values fall in rows within
# the "electric_bike" rideable type. So I am going to remove those columns.
# As for columns with irrelevant information (ride_id, start_station_name, start_station_id,
# end_station_name, end_station_id, start_lat, start_lng, end_lat, end_lng),
# they are not pertinent to my business task. I am going to remove these columns as well

# putting the columns I'm removing into a different data frame and using subset to remove them from all_trips
removed_columns <- all_trips[, c("start_station_name", "start_station_id",
                                 "end_station_name", "end_station_id",
                                 "start_lat", "end_lat", "start_lng", "end_lng")]

all_trips <- subset(all_trips, select =
                      -c(start_station_name, start_station_id, end_station_name,
                         end_station_id, end_lat, end_lng, start_lat, start_lng))
 
# Going to remove rows with trip_duration less than or equal to 0.
all_trips <- all_trips[!(all_trips$trip_duration <= 0),]
 
# Backing up the cleaned data
 write.csv(all_trips, "cyclistic_all_trips_cleaned_v2.csv")
 
 ## Exploratory Data Analysis
 
 # Ride counts by user type and rideable type
 ride_counts <- all_trips %>%
   group_by(member_casual, rideable_type) %>%
   summarise(ride_count = n())
 print(ride_counts)
 
 # Plotting number of rides by rideable type and user type
 ggplot(ride_counts, aes(x = rideable_type, y = ride_count,
                         fill = member_casual)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Rideable Type", y = "Number of Rides", title = "Number of Rides by Rideable Type and Member Type") +
   theme_minimal()
 
 # Average ride duration by user type
 avg_duration_casual <- all_trips %>%
   filter(member_casual == "casual") %>%
   summarise(avg_ride_duration_casual = mean(trip_duration, na.rm = TRUE))
 
 avg_duration_member <- all_trips %>%
   filter(member_casual == "member") %>%
   summarise(avg_ride_duration_member = mean(trip_duration, na.rm = TRUE))
 
 print(avg_duration_casual)
 print(avg_duration_member)
 
 # Plotting average ride duration by user type
 avg_duration <- all_trips %>%
   group_by(member_casual) %>%
   summarise(avg_trip_duration = mean(trip_duration, na.rm = TRUE))
 
 ggplot(avg_duration, aes(x = member_casual, y = avg_trip_duration,
                          fill = member_casual)) +
   geom_bar(stat = "identity") +
   labs(x = "Rider Type", y = "Average Trip Duration", 
        title = "Average Trip Duration By Rider Type") +
   theme_minimal()
 
 # Total rides by day of the week
 all_trips$day_of_week <-
   factor(all_trips$day_of_week, levels = 
            c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
              "Saturday"))
 
 rides_per_day <- all_trips %>%
   group_by(member_casual, day_of_week) %>%
   summarise(num_rides = n())
 
 print(rides_per_day)
 
 # Plotting number of rides by day of the week
 ggplot(rides_per_day,
        aes(x = day_of_week, y = num_rides, fill = member_casual)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Day Of Week", y = "Number of Rides",
        title = "Number of Rides by Day of Week") +
   scale_x_discrete(labels =
                      c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                        "Friday", "Saturday")) +
   theme_minimal() +
   theme(axis.text.x =
           element_text(angle = 45, hjust = 1))
 
 # Total rides by month
 all_trips$month <-
   factor(all_trips$month, levels = 
            c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
 
 rides_per_month <- all_trips %>%
   group_by(member_casual, month) %>%
   summarise(num_rides = n())
 
 print(rides_per_month)
 
 # Plotting number of rides by month
 ggplot(rides_per_month,
        aes(x = month, y = num_rides, fill = member_casual)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Month", y = "Number of Rides",
        title = "Number of Rides by Month") +
   scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
   theme_minimal()
 
 # Average ride duration by day of the week
 avg_duration_by_day <- all_trips %>%
   group_by(day_of_week, member_casual) %>%
   summarise(avg_ride_duration = mean(trip_duration, na.rm = TRUE))
 
 print(avg_duration_by_day)
 
 # Plotting average ride duration by day of the week
 ggplot(avg_duration_by_day,
        aes(x = day_of_week, y = avg_ride_duration,
            fill = member_casual)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Day of Week", y = "Average Duration",
        title = "Average Trip Duration by Day") +
   scale_x_discrete(labels =
                      c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                        "Friday", "Saturday")) +
   theme_minimal() +
   theme(axis.text.x =
           element_text(angle = 45, hjust = 1))
 
 # Average ride duration by month
 avg_duration_by_month <- all_trips %>%
   group_by(month, member_casual) %>%
   summarise(avg_ride_duration = 
               mean(trip_duration, na.rm = TRUE))
 
 print(avg_duration_by_month)
 
 # Plotting average ride duration by month
 ggplot(avg_duration_by_month,
        aes(x = month, y = avg_ride_duration,
            fill = member_casual)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Month", y = "Average Duration", 
        title = "Average Trip Duration by Month") +
   theme_minimal()
 
 # Ride frequency by time of day
 all_trips <- all_trips %>%
   mutate(time_interval = case_when(
     hour(started_at) >= 5 & hour(started_at) < 12 ~ "Morning",
     hour(started_at) >= 12 & hour(started_at) <= 17 ~ "Afternoon",
     TRUE ~ "Night"
   ))
 
 rides_time_of_day <- all_trips %>%
   group_by(member_casual, time_interval) %>%
   summarise(num_rides = n(), .groups = 'drop')
 
 all_trips$time_interval <- factor(all_trips$time_interval, levels = c("Morning", "Afternoon", "Night"))
 
 # Plotting ride frequency by time of day
 ggplot(rides_time_of_day,
        aes(x = time_interval, y = num_rides,
            fill = member_casual)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Time Interval", y = "Number of Rides",
        title = "Ride Frequency by Time of Day") +
   theme_minimal()

# Ride Duration Distribution
 ggplot(all_trips, aes(x = trip_duration)) +
   geom_histogram(binwidth = 5, fill = "blue", color = "black") +
   labs(x = "Trip Duration (minutes)", y = "Frequency", title = "Distribution of Trip Durations") +
   theme_minimal() +
   xlim(0, 100)
 