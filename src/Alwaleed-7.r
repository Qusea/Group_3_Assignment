install.packages("nycflights13")
library(tidyverse)
library(nycflights13)
library(DataExplorer)
library(ggplot2)
nycflights13::flights -> flights
nycflights13::airlines -> airlines
nycflights13::weather -> weather
nycflights13::planes -> planes
nycflights13::airports -> airports


head(flights, 10)

# Automated EDA
DataExplorer::create_report(flights)

#Merging the flights and weather DataFrames
flights_weather <- merge(flights,weather,by=c("origin", "year", "month", "day", "time_hour", "hour"))

# Is there any relationship between weather and air_time?
# flights_weather %>%
# group_by(dest, origin) %>%
# select(weather, air_time, dest) %>%
# ggplot(aes(x = air_time, y = weather)) +
# geom_point() +
# facet_wrap(vars(dest), scales = "free")

# Is there any relationship between temp and air_time?
flights_weather %>%
  group_by(dest, origin) %>%
  select(temp, air_time, dest) %>%
  ggplot(aes(x = air_time, y = temp)) +
  geom_point() +
  facet_wrap(vars(dest), scales = "free")
# Merging the flights_EV and weather
flights_EV_weather <- merge(flights_EV,weather,by=c("origin", "year", "month", "day", "time_hour", "hour"))

# Is there any relationship between weather and air_time?
# flights_EV_weather %>%
#  group_by(dest, origin) %>%
#  select(temp, air_time, dest) %>%
#  ggplot(aes(x = air_time)) +
#  geom_histogram() +
#  facet_wrap(vars(dest), scales = "free")



# Filtering the flights to take only carrier EV
flights_EV <- flights %>%
  filter(carrier == "EV")
# Remove the missing values from arr_delay
flights_EV <- flights_EV %>% drop_na(arr_delay)

# Create the variable "season" for the 4 seasons of the year
flights_EV <- mutate(flights_EV, season = ifelse(month %in% 9:11, "fall",
                                                 ifelse(month %in% 3:5, "spring",
                                                        ifelse(month %in% 6:8, "summer","winter"))))

# Counting the number of flights for every season
flights_EV %>%
  group_by(season) %>%
  summarise(count = n())