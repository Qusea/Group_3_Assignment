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
# flights_weather %>%
#  group_by(dest, origin) %>%
#  select(temp, dep_delay, dest) %>%
#  ggplot(aes(x = dep_delay, y = dest, color = temp)) +
#  geom_point(position = position_jitter(height = 0.25)) +
#  facet_grid(cols = vars(origin))

# Merging the flights_EV and weather
flights_EV_weather <- merge(flights_EV,weather,by=c("origin", "year", "month", "day", "time_hour", "hour"))

# Is there any relationship between weather and air_time?
# flights_EV_weather %>%
#  group_by(dest, origin) %>%
#  select(temp, air_time, dest) %>%
#  ggplot(aes(x = air_time)) +
#  geom_histogram() +
#  facet_wrap(vars(dest), scales = "free")



# Create the variable "season" for the 4 seasons of the year
flights_EV <- mutate(flights_EV, season = ifelse(month %in% 9:11, "fall",
                                                 ifelse(month %in% 3:5, "spring",
                                                        ifelse(month %in% 6:8, "summer","winter"))))



# Counting the number of flights for every season
flights_EV %>%
  group_by(season) %>%
  summarise(count = n())



# Calculating average departure delay per season for every origin
flights_EV_weather %>%
  group_by(season, origin) %>%
  summarise(mean(dep_delay))

# Calculating average departure delay per season for every origin and destination
avg_dep_delay <- flights_EV_weather %>%
  group_by(season, origin, dest) %>%
  summarise(mean(dep_delay))

# Is there a relationship between departure delay and the season?
flights_EV_weather <- mutate(flights_EV_weather, season = ifelse(month %in% 9:11, "fall",
                                                 ifelse(month %in% 3:5, "spring",
                                                        ifelse(month %in% 6:8, "summer","winter"))))


# ----
# Randomly choosing sample of 50 points to see how the delays per origin look for EV carrier
 slice_flights_EV_weather <- flights_EV_weather %>%
  group_by(origin) %>%
  slice_sample(n = 50)

 slice_flights_EV_weather %>%
  group_by(dest, origin) %>%
  select(season, dep_delay, dest) %>%
  ggplot(aes(x = dep_delay, y = dest, color = season)) +
  geom_point(position = position_jitter(height = 0.25)) +
  facet_grid(cols = vars(origin))


# Filtering the flights to take only carrier EV
flights_EV <- flights %>%
  filter(carrier == "EV")
# Remove the missing values from arr_delay
flights_EV <- flights_EV %>% drop_na(arr_delay)

# Create the variable "season" for the 4 seasons of the year
flights_EV <- mutate(flights_EV, season = ifelse(month %in% 9:11, "fall",
                                                 ifelse(month %in% 3:5, "spring",
                                                        ifelse(month %in% 6:8, "summer","winter"))))



# Plotting the average departure delay vs destination
# dest_10 %>%
#  group_by(dest, origin) %>%
#  select(season, `mean(dep_delay)`, dest) %>%
#  ggplot(aes(x = 'mean(dep_delay)', y = dest, color = season)) +
#  geom_point(position = position_jitter(height = 0.25)) +
#  facet_grid(cols = vars(origin))

# ----
# Distribution of Departure Delays per season
ggplot(data = flights_EV, aes(x=season, y=dep_delay))+
  geom_boxplot(alpha = 0.2) + ylim(-25,50)+
  labs(title = "Distribution of Departure Delays per Year", y = "Departure Delay", x = "Seasons of a Year")