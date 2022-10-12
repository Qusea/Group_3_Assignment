install.packages("nycflights13")
library (tidyverse)
library(dplyr)
library(janitor)
library(DataExplorer)
library(nycflights13)
library(scales)
library(ggridges)


#loading the data
flight <- nycflights13::flights

#having a look at the data
glimpse(flight_df) # 19 variables, and 328521 observation
summary(flight_df)
?flights

#number of distinct origin, destination, and carriers
unique(flight$origin)
length(unique(flight$origin)) # 3 origin airport

unique(flight$dest)
length(unique(flight$dest)) # 105 destination airports 

unique(flight$carrier)
length(unique(flight$carrier)) #16 carrier

sum(is.na(flight$dep_time)) # number of canceled flights. 
sum(is.na(flight)) #number of missing values

flight %>% 
  drop_na(dep_time) -> flight


# number of missing values and their location in the data set after removing the canceled flights
sapply(flight, function(x) sum(is.na(x))) 
unique(flight$year)
unique(flight$month) #note the months are not ordered
unique(flight$day) 

#making the months in order. 
flight %>% 
  arrange(month) -> flight


unique(flight$origin)

#Delays count by month from all NYC airport.
flight %>% 
  filter(dep_delay > 0) %>% 
  group_by(month) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(month, count))+
  geom_point(shape = 4) +
  geom_line() +
  scale_x_continuous("Month", n.breaks = 12) +
  scale_y_continuous("Count of delays") +
  ggtitle("Delays count by month from all NYC airport.") +
  theme_classic()

#Delays count by month from EWR airport.
flight %>% 
  filter(origin == "EWR" & dep_delay > 0) %>% 
  group_by(month) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(month, count))+
  geom_point(shape = 4) +
  geom_line() +
  scale_x_continuous("Month", n.breaks = 12) +
  scale_y_continuous("Count of delays") +
  ggtitle("Delays count by month from EWR airport.") + 
  theme_classic()

#Delays count by month from LGA airport.
flight %>% 
  filter(origin == "LGA" & dep_delay > 0) %>% 
  group_by(month) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(month, count))+
  geom_point(shape = 4) +
  geom_line() +
  scale_x_continuous("Month", n.breaks = 12) +
  scale_y_continuous("Count of delays") +
  ggtitle("Delays count by month from LGA airport.") + 
  theme_classic()

#Delays count by month from JFK airport.
flight %>% 
  filter(origin == "JFK" & dep_delay > 0) %>% 
  group_by(month) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(month, count))+
  geom_point(shape = 4) +
  geom_line() +
  scale_x_continuous("Month", n.breaks = 12) +
  scale_y_continuous("Count of delays") +
  ggtitle("Delays count by month from JFK airport.") + 
  theme_classic()

#-----------
# distribution of delays 
ggplot(flight, aes(x = dep_delay)) +
  geom_histogram(bins = 80) +
  scale_x_continuous(limits = c(-100, 500)) # There is 60 more delay that are higher than 500 min

#---------
#Most delay to what destination
length(unique(flight$dest))
flight %>%
  filter(dep_delay > 0) %>% 
  group_by(dest) %>% 
  summarise(count=n()) %>% 
  arrange(-count)

#--------
#Average delay in each carrier
flight %>% 
  group_by(carrier) %>% 
  summarise(avg = mean(dep_delay), std = sd(dep_delay)) %>% 
  ggplot(aes(carrier, avg)) +
  geom_col() +
  #geom_errorbar(aes(ymin=avg-std, ymax=avg+std))+
  scale_y_continuous("Average delay in min") +
  scale_x_discrete("Carrier") +
  ggtitle("Average delay in minute for each carrier thorughout 2013") +
  theme_classic()
#-------

