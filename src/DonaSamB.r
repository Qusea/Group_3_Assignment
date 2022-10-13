library(tidyverse)
library(nycflights13)
install.packages("janitor")
library(janitor)

#removes year because its the same 
flights <- nycflights13::flights %>%
  remove_constant()
  

glimpse(flights)
names(flights)

#"month"          "day"            "dep_time"       "sched_dep_time" "dep_delay"     
#[6] "arr_time"       "sched_arr_time" "arr_delay"      "carrier"        "flight"        
#[11] "tailnum"        "origin"         "dest"           "air_time"       "distance"      
#[16] "hour"           "minute"         "time_hour"     



#checking delays per month

flights %>%
  filter(dep_delay > 0) %>%
   group_by(month) %>%
   count()

#nycflights13:: < to see diffrent tables 
nycflights13::
  
#not all carriers operate in all 3 airports. following code is to make sure one of those carriers dont.
flights %>%
  filter(carrier == "AS" ) %>%
  count(origin) 

  

#diff airlines in diff airport delay comp. code chunk 

flights %>%
  group_by(carrier) %>%
  #to find out how many airports each carrier operates in
  summarise(n_airports = length(unique(origin))) %>% 
  #removing carriers that don't operate in all 3 aiports
  filter(n_airports == 3) %>% 
  #adding that to an object
  pull(carrier) -> carriers_all_airports

flights %>% 
  filter(carrier %in% carriers_all_airports) -> flights_all_3

ggplot(data = flights_all_3, mapping = aes(x = carrier, y = dep_delay)) + geom_col()


