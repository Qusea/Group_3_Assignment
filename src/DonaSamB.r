library(tidyverse)
library(nycflights13)
install.packages("janitor")
library(janitor)

flights <- nycflights13::flights %>%
  #removes year because its the same 
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
nycflights13::airlines
  
  
 # mother code 
flights %>%
  filter(dep_delay > 0) %>%
  count(origin) %>%
  arrange(desc(n))

#diff airlines in diff airport delay comp.

flights %>%
  filter(dep_delay > 0) %>%
  group_by(carrier)%>%
  #count(origin) %>%
  #arrange(carrier) %>%
  #remove he carriers that don't operate in all airports (manually) 
  filter(carrier != "AS" || carrier != "F9" || carrier != "FL" || carrier != "HA" || 
           carrier != "OO" || carrier != "VX" || carrier != "WN" || carrier != "YV") %>%
  unique(carrier)


#not all carriers operate in all 3 airports. following code is to make sure one of those carriers dont.
flights %>%
  filter(carrier == "AS" ) %>%
  count(origin) 


#remove the carriers that don't operate in all airports in one function tmw



