library(nycflights13)
library(tidyverse)
library(janitor)

flights <- nycflights13::flights |>
    # removes the year variable because all rows have the same value
    remove_constant()

glimpse(flights)

unique(flights$year)
unique(flights$month)
unique(flights$origin)

# check for missing values
sum(is.na(flights$month))

for (col in colnames(flights)) {
    # print(col)
    print(glue::glue("{col} {sum(is.na(flights[, col]))}"))
}
    "output excluding zero
    dep_time 8255
    dep_delay 8255
    arr_time 8713
    arr_delay 9430
    tailnum 2512
    air_time 9430"

# rabbit hole
"
# check the dep_time - sched_dep_time = dep_delay
flights %>%
    select(dep_time, sched_dep_time, dep_delay) %>%
    # filter(!is.na(dep_delay)) %>%
    # filter(dep_delay >= 0) %>%
    mutate(delay_true = dep_time - sched_dep_time == dep_delay) %>%
    # filter(dep_delay != TRUE) %>%
    count(delay_true)

flights %>%
    select(dep_time, sched_dep_time, dep_delay) %>%
    filter(!is.na(dep_delay)) %>%
    mutate(calc_delay = dep_time - sched_dep_time) %>%
    mutate(calc_delay = ifelse(calc_delay < -40, (calc_delay + 40), calc_delay)) %>%
    mutate(calc_delay = ifelse(40 < calc_delay, (calc_delay - 40), calc_delay)) %>%
    mutate(delay_true = calc_delay == dep_delay) %>% print(n = 1000)
    # count(delay_true)


# check the arr_time - sched_arr_time = arr_delay
flights %>%
    select(arr_time, sched_arr_time, arr_delay) %>%
    # filter(!is.na(arr_delay)) %>%
    mutate(delay_true = arr_time - sched_arr_time == arr_delay) %>%
    count(delay_true)
"

# Check which airport has the most flights
flights %>%
    count(origin) %>%
    arrange(desc(n))

    "output
    origin      n
      <chr>   <int>
    1 EWR    120835
    2 JFK    111279
    3 LGA    104662"

# Check which airport has the most delays
flights %>%
    filter(dep_delay > 0) %>%
    count(origin) %>%
    arrange(desc(n))

    "output
      origin     n
      <chr>  <int>
    1 EWR    52711
    2 JFK    42031
    3 LGA    33690"

# Check which carrier has the most flights
flights %>%
    count(carrier) %>%
    arrange(desc(n))

    "output
       carrier     n
       <chr>   <int>
     1 UA      58665
     2 B6      54635
     3 EV      54173
     4 DL      48110
     5 AA      32729
     6 MQ      26397
     7 US      20536
     8 9E      18460
     9 WN      12275
    10 VX       5162
    11 FL       3260
    12 AS        714
    13 F9        685
    14 YV        601
    15 HA        342
    16 OO         32"

# Check which carrier has the most delays
flights %>%
    filter(dep_delay > 0) %>%
    count(carrier) %>%
    arrange(desc(n))

    "output
       carrier     n
       <chr>   <int>
     1 UA      27261
     2 EV      23139
     3 B6      21445
     4 DL      15241
     5 AA      10162
     6 MQ       8031
     7 9E       7063
     8 WN       6558
     9 US       4775
    10 VX       2225
    11 FL       1654
    12 F9        341
    13 YV        233
    14 AS        226
    15 HA         69
    16 OO          9"

# Check how diffrent planes (tailnum) are used
unique(flights$tailnum)

# Check which plane (tailnum) has the most flights
flights %>%
    filter(!is.na(tailnum)) %>%
    count(tailnum) %>%
    arrange(desc(n))



weather <- nycflights13::weather |>
    # removes the year variable because all rows have the same value
    remove_constant()
