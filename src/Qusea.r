library(nycflights13)
library(tidyverse)
library(janitor)

flights <- nycflights13::flights %>%
    # removes the year variable because all rows have the same value
    remove_constant() %>%
    drop_na(dep_time)

# create new dataframe that only has delays
delayed_flights <- flights %>%
    filter(dep_delay > 0)

print(count(flights))
print(count(delayed_flights))
glimpse(flights)

unique(flights$year)
unique(flights$month)
unique(flights$origin)

# check for missing values
sum(is.na(flights$month))

for (col in colnames(flights)) {
    print(glue::glue("{col} {sum(is.na(flights[, col]))}"))
}

# Check which airport has the most flights
flights %>%
    count(origin) %>%
    arrange(desc(n))

# Check which airport has the most delays
flights %>%
    filter(dep_delay > 0) %>%
    count(origin) %>%
    arrange(desc(n))

# Check the proportion of flights that are delayed by airport
flights %>%
    group_by(origin) %>%
    summarise(
        flights = n(),
        delays = sum(dep_delay > 0),
        prop = sum(dep_delay > 0) / n()
    )



# Check which carrier has the most flights
flights %>%
    count(carrier) %>%
    arrange(desc(n))


# Check which carrier has the most delays
flights %>%
    filter(dep_delay > 0) %>%
    count(carrier) %>%
    arrange(desc(n))


# Check the proportion of flights that are delayed by carrier
flights %>%
    group_by(carrier) %>%
    summarise(
        flights = n(),
        delays = sum(dep_delay > 0),
        prop = sum(dep_delay > 0) / n()
    ) %>%
    arrange(desc(prop))

# it seems that the proportion of delays to flights
# increases with the number of flights
# for airports but not carriers

# Check how many diffrent planes (tailnum) are used
unique(flights$tailnum)
    # 4044 diffrent planes

# Check which plane (tailnum) has the most flights
flights %>%
    filter(!is.na(tailnum)) %>%
    count(tailnum) %>%
    arrange(desc(n))

# Check which plane (tailnum) has the most delays
flights %>%
    filter(!is.na(tailnum)) %>%
    filter(dep_delay > 0) %>%
    count(tailnum) %>%
    arrange(desc(n))

# Check which plane (tailnum) has the most delays relative to the number of flights
flights %>%
    filter(!is.na(tailnum)) %>%
    filter(dep_delay > 0) %>%
    count(tailnum) %>%
    mutate(prop = n / sum(n)) %>%
    arrange(desc(prop))

# Get the mean for dep_delay and arr_delay
flights %>%
filter(!is.na(arr_delay)) %>%
    summarise(
        mean_dep_delay = mean(dep_delay),
        mean_arr_delay = mean(arr_delay)
    )

remove_outliers <- function(df) {
  df %>%
    mutate_if(is.numeric, ~ ifelse(abs(.) > 2 * sd(.), NA, .)) %>%
    na.omit()
}

# Check how dep_delay effects arr_delay
# removing values 2x std away from the mean
flights %>%
    # filter(dep_delay > 0) %>%
    filter(!is.na(arr_delay)) %>%
    # filter(arr_delay > 0) %>%
    # filter(dep_delay < mean(dep_delay) + 2 * sd(dep_delay)) %>%
    # filter(arr_delay < mean(arr_delay) + 2 * sd(arr_delay)) %>%
    ggplot(aes(x = dep_delay, y = arr_delay)) +
    geom_point() +
    geom_smooth(method = "lm")


weather <- nycflights13::weather %>%
    # removes the year variable because all rows have the same value
    remove_constant()

glimpse(weather)
