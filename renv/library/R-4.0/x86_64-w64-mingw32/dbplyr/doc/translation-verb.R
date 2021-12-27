## ---- message = FALSE---------------------------------------------------------
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
flights <- copy_to(con, nycflights13::flights)
airports <- copy_to(con, nycflights13::airports)

## -----------------------------------------------------------------------------
flights %>%
  select(contains("delay")) %>%
  show_query()

flights %>%
  select(distance, air_time) %>%  
  mutate(speed = distance / (air_time / 60)) %>%
  show_query()

## -----------------------------------------------------------------------------
flights %>% 
  filter(month == 1, day == 1) %>%
  show_query()

## -----------------------------------------------------------------------------
flights %>% 
  arrange(carrier, desc(arr_delay)) %>%
  show_query()

## -----------------------------------------------------------------------------
flights %>%
  group_by(month, day) %>%
  summarise(delay = mean(dep_delay)) %>%
  show_query()

