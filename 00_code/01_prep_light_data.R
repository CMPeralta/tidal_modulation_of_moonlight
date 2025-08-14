#!/usr/bin/env Rscript

library(tidyverse)

dinard_light <- as_tibble(read.csv('01_data/249803', header = TRUE))

#create new date_time column,transform to UTC (CET - 2h)
dinard_light %>%
  mutate(time_date = as.POSIXct(paste(date, time), format="%Y-%m-%d %H:%M:%S", tz = "UTC"), #here UTC needs to be defined before (does not change anything, but allows for the filter to work afterwords)
         time_date_UTC = as.POSIXct(time_date) - (2*60*60),
         time_UTC = format(time_date_UTC, "%H:%M:%S"),
         date_UTC = format(time_date_UTC, "%Y-%m-%d")) %>% 
  select(-c(time_date, date, time)) %>% 
#filter out hours so that the dataset starts and ends at 12:00 (for plotting reasons)
  filter(time_date_UTC >= as.POSIXct("2013-10-17 12:00:13",tz = "UTC"),
        time_date_UTC < as.POSIXct("2014-02-01 11:59:13",tz = "UTC")) %>% 
#give days
  group_by(date_UTC) %>% 
  mutate(day = cur_group_id(), 
        day_daylight = cur_group_id()) %>% 
  ungroup() %>% 
#Day is for plotting moonlight (day starts at 12:00 so that middle of the night is in the middle of the plot)
  mutate(day = ifelse(hour(time_date_UTC) < 12, day - 1, day)) %>% 
  select(time_date_UTC, day, day_daylight, , time_UTC, date_UTC, everything()) %>% 
  write.csv("01_data/dinard_light_data_formatted.csv")