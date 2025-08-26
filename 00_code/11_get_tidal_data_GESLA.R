#!/usr/bin/env Rscript 

library(tidyverse)
library(dplyr)
library(lubridate)
library(geslaR) #needs to be installed via CRAN, e.g. (1st istall arrow via conda, then in R: install.packages("geslaR", dependencies = TRUE, repos = "https://cloud.r-project.org")
library(oce)
library(readxl)
library(patchwork)


#IMPORTANT NOTE: 
## Figure S4 from Peralta et al. has tidal data extracted manually from maree.info. This script gets hourly observed sea levels from GESLA, timing of high and low tides is an approximation! 
## To get high/low tides, the data was fitted using a harmonic model (using oce::tidem)
## Predicted tides from GESLA occur ~5 minutes later on average (± 5 min SD) than maree.info and water level is ~0.09 m higher (± 0.13 m SD) on average

# ------------------------------------
# 1) get FRANCE observations (GESLA)

raw <- query_gesla(country = "FRA", year = 2013:2014, as_data_frame = TRUE)

#search geographical location  
#unique(raw$site_name[grepl("malo", raw$site_name, ignore.case = TRUE)])

sm_all <- raw %>%
  filter(site_name %in% c("Saint_Malo")) %>%
  transmute(site_name,
            datetime = as_datetime(date_time, tz = "Europe/Paris"),
            sea_level_m  = as.numeric(sea_level)) %>%
  arrange(site_name, datetime) %>%
  filter(!is.na(sea_level_m))

res_info <- sm_all %>%
  group_by(site_name) %>%
  summarize(dt = median(diff(datetime), na.rm = TRUE), .groups = "drop") %>%
  arrange(dt)

series_to_use <- res_info$site_name[1]  #pick smallest median timestep
message("Using GESLA series: ", series_to_use)

sm <- sm_all %>%
  filter(site_name == series_to_use) %>%
  arrange(datetime)

# ------------------------------------
# 2) harmonic fit + prediction (grid each 5 min)

start_local <- ymd_hms("2013-09-01 00:00:00", tz = "Europe/Paris")
end_local   <- ymd_hms("2014-02-01 00:00:00", tz = "Europe/Paris")

fit <- oce::tidem(t = sm$datetime, x = sm$sea_level_m, latitude = 48.65)
grid <- seq(from = start_local, to = end_local, by = "5 min")
pred <- oce::predict.tidem(fit, newdata = grid)

pred_df <- tibble(
  datetime = grid,
  level_m = as.numeric(pred))

# ------------------------------------
# 3) candidate highs/lows by neighbor test
cands <- pred_df %>%
mutate(
    prev_val = dplyr::lag(level_m),
    next_val = dplyr::lead(level_m),
    type = dplyr::case_when(
      !is.na(prev_val) & !is.na(next_val) & level_m > prev_val & level_m > next_val ~ "HIGH",
      !is.na(prev_val) & !is.na(next_val) & level_m < prev_val & level_m < next_val ~ "LOW",
      TRUE ~ NA_character_)) %>%
filter(!is.na(type)) %>%
select(datetime, type, level_m) %>%
arrange(datetime)

#enforce alternation + ≥ 3 h separation
min_sep_hours <- 3
keep <- logical(nrow(cands))
last_t <- as.POSIXct(NA, tz = "Europe/Paris")
last_type <- NA_character_

for (i in seq_len(nrow(cands))) {
  if (is.na(last_t)) {
    keep[i] <- TRUE
    last_t <- cands$datetime[i]
    last_type <- cands$type[i]} 
    else {
    dt_h <- as.numeric(difftime(cands$datetime[i], last_t, units = "hours"))
    if (dt_h >= min_sep_hours && cands$type[i] != last_type) {
      keep[i] <- TRUE
      last_t <- cands$datetime[i]
      last_type <- cands$type[i]}}}

tides <- cands[keep, ]

# 4) format and save
tz_local <- "Europe/Paris"
tides_out <- tides %>%
  mutate(
    datetime_local = with_tz(datetime, tz_local)) %>%
  transmute(datetime, type, water_level_m = level_m)

write.csv(tides_out, "01_data/tidal_data_GESLA_STmalo.csv")


