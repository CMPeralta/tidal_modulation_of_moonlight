#!/usr/bin/env Rscript

library(tidyverse)
library(data.table)
library(xts)
library(cowplot)

## Figure1a - DINARD UNDERWATER

dinard_form <- as_tibble(read.csv("01_data/dinard_light_data_formatted.csv", row.names = NULL))
dinard_form <- dinard_form %>%
    mutate(time_date_UTC = as.POSIXct(dinard_form$time_date_UTC, format="%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    select(time_date_UTC, everything(), 
            -c(X,time_UTC, date_UTC))

##convert to datatable
dinard_form_dt <- data.table(dinard_form)

##############################PREP LIGHT DATA##############################
##CALCULATE AVERAGE 3O MIN 

##convert your data.table to an xts object
x <- as.xts(dinard_form_dt[,time_date_UTC := as.POSIXct(time_date_UTC, tz = "UTC")])
##calculate means per half an hour 
dinard_form_avg <- period.apply(x, endpoints(x, "minutes", 30), mean)
##separate date and time - rownames are date, that needs to be converted
dinard_form_avg_ed <- data.frame(time_date_UTC=index(dinard_form_avg), coredata(dinard_form_avg))
#removeX
names(dinard_form_avg_ed) <- sub("^X", "", names(dinard_form_avg_ed))

dinard_form_avg_ed <- as_tibble(dinard_form_avg_ed)

#get date and time, add time as factor and order to be able to filter time later
dinard_form_avg_ed <- dinard_form_avg_ed %>%
    mutate(date = as.Date(time_date_UTC, tz= "UTC"), 
            time = factor(format(as.POSIXct(time_date_UTC), format = "%H:%M"),ordered = TRUE)) %>% 
    select(date, time, everything()) 

#sum up all wavelenghts and bin by each 100nm 
dinard_form_avg_wave <- dinard_form_avg_ed %>%
    mutate(nm_all_wave = rowSums(across(6:197), na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(nm_300 = rowSums(select(., starts_with('3')))) %>%
    mutate(nm_400 = rowSums(select(., starts_with('4')))) %>%
    mutate(nm_500 = rowSums(select(., starts_with('5')))) %>%
    mutate(nm_600 = rowSums(select(., starts_with('6')))) %>%
    mutate(nm_700 = rowSums(select(., starts_with('7')))) %>%
    mutate(nm_800 = rowSums(select(., starts_with('8')))) %>%
    mutate(nm_900 = rowSums(select(., starts_with('9')))) %>%
    select(-c(6:197)) %>%
    mutate(nm_400_500_600 = rowSums(across(8:10), na.rm = TRUE))

#transform negative values to 0 for heatmap 
cols <- c("nm_900", "nm_800", "nm_700", "nm_600", "nm_500", "nm_400", "nm_300", "nm_400_500_600", "nm_all_wave")
dinard_form_avg_wave_zeros <- dinard_form_avg_wave

for (col in cols) {
  dinard_form_avg_wave_zeros[[col]][dinard_form_avg_wave_zeros[[col]] < 0] <- 0}

#define factors to set order in the heatmap 
dinard_form_avg_wave_zeros$time2 <- factor(dinard_form_avg_wave_zeros$time, 
    levels = c("12:25","12:55","13:25","13:55","14:25","14:55","15:25","15:55","16:25","16:55","17:25","17:55","18:25",
    "18:55","19:25","19:55","20:25","20:55","21:25","21:55","22:25","22:55", "23:25","23:55",
    "00:25","00:55","01:25","01:55","02:25","02:55","03:25","03:55","04:25","04:55","05:25","05:55",
    "06:25","06:55","07:25","07:55","08:25","08:55","09:25","09:55","10:25","10:55","11:25","11:55"))

#filter for two moon cycles and filter for night timepoints 
dinard_form_avg_wave_zeros_NM_NM <- dinard_form_avg_wave_zeros %>%
    filter(time_date_UTC >= as.POSIXct("2013-11-03 12:00:13",tz = "UTC"),
            time_date_UTC < as.POSIXct("2014-01-02 12:25:13",tz = "UTC")) %>%
    mutate(nm_400_500_600_na = ifelse(hour(time_date_UTC) >= 6 & hour(time_date_UTC) <= 17 & 
                            (hour(time_date_UTC) > 6 | minute(time_date_UTC) > 25) &
                            (hour(time_date_UTC) < 17 | minute(time_date_UTC) < 55),
                            NA, nm_400_500_600)) 

###########################################################################

##############################PREP TIDAL DATA##############################
#for details on how to obtain ERA5 water level data and how the data was pre-processed (manually), see Peralta et al. 2025
tides_raw <- as.tibble(read.csv("10_pre_processed_data/tidal_data_water_level_ERA5_X_dinard_357.9712_Y_48.64014.csv")) 

#convert to posixct and remove unnecessary columns 
tides_ed <- tides_raw %>%
    mutate(date_UTC = as.POSIXct((DATE), format="%d/%m/%Y", tz = "UTC"), 
        time_date_UTC = as.POSIXct(paste(date_UTC, TIME_UTC), format="%Y-%m-%d %H:%M:%S",tz = "UTC"),
        time_UTC = format(as.POSIXct(time_date_UTC), format = "%H:%M")) %>%
    select(-n, -X, -DATE, -TIME_UTC) %>%
    relocate(DAY, date_UTC, time_UTC, time_date_UTC) %>%
    rename(day = DAY,
            tide = TIDE,
            water_level= WATER_LEVEL)

#get days accordingly to the dinard light dataset 
start_date <- 18-1

tides_ed_day <- tides_ed %>% 
  filter(time_date_UTC >= as.POSIXct("2013-11-03 12:00:13",tz = "UTC"),
        time_date_UTC < as.POSIXct("2014-01-02 11:55:13",tz = "UTC"))%>%
  group_by(date_UTC) %>% 
  mutate(day_daylight =cur_group_id() + start_date,
        day_moonlight = cur_group_id() + start_date) %>% 
    ungroup() %>% 
#day is for plotting moonlight (day starts at 12:00 so that middle of the night is in the middle of the plot)
  mutate(day_moonlight = ifelse(hour(time_date_UTC) < 12, day_daylight - 1, day_daylight)) %>% 
  select(day,time_date_UTC, day_daylight, day_moonlight, everything()) %>%
  select(-day)  %>%
  rename(day = day_moonlight)

#Round timepoints to match the light data from dinard 
#separate hours, min and sec 
tides_ed_day_bin <- tides_ed_day %>%
    separate(time_UTC, into = c("hour", "minutes")) %>%
    group_by(date_UTC, hour) %>%
    mutate(new_min = ifelse((minutes > 25 & minutes <= 55), 55, ifelse(minutes < 25, 25, 25)), 
            hour = as.numeric(hour), 
            new_hour = ifelse(minutes > 55, hour+1, hour),
            new_hour = sprintf('%02d', as.numeric(new_hour)), 
            time = str_remove_all(paste0(new_hour, ":", new_min), " ")) %>%
    ungroup() %>% 
    select(-c(new_min,new_hour, hour, minutes))

tides_ed_day_bin$time_fct <- factor(tides_ed_day_bin$time, levels = c("12:25","12:55","13:25","13:55",
"14:25","14:55","15:25","15:55","16:25","16:55","17:25","17:55","18:25",
"18:55","19:25","19:55","20:25","20:55","21:25","21:55","22:25","22:55", 
"23:25","23:55","00:25","00:55","01:25","01:55","02:25","02:55","03:25",
"03:55","04:25","04:55","05:25","05:55","06:25","06:55","07:25","07:55",
"08:25","08:55","09:25","09:55","10:25","10:55","11:25","11:55"))

#filter HT and LT for ploting 
tides_ed_day_bin_HT <- tides_ed_day_bin[tides_ed_day_bin$tide == "HT",]
tides_ed_day_bin_LT <- tides_ed_day_bin[tides_ed_day_bin$tide == "LT",]
###########################################################################

#theme for plotting 
theme_1A <-   theme_bw() + 
                theme(
                legend.position = "bottom", legend.title = element_text(margin = margin(r = 10)),
                plot.title = element_text(hjust = 0.5, size = 13),
                axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                #axis.ticks.length=unit(.25, "cm"),
                axis.title.x = element_text(size = 12, margin = margin(t = 10)),
                axis.title.y = element_text(size = 12, margin = margin(r = 10)))

#plot  
p <- ggplot() +
geom_tile(data = dinard_form_avg_wave_zeros_NM_NM, aes(day,time2,fill=nm_400_500_600_na),color= NA,size=0,show.legend = TRUE) + 
scale_fill_gradient2(
            low = "black", 
            high = "darkgoldenrod1", 
            mid = "gainsboro", 
            midpoint = max(dinard_form_avg_wave_zeros_NM_NM$nm_400_500_600_na/2, na.rm = TRUE),
            na.value="white",
            name=bquote(atop("Night light",(mW/m^-2/nm^-1))), 
            limits=c(0,max(dinard_form_avg_wave_zeros_NM_NM$nm_400_500_600_na, na.rm = TRUE)), 
            breaks = c(0, 0.03, 0.06)) +
geom_point(data = tides_ed_day_bin_LT, size = 2, aes(x = day, y = time_fct, colour = "Low Tide"), shape = 25, fill = "#418fde", color = "grey") +
geom_point(data = tides_ed_day_bin_HT, size = 2, aes(x = day, y = time_fct, colour = "High Tide"), shape = 25, fill = "#DD312E", color = "grey") +
scale_x_continuous(expand = c(0.001,0.001), breaks = c(18,32,48,62,77)) +
scale_y_discrete(expand = c(0, 0), breaks = c("11:55", "06:25", "00:25", "17:55", "12:25"), labels = c("12:00", "06:00", "00:00", "18:00", "12:00")) + 
xlab("Day of measurement") + 
ylab("Time UTC [hh:mm]") +
guides(colour = guide_colourbar(title.vjust = 1.5)) + 
ggtitle("Dinard - underwater") + 
theme_1A 

#build a tiny legend with two triangles 
leg <- ggplot() +
  # low tide
  geom_point(aes(x = 0, y = 1), shape = 25, size = 2, colour = "grey", fill = "#418fde") +
  geom_text(aes(x = 0.18, y = 1, label = "Low Tide"), hjust = 0, vjust = 0.5) +
  # high tide
  geom_point(aes(x = 0, y = 0), shape = 25, size = 2, colour = "grey", fill = "#DD312E") +
  geom_text(aes(x = 0.18, y = 0, label = "High Tide"), hjust = 0, vjust = 0.5) +
  xlim(-0.05, 1.2) + ylim(-0.6, 1.6) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

final <- ggdraw() +
  draw_plot(p) +
  draw_plot(leg, x = 0.15, y = 0.01, width = 0.18, height = 0.22)  # tweak position/size as you like

#ggsave("02_visuals/Figure1a_nm_400_500_600_2cycles_NIGHT.pdf", width = 6, height = 4.5)
ggsave("02_visuals/Figure1a_nm_400_500_600_2cycles_NIGHT.png", width = 8, height = 5, units = "in", dpi = 300)
