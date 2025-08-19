#!/usr/bin/env Rscript 

library(tidyverse)
library(data.table)
library(xts)
library(ggpubr)

#FIGURE S3 - ABSOLUTE AND RELATIVE MOONLIGHT and DAYLIGHT OVER FOUR CONSECUTIVE MONTHS 

dinard_form <- as_tibble(read.csv("01_data/dinard_light_data_formatted.csv", row.names = NULL))
dinard_form <- dinard_form %>%
    mutate(time_date_UTC = as.POSIXct(dinard_form$time_date_UTC, format="%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    select(time_date_UTC, everything(), 
            -c(X)) %>%
    rename_with(~ sub("^X", "", .x), starts_with("X"))


##PREP data for FIGURE S3ab

###filter to get night data 
dinard_form_night0 <- dinard_form[dinard_form$time_UTC >= "18:00:13" & dinard_form$time_UTC <= "23:55:13",]
dinard_form_night1 <- dinard_form[dinard_form$time_UTC >= "00:00:13" & dinard_form$time_UTC <= "05:55:13",]
dinard_form_night2 <- rbind(dinard_form_night0,dinard_form_night1)
dinard_form_night <- dinard_form_night2[order(dinard_form_night2$time_date_UTC),]

###average night data - 30 min 
dinard_form_night_dt <- data.table(dinard_form_night)
x <- as.xts(dinard_form_night_dt[,time_date_UTC := as.POSIXct(time_date_UTC, tz = "UTC")])
dinard_form_night_av0 <- period.apply(x, endpoints(x, "minutes", 30), mean)
dinard_form_night_av <- data.frame(time_date_UTC=index(dinard_form_night_av0), coredata(dinard_form_night_av0))
names(dinard_form_night_av) <- sub("^X", "", names(dinard_form_night_av))

####get date and time, add time as factor and order to be able to filter time later
dinard_form_night_av_ed <- dinard_form_night_av %>%
    mutate(date = as.Date(time_date_UTC, tz= "UTC"), 
            time = factor(format(as.POSIXct(time_date_UTC), format = "%H:%M"),ordered = TRUE)) %>% 
    select(date, time, everything()) 
dinard_form_night_av_ed <- as.tibble(dinard_form_night_av_ed)

###sum up all wavelenghts and bin by each 100nm 
dinard_form_night_av_ed <- dinard_form_night_av_ed %>%
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

###transform negative values to 0 for heatmap 
cols <- c("nm_900", "nm_800", "nm_700", "nm_600", "nm_500", "nm_400", "nm_300", "nm_400_500_600", "nm_all_wave")
dinard_form_night_av_zeros <- dinard_form_night_av_ed

for (col in cols) {
  dinard_form_night_av_zeros[[col]][dinard_form_night_av_zeros[[col]] < 0] <- 0}

###themes for all plots
theme_4_bottom1_AC <- theme(
    plot.title = element_text(hjust = 0.5, size = 25), 
    panel.grid = element_blank(),
    axis.text.x = element_text(angle=0, vjust=1, hjust = 0.5, size = 25), 
    axis.title.x =element_blank(), 
    axis.title.y =element_blank(),
    strip.text.x = element_blank(),
    axis.text.y = element_text(size = 25), 
    axis.ticks.length=unit(.25, "cm"),
    legend.key.size = unit(1.5, 'cm'),
    legend.text = element_text(size=18),
    legend.title = element_text(size=18),
    plot.margin = margin(0.5,1,0.5,0.5, "cm"))
  

##FIGURE S3a - MOONLIGHT - Absolute light intensity 

###define factors to set order in the heatmap 
dinard_form_night_av_zeros$time2 <- factor(dinard_form_night_av_zeros$time, levels = c("18:25","18:55","19:25","19:55","20:25","20:55","21:25","21:55","22:25","22:55", "23:25","23:55","00:25","00:55","01:25","01:55","02:25","02:55","03:25","03:55","04:25","04:55","05:25","05:55"))
dinard_form_night_av_zeros <- dinard_form_night_av_zeros %>%
  select(time2, everything()) 

###plot all wavelenghts  
heat_300_night_FM  <- ggplot(dinard_form_night_av_zeros,aes(day,time2,fill=nm_300)) +
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.02,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,0.04)) +
  geom_tile(color= NA,size=0, show.legend = FALSE) + 
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("18:25", "21:25", "00:25","03:25","05:55"), labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) + 
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) + 
  theme_4_bottom1_AC +
  labs(title="300-399 nm")

heat_400_night_FM  <- ggplot(dinard_form_night_av_zeros,aes(day,time2,fill=nm_400)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.02,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,0.04)) +
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("18:25", "21:25", "00:25","03:25","05:55"), labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) + 
  theme_4_bottom1_AC +
  labs(title="400-499 nm")

heat_500_night_FM  <- ggplot(dinard_form_night_av_zeros,aes(day,time2,fill=nm_500)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.02,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,0.04)) +
  scale_y_discrete(expand = c(0.0, 0.0),breaks = c("18:25", "21:25", "00:25","03:25","05:55"), labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +  
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) + 
  theme_4_bottom1_AC + 
  labs(title="500-599 nm")

heat_600_night_FM  <- ggplot(dinard_form_night_av_zeros,aes(day,time2,fill=nm_600)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.02,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,0.04)) +
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("18:25", "21:25", "00:25","03:25","05:55"), labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +  
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) + 
  theme_4_bottom1_AC +
  labs(title="600-699 nm")

heat_700_night_FM  <- ggplot(dinard_form_night_av_zeros,aes(day,time2,fill=nm_700)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.02,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,0.04)) +
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("18:25", "21:25", "00:25","03:25","05:55"), labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +  
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) + 
 theme_4_bottom1_AC +
  labs(title="700-799 nm")

heat_800_night_FM  <- ggplot(dinard_form_night_av_zeros,aes(day,time2,fill=nm_800)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.02,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,0.04)) +
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("18:25", "21:25", "00:25","03:25","05:55"), labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +  
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) + 
  theme_4_bottom1_AC +
  labs(title="800-899 nm")

heat_900_night_FM  <- ggplot(dinard_form_night_av_zeros,aes(day,time2,fill=nm_900)) +
    geom_tile(color= NA,size=0,show.legend = FALSE) + 
     scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.02,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,0.04)) +
    scale_y_discrete(expand = c(0.0, 0.0), breaks = c("18:25", "21:25", "00:25","03:25","05:55"), labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +  
    scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) +  
    theme_4_bottom1_AC + 
    labs(title="900-999 nm") 

heat_night_all <- ggarrange(heat_300_night_FM,heat_400_night_FM,heat_500_night_FM,heat_600_night_FM,heat_700_night_FM,heat_800_night_FM,heat_900_night_FM,
                    ncol = 1, nrow = 7,
                    vjust = 1,
                    hjust = 1,
                    font.label = list(size = 11))

##FIGURE S3b - MOONLIGHT - Relative light intensity 

###normalize 
nm_col_night <- colnames(dinard_form_night_av_zeros)[7:15]
dinard_form_night_av_zeros_norm <- dinard_form_night_av_zeros %>%
  ungroup() %>%
  mutate_at(vars(nm_col_night), ~ as.numeric(.)/max(as.numeric(.))) %>% 
  mutate_at(vars(nm_col_night), ~ as.numeric(sprintf("%.10f", .))) %>%
  mutate_at(vars(nm_col_night), ~ replace(., . < 0, 0))

dinard_form_night_av_zeros_norm$time2 <- factor(dinard_form_night_av_zeros_norm$time, levels = c("18:25","18:55","19:25","19:55","20:25","20:55","21:25","21:55","22:25","22:55", "23:25","23:55","00:25","00:55","01:25","01:55","02:25","02:55","03:25","03:55","04:25","04:55","05:25","05:55"))
dinard_form_night_av_zeros_norm <- dinard_form_night_av_zeros_norm %>%
  select(time2, everything()) 

###plot all wavelenghts 
 heat_300_night  <- ggplot(dinard_form_night_av_zeros_norm,aes(day,time2,fill=nm_300)) +
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  + 
  geom_tile(color= NA,size=0, show.legend = FALSE) + 
  scale_y_discrete(expand = c(0.0, 0.0),   breaks = c("18:25", "21:25", "00:25","03:25","05:55"),labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) +
  theme_4_bottom1_AC  +
  labs(title="300-399 nm")

heat_400_night  <- ggplot(dinard_form_night_av_zeros_norm,aes(day,time2,fill=nm_400)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0),  breaks = c("18:25", "21:25", "00:25","03:25","05:55"),labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) + 
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) +
  theme_4_bottom1_AC +
  labs(title="400-499 nm")

heat_500_night  <- ggplot(dinard_form_night_av_zeros_norm,aes(day,time2,fill=nm_500)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  + 
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("18:25", "21:25", "00:25","03:25","05:55"),labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) +
  theme_4_bottom1_AC + 
  labs(title="500-599 nm")

heat_600_night  <- ggplot(dinard_form_night_av_zeros_norm,aes(day,time2,fill=nm_600)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  + 
  scale_y_discrete(expand = c(0.0, 0.0),  breaks = c("18:25", "21:25", "00:25","03:25","05:55"),labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) +
  theme_4_bottom1_AC +
  labs(title="600-699 nm")

heat_700_night  <- ggplot(dinard_form_night_av_zeros_norm,aes(day,time2,fill=nm_700)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("18:25", "21:25", "00:25","03:25","05:55"),labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) + 
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) +
 theme_4_bottom1_AC +
  labs(title="700-799 nm")

heat_800_night  <- ggplot(dinard_form_night_av_zeros_norm,aes(day,time2,fill=nm_800)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0),  breaks = c("18:25", "21:25", "00:25","03:25","05:55"),labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) + 
  theme_4_bottom1_AC +
  labs(title="800-899 nm")

heat_900_night  <- ggplot(dinard_form_night_av_zeros_norm,aes(day,time2,fill=nm_900)) +
  geom_tile(color= NA,size=0,show.legend = FALSE) + 
    scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0),  breaks = c("18:25", "21:25", "00:25","03:25","05:55"),labels = c("18:00", "21:00", "00:00", "03:00", "06:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(3, 18, 32, 48, 62, 77, 92, 106)) +  
  theme_4_bottom1_AC + 
  labs(title="900-999 nm") 

heat_night_all_norm <- ggarrange(heat_300_night,heat_400_night,heat_500_night,heat_600_night,heat_700_night,heat_800_night,heat_900_night,
                    ncol = 1, nrow = 7,
                    vjust = 1,
                    hjust = 1,
                    font.label = list(size = 11))

##PREP data for FIGURE S3cd - DAYLIGHT

###filter to get day data
###In order to have only full days in the daylight dataset, first half and last half days need to be excluded 
dinard_form_day0 <- dinard_form[dinard_form$time_UTC >= "06:00:13" & dinard_form$time_UTC <= "17:55:13",]
dinard_form_day <- dinard_form_day0 %>% 
  filter(time_date_UTC >= as.POSIXct("2013-10-18 00:00:13",tz = "UTC"),
        time_date_UTC < as.POSIXct("2014-02-01 06:00:13",tz = "UTC"))%>%
  group_by(date_UTC) %>% 
  mutate(day_daylight_new=cur_group_id()) %>% 
  select(time_date_UTC,day, day_daylight, day_daylight_new, everything()) %>% 
  ungroup()

###remove _nm from column name
for ( col in 1:ncol(dinard_form_day)){
      colnames(dinard_form_day)[col] <-  sub("_nm", "", colnames(dinard_form_day)[col])}

###average day data - 30 min 
dinard_form_day_dt <- data.table(dinard_form_day)
x <- as.xts(dinard_form_day_dt[,time_date_UTC := as.POSIXct(time_date_UTC, tz = "UTC")])
dinard_form_day_av0 <- period.apply(x, endpoints(x, "minutes", 30), mean)
dinard_form_day_av <- data.frame(time_date_UTC=index(dinard_form_day_av0), coredata(dinard_form_day_av0))
names(dinard_form_day_av) <- sub("^X", "", names(dinard_form_day_av))

####get date and time, add time as factor and order to be able to filter time later
dinard_form_day_av_ed <- dinard_form_day_av %>%
    mutate(date = as.Date(time_date_UTC, tz= "UTC"), 
            time = factor(format(as.POSIXct(time_date_UTC), format = "%H:%M"),ordered = TRUE)) %>% 
    select(date, time, everything()) 
dinard_form_day_av_ed <- as.tibble(dinard_form_day_av_ed)

###sum up all wavelenghts and bin by each 100nm 
dinard_form_day_av_ed <- dinard_form_day_av_ed %>%
    mutate(nm_all_wave = rowSums(across(7:198), na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(nm_300 = rowSums(select(., starts_with('3')))) %>%
    mutate(nm_400 = rowSums(select(., starts_with('4')))) %>%
    mutate(nm_500 = rowSums(select(., starts_with('5')))) %>%
    mutate(nm_600 = rowSums(select(., starts_with('6')))) %>%
    mutate(nm_700 = rowSums(select(., starts_with('7')))) %>%
    mutate(nm_800 = rowSums(select(., starts_with('8')))) %>%
    mutate(nm_900 = rowSums(select(., starts_with('9')))) %>%
    select(-c(7:198)) %>%
    mutate(nm_400_500_600 = rowSums(across(9:11), na.rm = TRUE))

###transform negative values to 0 for heatmap 
cols <- c("nm_900", "nm_800", "nm_700", "nm_600", "nm_500", "nm_400", "nm_300", "nm_400_500_600", "nm_all_wave")
dinard_form_day_av_zeros <- dinard_form_day_av_ed

for (col in cols) {
  dinard_form_day_av_zeros[[col]][dinard_form_day_av_zeros[[col]] < 0] <- 0}

##FIGURE S3c - DAYLIGHT - Absolute light intensity 

###define factors to set order in the heatmap 
dinard_form_day_av_zeros$time2 <- factor(dinard_form_day_av_zeros$time, levels = c("06:25","06:55","07:25","07:55","08:25","08:55","09:25","09:55","10:25","10:55","11:25","11:55","12:25","12:55","13:25","13:55","14:25","14:55","15:25","15:55","16:25","16:55","17:25","17:55"))
dinard_form_day_av_zeros <- dinard_form_day_av_zeros %>%
  select(time2, everything()) 

###plot all wavelenghts 
heat_300_day_lim  <- ggplot(dinard_form_day_av_zeros,aes(day_daylight_new,time2,fill=nm_300)) +
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 1500,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,3000)) +
  geom_tile(color= NA,size=0.1, show.legend = FALSE) + 
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"), labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC +
  labs(title="300-399 nm")

heat_400_day_lim  <- ggplot(dinard_form_day_av_zeros,aes(day_daylight_new,time2,fill=nm_400)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 1500,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,3000)) +
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"), labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC +
  labs(title="400-499 nm")

heat_500_day_lim  <- ggplot(dinard_form_day_av_zeros,aes(day_daylight_new,time2,fill=nm_500)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 1500,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,3000)) +
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC + 
  labs(title="500-599 nm")

heat_600_day_lim  <- ggplot(dinard_form_day_av_zeros,aes(day_daylight_new,time2,fill=nm_600)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 1500,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,3000)) + 
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC +
  labs(title="600-699 nm")

heat_700_day_lim <- ggplot(dinard_form_day_av_zeros,aes(day_daylight_new,time2,fill=nm_700)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 1500,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,3000)) +
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
 theme_4_bottom1_AC +
  labs(title="700-799 nm")

heat_800_day_lim  <- ggplot(dinard_form_day_av_zeros,aes(day_daylight_new,time2,fill=nm_800)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 1500,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,3000)) +
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC +
  labs(title="800-899 nm")

heat_900_day_lim  <- ggplot(dinard_form_day_av_zeros,aes(day_daylight_new,time2,fill=nm_900)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 1500,na.value="black",name=bquote(mW/m^-2/nm^-1), limits=c(0,3000)) + 
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) +  
  theme_4_bottom1_AC + 
  labs(title="900-999 nm") 

heat_day_all <- ggarrange(heat_300_day_lim,heat_400_day_lim,heat_500_day_lim,heat_600_day_lim,heat_700_day_lim,heat_800_day_lim,heat_900_day_lim,
                    ncol = 1, nrow = 7,
                    vjust = 1,
                    hjust = 1,
                    font.label = list(size = 11))


##FIGURE S3d - DAYLIGHT - Relative light intensity 

###normalize 
nm_col_day <- colnames(dinard_form_day_av_zeros)[8:16]
dinard_form_day_av_zeros_norm <- dinard_form_day_av_zeros %>%
  ungroup() %>%
  mutate_at(vars(nm_col_night), ~ as.numeric(.)/max(as.numeric(.))) %>% 
  mutate_at(vars(nm_col_night), ~ as.numeric(sprintf("%.10f", .))) %>%
  mutate_at(vars(nm_col_night), ~ replace(., . < 0, 0))

####plot all wavelenghts 
heat_300_day_norm  <- ggplot(dinard_form_day_av_zeros_norm,aes(day_daylight_new,time2,fill=nm_300)) +
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  geom_tile(color= NA,size=0.1, show.legend = FALSE) + 
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC + 
  labs(title="300-399 nm")

heat_400_day_norm  <- ggplot(dinard_form_day_av_zeros_norm,aes(day_daylight_new,time2,fill=nm_400)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
   scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC +
  labs(title="400-499 nm")

heat_500_day_norm  <- ggplot(dinard_form_day_av_zeros_norm,aes(day_daylight_new,time2,fill=nm_500)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
   scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC +
  labs(title="500-599 nm")

heat_600_day_norm  <- ggplot(dinard_form_day_av_zeros_norm,aes(day_daylight_new,time2,fill=nm_600)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
   scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC + 
  labs(title="600-699 nm")

heat_700_day_norm  <- ggplot(dinard_form_day_av_zeros_norm,aes(day_daylight_new,time2,fill=nm_700)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
  scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) +  
  theme_4_bottom1_AC +
  labs(title="700-799 nm")

heat_800_day_norm  <- ggplot(dinard_form_day_av_zeros_norm,aes(day_daylight_new,time2,fill=nm_800)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
 scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  +  
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC +
  labs(title="800-899 nm")

heat_900_day_norm  <- ggplot(dinard_form_day_av_zeros_norm,aes(day_daylight_new,time2,fill=nm_900)) +
  geom_tile(color= NA,size=0.1,show.legend = FALSE) + 
 scale_fill_gradient2(low = "black", high = "darkgoldenrod1", mid = "white", midpoint = 0.5,na.value="black",name=bquote(mW/m^-2/nm^-1),limits=c(0,1), labels =  function(x) sprintf("%g", x))  + 
  scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:25", "09:25", "12:25","15:25","17:55"),labels = c("06:00", "09:00", "12:00", "15:00", "18:00")) +
  scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
  theme_4_bottom1_AC +
  labs(title="900-999 nm") 

heat_day_NORM_all <- ggarrange(heat_300_day_norm,heat_400_day_norm,heat_500_day_norm,heat_600_day_norm,heat_700_day_norm,heat_800_day_norm,heat_900_day_norm,
                    ncol = 1, nrow = 7,
                    vjust = 1,
                    hjust = 1,
                    font.label = list(size = 11))

#write arranged figures into a single multi-page PDF
pdf("02_visuals/FigureS3_Dinard_summed_waves_over_time.pdf", width = 12, height = 28, useDingbats = FALSE)

print(heat_night_all)
print(heat_night_all_norm)
print(heat_day_all)        
print(heat_day_NORM_all)   

dev.off()