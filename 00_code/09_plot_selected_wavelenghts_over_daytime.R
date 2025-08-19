#!/usr/bin/env Rscript

library(tidyverse)

#FIGURE S4 - selected daylight wavelenghts over time  

dinard_form <- as_tibble(read.csv("01_data/dinard_light_data_formatted.csv", row.names = NULL))
dinard_form <- dinard_form %>%
    mutate(time_date_UTC = as.POSIXct(dinard_form$time_date_UTC, format="%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    select(time_date_UTC, everything(), 
            -c(X)) %>%
    rename_with(~ sub("^X", "", .x), starts_with("X"))


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

###select column names 
nm_col_day0 <- colnames(dinard_form_day)[7:198]

#NORMALIZATION -  values per nm 
dinard_form_day_norm <- dinard_form_day %>%
  ungroup() %>%
  mutate_at(vars(nm_col_day0), ~ as.numeric(.)/max(as.numeric(.))) %>% 
  mutate_at(vars(nm_col_day0), ~ as.numeric(sprintf("%.10f", .))) %>%
  mutate_at(vars(nm_col_day0), ~ replace(., . < 0, NA)) %>% # substitute values below zero by NA 
  rename_with(~ paste0("nm_", .), 7:198) 

#remove :13 seconds from time 
dinard_form_day_norm$time_UTC_2 <- gsub(":13", "",dinard_form_day_norm$time_UTC)
#normal order
dinard_form_day_norm$time_UTC_3 <- factor(dinard_form_day_norm$time_UTC_2, levels = paste(unique(dinard_form_day_norm$time_UTC_2)))

#final day dataset to use for FigS3 (selected waves not averaged, not summed with tides)
dinard_form_day_norm <- dinard_form_day_norm %>% 
  select(day_daylight_new,time_UTC_3,time_UTC_2,everything())


###PLOTTING 

cols_plot_subs_bins <- c("nm_317","nm_337","nm_358","nm_378","nm_398","nm_401","nm_425","nm_448","nm_472","nm_495","nm_502","nm_526","nm_549","nm_572","nm_596","nm_603","nm_626","nm_649","nm_673","nm_696","nm_703","nm_726","nm_749","nm_772","nm_795","nm_802","nm_825","nm_848","nm_871","nm_894","nm_900","nm_914","nm_927","nm_940","nm_953") #pick five per 100nm class, interspersed equally

create_plot <- function(col_name) {
ggplot() +
      geom_tile(data = dinard_form_day_norm, aes(day_daylight,time_UTC_3,fill=!!as.name(col_name)), color= NA,show.legend = FALSE) + 
      scale_fill_gradient2(low = "black", high = "darkorange2", mid = "white", midpoint = 0.5,na.value="black",name=bquote(atop("Day light",(mW/m^-2/nm^-1)))) +
      scale_y_discrete(expand = c(0.0, 0.0), breaks = c("06:00", "09:25", "12:25","15:25","17:55")) + 
      scale_x_continuous(expand = c(0.0, 0.0), breaks = c(2,17,31,47,61,76,91,105)) + 
      #geom_point(data =tides_filtered_4daylight_s_f, size =1, aes(x = day_daylight, y = time4,color = water_level_m)) + 
      #scale_color_gradient(low=muted("blue"), high=muted("darkred"),name = bquote(atop("Water level","(m)"))) +
       theme(axis.text.x = element_blank(), 
        panel.grid = element_blank(),
       axis.text.y = element_blank(), 
       panel.background = element_blank(),
       axis.line = element_line(),
       legend.position = "none", 
       axis.title =element_blank(),
       plot.title = element_text(hjust = 0.5)) + 
    labs(title=col_name)
}

#loop through each column name and create the plots 
plots <- lapply(cols_plot_subs_bins, create_plot)

# Combine the plots into a grid using grid.arrange - needs to be saved as a variable so that ggsave does not save just the last figure!
g <- gridExtra::grid.arrange(grobs = plots, ncol = 5)

ggsave("02_visuals/FigureS4_selected_wavelenghts_daylight_tides.pdf",g, width = 15, height = 8,limitsize = FALSE)
