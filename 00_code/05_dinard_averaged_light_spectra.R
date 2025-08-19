#!/user/bin/env Rscript

library(tidyverse)
library(patchwork)

#get data
dinard_form <- as_tibble(read.csv("01_data/dinard_light_data_formatted.csv", row.names = NULL))
dinard_form <- dinard_form %>%
    mutate(time_date_UTC = as.POSIXct(dinard_form$time_date_UTC, format="%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    select(time_date_UTC, everything(), 
            -c(X)) %>%
    rename_with(~ sub("^X", "", .x), starts_with("X"))

#filter for days after full moon in two cycles 
dinard_form_d2 <- dinard_form[dinard_form$date_UTC == "2013-11-18" | dinard_form$date_UTC == "2013-12-18", ]

###NIGHT CALCULATIONS 
dinard_form_d2_night <- dinard_form_d2[dinard_form_d2$time_UTC >= "02:00:13" & dinard_form_d2$time_UTC <= "04:55:13",]

#number of datapoints
n_night <- nrow(dinard_form_d2_night)

#mean and standard deviation for each wavelength
average_spectrum_stats_NIGHT <- dinard_form_d2_night %>%
  select(-c(date_UTC, time_UTC, time_date_UTC, day, day_daylight)) %>%  # Exclude the time column
  summarise_all(list(mean = mean, sd = sd))

#standard error for each wavelength
average_spectrum_stats_NIGHT <- average_spectrum_stats_NIGHT %>%
  mutate(across(ends_with("sd"), ~ . / sqrt(n_night), .names = "{col}_se"))

#transpose
average_spectrum_NIGHT_mean <- as.data.frame(t(average_spectrum_stats_NIGHT %>% select(ends_with("mean"))))
average_spectrum_NIGHT_sd <- as.data.frame(t(average_spectrum_stats_NIGHT %>% select(ends_with("sd"))))
average_spectrum_NIGHT_se <- as.data.frame(t(average_spectrum_stats_NIGHT %>% select(ends_with("se"))))

#rename rownames for col
rownames(average_spectrum_NIGHT_mean) <- sub("_nm_mean", "", rownames(average_spectrum_NIGHT_mean))
rownames(average_spectrum_NIGHT_sd) <- sub("_nm_sd", "", rownames(average_spectrum_NIGHT_sd))
rownames(average_spectrum_NIGHT_se) <- sub("_nm_sd_se", "", rownames(average_spectrum_NIGHT_se))

#combine all 
average_spectrum_NIGHT <- data.frame(
  wavelength = as.numeric(rownames(average_spectrum_NIGHT_mean)),
  average = unlist(average_spectrum_NIGHT_mean),
  sd = unlist(average_spectrum_NIGHT_sd),
  se = unlist(average_spectrum_NIGHT_se))

#numeric
average_spectrum_NIGHT <- average_spectrum_NIGHT %>%
  mutate(across(c(wavelength, average, sd, se), as.numeric))

#remove values below zero and filter out wavelenghts 
average_spectrum_NIGHT_f <- average_spectrum_NIGHT %>%
  filter(average > 0, wavelength >= 380, wavelength <= 750)

###DAY CALCULATIONS 
dinard_form_d2_day <- dinard_form_d2[dinard_form_d2$time_UTC >= "11:00:13" & dinard_form_d2$time_UTC <= "13:55:13",]

#number of datapoints
n <- nrow(dinard_form_d2_day)

#mean and standard deviation for each wavelength
average_spectrum_stats <- dinard_form_d2_day %>%
  select(-c(date_UTC, time_UTC, time_date_UTC, day, day_daylight)) %>%  # Exclude the time column
  summarise_all(list(mean = mean, sd = sd))

#standard error for each wavelength
average_spectrum_stats <- average_spectrum_stats %>%
  mutate(across(ends_with("sd"), ~ . / sqrt(n), .names = "{col}_se"))

#transpose
average_spectrum_mean <- as.data.frame(t(average_spectrum_stats %>% select(ends_with("mean"))))
average_spectrum_sd <- as.data.frame(t(average_spectrum_stats %>% select(ends_with("sd"))))
average_spectrum_se <- as.data.frame(t(average_spectrum_stats %>% select(ends_with("se"))))

#rename rownames for col
rownames(average_spectrum_mean) <- sub("_nm_mean", "", rownames(average_spectrum_mean))
rownames(average_spectrum_sd) <- sub("_sd", "", rownames(average_spectrum_sd))
rownames(average_spectrum_se) <- sub("_sd_se", "", rownames(average_spectrum_se))

#combine all 
average_spectrum_DAY <- data.frame(
  wavelength = as.numeric(rownames(average_spectrum_mean)),
  average = unlist(average_spectrum_mean),
  sd = unlist(average_spectrum_sd),
  se = unlist(average_spectrum_se))

#numeric
average_spectrum_DAY <- average_spectrum_DAY %>%
  mutate(across(c(wavelength, average, sd, se), as.numeric))

#remove values below zero and filter out wavelenghts 
average_spectrum_DAY_f <- average_spectrum_DAY %>%
  filter(wavelength >= 380, wavelength <= 750)

#######PLOTTING#######

#NIGHT 
night_spect_se <- ggplot(data = average_spectrum_NIGHT_f, aes(x = wavelength, y = average)) + 
  geom_point(color = "#2b4bb5") +  # Change geom_path() to geom_point() to add points
  geom_ribbon(aes(ymin = average - se, ymax = average + se), fill = "#2b4bb5", alpha = 0.2) + 
  scale_y_continuous(expand = c(0.0, 0.0)) + 
  scale_x_continuous(expand = c(0.0, 0.0), breaks = seq(300, 800, by = 50)) + 
  theme(axis.text.x = element_text(size = 12),  # Increase text size
        axis.text.y = element_text(size = 12),  # Increase text size
        axis.title = element_text(size = 14),  # Increase axis title size
        axis.line = element_line(size = 0.5),
        panel.background = element_blank()) + 
  xlab("Wavelength (nm)") + 
  ylab(expression("Average irradiance (mW" ~ m^{-2} ~ nm^{-1} ~ ")"))

day_spect_se <- ggplot(data = average_spectrum_DAY_f, aes(x = wavelength, y = average)) + 
  geom_point(color = "#ffbc2c") +  # Change geom_path() to geom_point() to add points
  geom_ribbon(aes(ymin = average - se, ymax = average + se), fill = "#ffbc2c", alpha = 0.2) + 
  scale_y_continuous(expand = c(0.0, 0.0)) + 
  scale_x_continuous(expand = c(0.0, 0.0), breaks = seq(300, 800, by = 50)) + 
  theme(axis.text.x = element_text(size = 12),  # Increase text size
        axis.text.y = element_text(size = 12),  # Increase text size
        axis.title = element_text(size = 14),  # Increase axis title size
        axis.line = element_line(size = 0.5),
        panel.background = element_blank()) + 
  xlab("Wavelength (nm)") + 
  ylab(expression("Average irradiance (mW" ~ m^{-2} ~ nm^{-1} ~ ")"))


day_spect_se+night_spect_se

ggsave("02_visuals/FigureS2bc_daylight_and_moonlight_averaged_spectraFM.pdf", width = 12, height =5)
