#!/usr/bin/env Rscript

library(tidyverse)

dinard_formatted <- as_tibble(read.csv("01_data/dinard_light_data_formatted.csv", row.names = NULL))

#ranges to subset the data 
ranges <- data.frame(
  start = c("2013-10-17", "2013-11-04", "2013-11-22",
            "2013-12-10", "2013-12-28", "2014-01-15"),
  end   = c("2013-11-03", "2013-11-21", "2013-12-09",
            "2013-12-27", "2014-01-14", "2014-02-01"),
  tag   = c("1st_part", "2nd_part", "3rd_part",
            "4th_part", "5th_part", "6th_part"),
  stringsAsFactors = FALSE)

#function to filter by dates and by wavelenghts (visible spectra), save CSV in temp folder 
process_window <- function(start, end, tag) {
out <-  dinard_formatted %>%  
    filter(date_UTC >= as.Date(start), date_UTC <= as.Date(end)) %>%
    pivot_longer(cols = ends_with("nm"),
                 names_to = "wavelength",
                 values_to = "intensity") %>%
    mutate(wavelength = str_remove(wavelength, "^X"),
            wavelength = as.numeric(str_remove(wavelength, "_nm"))) %>%
    filter(dplyr::between(wavelength, 380, 750))

  write.csv(out, paste0("01_data/temp/dinard_long_for_python_", tag, ".csv"), row.names = FALSE)
}

for (i in seq_len(nrow(ranges))) {
    process_window(ranges$start[i], ranges$end[i], ranges$tag[i])
}