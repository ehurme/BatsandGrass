# Buzz count gam

## load libraries
library(pacman)
p_load(data.table, lubridate, tidyverse, ggplot2,
       mgcv, suncalc, janitor, corrplot)

bdf <- fread("../../../Dropbox/MPI/BatsandGrass/Data/buzzes-melina-meadows_MD.csv") %>% clean_names()
tmpsplt <- bdf$buzzfile_na_means_no_buzz %>% strsplit(., split = "-")
dates <- sapply(tmpsplt, "[", 1)
loc_hours <- sapply(tmpsplt, "[", 2)
loc <- sapply(strsplit(loc_hours, "_"), "[", 1)
hours <- sapply(strsplit(loc_hours, "_"), "[", 2)

bdf$timestamp <- ymd_hms(paste(dates, hours))
bdf$time %>% mdy
bdf %>% summary

## summarize per sampling minute?


bdf_sum <- bdf %>% group_by(year, where, time) %>%
  summarise(total_buzzes = sum(!is.na(buzzfile_na_means_no_buzz)))


# Sampling hours?
## get index of all relevant audio files




