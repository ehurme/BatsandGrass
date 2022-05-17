# Buzz count gam

## load libraries
library(pacman)
p_load(data.table, lubridate, tidyverse, ggplot2,
       mgcv, suncalc, janitor, corrplot)

bdf <- fread("../../../Dropbox/MPI/BatsandGrass/Data/buzzes-melina-meadows_MD.csv") %>% clean_names()
tmpsplt <- bdf$buzzfile_na_means_no_buzz %>% strsplit(., split = "-")
dates <- sapply(tmpsplt, "[", 1)
loc_hours <- sapply(tmpsplt, "[", 2)
bdf$loc <- sapply(strsplit(loc_hours, "_"), "[", 1)
hours <- sapply(strsplit(loc_hours, "_"), "[", 2)

bdf$timestamp <- ymd_hms(paste(dates, hours))
bdf$time %>% mdy
bdf %>% summary

## summarize per sampling minute?


bdf_sum <- bdf %>% group_by(year, where, time) %>%
  summarise(total_buzzes = sum(!is.na(buzzfile_na_means_no_buzz)))


# Sampling hours?
## get index of all relevant audio files

# ### audio files were 55 sec long
# files <- list.files("D:/batsandgrass/", pattern = "*.wav", ignore.case = TRUE,
#                     full.names = FALSE, recursive = TRUE)
# folders <- list.dirs("D:/batsandgrass/", recursive = TRUE, full.names = TRUE)
# save(files, folders, file = "../../../Dropbox/MPI/BatsandGrass/Data/audio_files.robj")

load("../../../Dropbox/MPI/BatsandGrass/Data/audio_files.robj")

# remove spectrogram files
files <- files[-c(which(grepl("Spectro", files)),
                 which(grepl("scent", files)))]

# remove scent experiment files
experiment <- sapply(strsplit(files, "/"), "[", 2)
experiment %>% table

files <- files[-which(experiment == "ScentExperiment2021")]

filenames <- {}
filenames <- sapply(strsplit(files, "/"), "[", 8)
filenames[which(is.na(filenames))] <- sapply(strsplit(files[which(is.na(filenames))], "/"), "[", 7)
filenames[which(nchar(filenames) < 18)] <- sapply(strsplit(files[which(nchar(filenames) < 18)], "/"), "[", 9)
files[which(is.na(filenames))]
table(nchar(filenames))

filenames[which(nchar(filenames) == 27)] <- substr(filenames[which(nchar(filenames) == 27)], 5, 27)

date <- substr(filenames, 1, 8) %>% ymd
time <- substr(filenames, nchar(filenames)-9, nchar(filenames)-4)
datetime <- ymd_hms(paste(date, time))
filenames[datetime %>% is.na %>% which]

loc <- sapply(strsplit(files, "/"), "[", 3)
loc %>% unique
loc[loc == "St.Katharina"] <- "StKatharina"
loc[loc == "Hockgraben UnI"] <- "Hockgraben"

loc %>% table
which(is.na(loc))

year <- sapply(strsplit(files, "/"), "[", 1)
year %>% unique


paste0(year, date, loc) %>% table

# parse filenames

fdate <- sapply(strsplit(filenames, "-"), "[", 1) %>% as.Date
floctime <- sapply(strsplit(filenames, "-"), "[", 2)
files[is.na(floctime) %>% which]
nchar(floctime) %>% table

ftime <- sapply(strsplit(floctime, "_"), "[", 2)
floc <- sapply(strsplit(floctime, "_"), "[", 1)


# put everything together in a data.table
df <- data.table(year, site = loc, loc = floc, date, time, datetime)
# # remove recordings after midnight?

df <- df[which(df$time %>% as.numeric > 150000),]

# summarize data

df %>% group_by(year, date, site) %>%
  summarise(start = min(datetime),
            end = max(datetime))
# get first and last times for each recording session


### fill in buzzes

bdf$where %>% unique
bdf$where[bdf$where == "Hockgraben Uni"] <- "Uni"

df$buzz <- 0
df$species <- NA

i = 1
for(i in 1:nrow(bdf)){
  idx <- {}
  idx <- which(bdf$where[i] == df$site &
                 bdf$timestamp[i] == df$datetime &
                 bdf$loc[i] == df$loc)
  if(length(idx) == 0){print(bdf[i,])}
  df$buzz[idx] <- df$buzz[idx] + 1
  df$species[idx] <- bdf$species[i]
}

df$species %>% unique
sum(df$buzz)
df$buzz %>% table

df$yday <- yday(df$datetime)

buzz_df <- df
save(bdf, buzz_df, file = "../../../Dropbox/MPI/BatsandGrass/Data/buzz_count.robj")










#which files didn't parse well?
# floctime[which(is.na(floc))]
files[which(is.na(floc))] # 21,000 #18,000

# 2020 files
filenames[which(is.na(floc))] <- sapply(strsplit(files[which(is.na(floc))], "/"), "[", 7)

fdate <- sapply(strsplit(filenames, "-"), "[", 1) %>% as.Date
floctime <- sapply(strsplit(filenames, "-"), "[", 2)
ftime <- sapply(strsplit(floctime, "_"), "[", 2)
floc <- sapply(strsplit(floctime, "_"), "[", 1)

#which files didn't parse well?
files[which(is.na(floc))] # 21,000 #18,000




