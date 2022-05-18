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

## summarize per sampling minute?

bdf_sum <- bdf %>% group_by(year, where, time, loc) %>%
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


location <- sapply(strsplit(files, "/"), "[", 6)
location[nchar(location) == 10] <- sapply(strsplit(files[nchar(location)==10], "/"), "[", 7)
location[nchar(location) == 5] <- sapply(strsplit(files[nchar(location)==5], "/"), "[", 7)
location[nchar(location) == 22] <- sapply(strsplit(files[nchar(location)==22], "/"), "[", 6)
location[location == "Audiomoth"] <- sapply(strsplit(files[location=="Audiomoth"], "/"), "[", 5)
location[location == "10.08"] <- sapply(strsplit(files[location=="10.08"], "/"), "[", 6)

location[location == "Edge" | location == "Rand"] <- "edge"
location[location == "Middle" | location == "Mitte"] <- "middle"

location %>% table


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

meadow <- sapply(strsplit(files, "/"), "[", 3)
meadow %>% unique
meadow[meadow == "St.Katharina"] <- "StKatharina"
meadow[meadow == "Hockgraben UnI"] <- "Hockgraben"

meadow %>% table
which(is.na(meadow))

year <- sapply(strsplit(files, "/"), "[", 1)
year %>% unique


paste0(date, meadow, location) %>% table

# parse filenames

fdate <- sapply(strsplit(filenames, "-"), "[", 1) %>% as.Date
floctime <- sapply(strsplit(filenames, "-"), "[", 2)
files[is.na(floctime) %>% which]
nchar(floctime) %>% table

ftime <- sapply(strsplit(floctime, "_"), "[", 2)
loc <- sapply(strsplit(floctime, "_"), "[", 1)


# put everything together in a data.table
buzz_df <- data.table(year, meadow, loc, location, date, time, datetime, filenames)
# # remove recordings after midnight?

# df <- df[which(df$time %>% as.numeric > 150000),]

# summarize data

buzz_df %>% group_by(year, date, meadow, location) %>%
  summarise(start = min(datetime),
            end = max(datetime))
# get first and last times for each recording session


### fill in buzzes

bdf$filenames <- paste0(sapply(strsplit(bdf$buzzfile_na_means_no_buzz, "--"), "[", 1), ".WAV")
bdf$where %>% unique
bdf$where[bdf$where == "Hockgraben Uni"] <- "Uni"
bdf$location <- "edge"
bdf$location[bdf$camera == "Mitte"] <- "middle"

buzz_df$buzz <- 0
buzz_df$species <- NA

NAs <- {}
i = 1
for(i in 1:nrow(bdf)){
  idx <- {}
  bdf[i,]
  idx <- which(bdf$filenames[i] == buzz_df$filenames)

  if(length(idx) > 1){
    idx <- which(bdf$filenames[i] == buzz_df$filenames &
                 bdf$where[i] == buzz_df$meadow &
                 bdf$location[i] == buzz_df$location)
  }

  buzz_df[idx]
  bdf[i,]
  if(length(idx) == 0){
    NAs <- c(NAs, i)
  }


  if(length(idx) == 1){
    buzz_df$buzz[idx] <- buzz_df$buzz[idx] + 1
    if(is.na(buzz_df$species[idx])){
      buzz_df$species[idx] <- bdf$species[i]
    }
    else{
      buzz_df$species[idx] <- paste(buzz_df$species[idx], bdf$species[i])
    }

  }
}
bdf[NAs,]
NAs %>% length

buzz_df$buzz %>% table

buzz_df$species %>% unique
sum(buzz_df$buzz, na.rm = TRUE)
sum(!is.na(bdf$buzzfile_na_means_no_buzz))

buzz_df$buzz %>% table

buzz_df$yday <- yday(buzz_df$datetime)

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




