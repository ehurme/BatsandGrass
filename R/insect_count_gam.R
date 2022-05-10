library(pacman)
p_load(data.table, lubridate, tidyverse, ggplot2, mgcv, suncalc)

df <- fread("../../../Dropbox/MPI/BatsandGrass/Data/German_set_all_results95_v7MM.csv")
df <- df[df$`note (if more then 10 insects)` != "rain",]


hist(df$found_insects, breaks = 1000, xlim = c(0,100))
sum(df$found_insects > 5)

df$folder_name_org %>% unique
df$time <- df$creation_date %>% ymd_hms
df$date <- date(df$time)
df$result <- {}
df$site <- sapply(strsplit(df$folder_name_org, "-"), "[", 2)

dates <- unique(df$date)
sunset <- suncalc::getSunlightTimes(date = dates, lat = 47.6904, lon = 9.1869)
df$sunset %>% unique
df$sunset2 <- {}
df$sunset2 <- ymd_hms("2000-01-01 12:00:00")

i = 1
for(i in 1:length(dates)){
  sidx <- which(sunset$date == dates[i])
  idx <- which(df$date == dates[i])
  df$sunset2[idx] <- sunset$sunset[sidx]
}

df$min_since_sunset <- df$time - df$sunset2

hour(df$time[1])

plot(df$date, df$found_insects)
