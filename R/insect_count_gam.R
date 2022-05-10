# Insect count

## load libraries
library(pacman)
p_load(data.table, lubridate, tidyverse, ggplot2, mgcv, suncalc, janitor)

## load the data
df <- fread("../../../Dropbox/MPI/BatsandGrass/Data/German_set_all_results95_v7MM.csv") %>% clean_names()
### remove rain
df <- df[df$note_if_more_then_10_insects != "rain",]

## how many pictures have insects?
hist(df$found_insects, breaks = 1000, xlim = c(0,100))
sum(df$found_insects == 0)/nrow(df) # 75.9% of pictures are empty
sum(df$found_insects >= 10)/nrow(df) # 2.6% contain swarms

# clean up dataframe
df$folder_name_org %>% unique
df$grassheight <- as.numeric(gsub(",", ".", gsub("\\.", "", df$grassheight)))

## add date and time
df$time <- df$creation_date %>% ymd_hms
# df$date <- date(df$time)
df$year <- year(df$time)

# remove result column
df$result <- {}

# save site name
df$site <- sapply(strsplit(df$folder_name_org, "-"), "[", 2)
df$date <- sapply(strsplit(df$folder_name_org, "-"), "[", 1) %>% ymd
## get sunset times
dates <- unique(df$date)
sunset <- suncalc::getSunlightTimes(date = dates, lat = 47.6904, lon = 9.1869, tz = "CET")
df$sunset %>% unique
df$sunset2 <- {}
df$sunset2 <- ymd_hms("2000-01-01 12:00:00", tz = "CET")

i = 1
for(i in 1:length(dates)){
  sidx <- which(sunset$date == dates[i])
  idx <- which(df$date == dates[i])
  df[idx,]
  sunset[sidx,]
  df$sunset2[idx] <- sunset$sunset[sidx] + 2*3600
}

df$min_since_sunset <- as.numeric(df$time - df$sunset2)
range(df$min_since_sunset)/3600
df[abs(df$min_since_sunset) > 50000,]
hist(abs(df$min_since_sunset), breaks = 1000)

plot(df$time[1:100], df$min_since_sunset[1:100])

plot(df$date, df$found_insects)


## add weather data
temp <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_tu_stunde_20070401_20211231_00044.txt")
wind <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_ff_stunde_20201001_20211231_15801.txt")

temp$TT_TU[temp$TT_TU < -200] <- NA
temp$time <- ymd_h(temp$MESS_DATUM)

wind$time <- ymd_h(wind$MESS_DATUM)

# plot(temp$time, temp$TT_TU)
plot(wind$time, wind$F)

for(i in 1:nrow(df)){
  tidx <- which.min(abs(temp$time - df$time[i]))
  widx <- which.min(abs(wind$time - df$time[i]))

  df$temperature[i] <- temp$TT_TU[tidx]
  df$wind[i] <- wind$F[widx]
}

df$found_insects %>% length
df$date %>% length
df$min_since_sunset %>% length
summary(df)

g1 <- gam(found_insects~
            # s(date)
            s(as.numeric(min_since_sunset))+
            s(temperature)+
            s(wind)+
            site,
          data = df,
          family = negbin(theta = 0.62),
          method = "REML", scale = 0)
summary(g1)
plot(g1)



