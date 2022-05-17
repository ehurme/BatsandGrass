# Insect count

## load libraries
library(pacman)
p_load(data.table, lubridate, tidyverse, ggplot2,
       mgcv, suncalc, janitor, corrplot)

## load the data
### 2020

df20 <- fread("../../../Dropbox/MPI/BatsandGrass/Data/bdf.final.csv")
df20 <- df20 %>% mutate(.,
                ID,
                platform,
                image_name = filename,
                # date = ymd(date.x.x),
                time = mtime,
                meadow = meadow.x,
                location,
                grassheight,
                insects = found_insects,
                # bats = totalcount,
                treatment = Treatment,
                .keep = "none") %>%
  filter(treatment=="pre" | treatment=="post" | treatment == "mowing")
df20$treatment %>% unique
df20$id <- sapply(strsplit(df20$ID, "-"), "[", 2)
df20$loc <- substr(df20$id, 1, nchar(df20$id)-1)
paste0(df20$loc, ": ", df20$meadow) %>% unique

# df20$timeID <- sapply(strsplit(df20$uniqueID, "-"), "[", 3)
df20$datetime <- round_date(df20$time, unit = "1 minute")
tmp <- df20 %>% group_by(meadow, location, loc, datetime, grassheight) %>%
  summarise(mean_insect = mean(insects), platforms = n())
summary(tmp)
### 2021

df21 <- fread("../../../Dropbox/MPI/BatsandGrass/Data/German_set_all_results95_v7MM.csv") %>% clean_names()
### remove result column
df21$result <- {}
df21$grassheight <- as.numeric(gsub(",", ".", gsub("\\.", "", df21$grassheight)))
### remove rain
df21 <- df21[df21$note_if_more_then_10_insects != "rain",]

df21 <- df21 %>% mutate(.,
                ID = image_name,
                # uniqueID,
                platform = NA,
                image_name = name_org,
                # date = ymd(creation_date),
                time = ymd_hms(creation_date),
                meadow = NA,
                location,
                grassheight,
                insects = found_insects,
                # bats = totalcount,
                treatment,
                .keep = "none") %>%
  filter(treatment=="pre" | treatment=="post" | treatment == "mowing")

df21$id <- sapply(strsplit(df21$ID, "_"), "[", 2)
df21$loc <- substr(df21$id, 1, nchar(df21$id)-1)

df21$datetime <- round_date(df21$time, unit = "1 minute")
tmp2 <- df21 %>% group_by(loc, location, datetime, grassheight) %>%
  summarise(mean_insect = mean(insects), platforms = n())

df <- full_join(tmp, tmp2)
df[which(df$platforms> 1),]

#df <- df %>% mutate(., time = datetime)
df$year <- year(df$datetime)

df$loc %>% unique()
df$meadow[df$loc == "H"] <- "Hegne"
df$meadow[df$loc == "HG"] <- "Hockgraben"
df$meadow[df$loc == "I"] <- "Institute"
df$meadow[df$loc == "S"] <- "Scheune"
df$meadow[df$loc == "SK"] <- "St.Katharina"
df$meadow[df$loc == "T"] <- "Tannenhof"
df$meadow[df$loc == "U"] <- "Uni"
df$meadow[df$loc == "UK"] <- "UniKurve"
df$meadow[df$loc == "V"] <- "Voliere"
df$meadow[df$loc == "W"] <- "Wollmatingen"
df$meadow[df$loc == "M"] <- "Mill"

## how many pictures have insects?
hist(df$mean_insect, breaks = 1000, xlim = c(0,10))

sum(df20$found_insects)

sum(df$mean_insect == 0)/nrow(df) # 55% of minutes monitored are empty

sum(df$mean_insect >= 10)/nrow(df) # 2.1% of minutes contain swarms

#######################################################################
## join buzz count

load("../../../Dropbox/MPI/BatsandGrass/Data/buzz_count.robj")
buzz_df$meadow <- buzz_df$site
buzz_df$year <- as.numeric(buzz_df$year)

# monitoring data
m <- full_join(df, buzz_df[,-c("site", "loc", "date", "time", "yday")],
               by = c("year", "datetime", "meadow"))

which(!is.na(m$mean_insect) & !is.na(m$buzz))


df$buzz <- NA
i
for(i in 1:nrow(buzz_df)){
  idx <- which(buzz_df$year[i] == df$year &
                 buzz_df$meadow[i] == df$meadow &
                 buzz_df$datetime[i] == df$time)
  if(length(idx)>0){
    df$buzz[idx] <- buzz_df$buzz[i]
  }
}

df$datetime <- df$time

full_join(df[,c()])











## get sunset times
dates <- unique(date(df$time))
dates <- dates[order(dates)]
sunset <- suncalc::getSunlightTimes(date = seq.Date(dates[1], dates[length(dates)], by = 1), lat = 47.6904, lon = 9.1869, tz = "CET")
df$sunset <- ymd_hms("2000-01-01 12:00:00", tz = "CET")

i = 17068
for(i in 1:nrow(df)){

  idx <- which.min(abs(df$time[i] - sunset$sunset))

  df$sunset[i] <- sunset$sunset[idx]
}

df$min_since_sunset <- as.numeric(df$time - (df$sunset+2*3600))/60
range(df$min_since_sunset)/60
df[abs(df$min_since_sunset) > 50000,]
which(abs(df$min_since_sunset) > 50000)
hist((df$min_since_sunset), breaks = 1000)

plot(df$time[1:100], df$min_since_sunset[1:100])

plot(df$time, df$mean_insect)


## add weather data
temp <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_tu_stunde_20070401_20211231_00044.txt")
wind <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_ff_stunde_20201001_20211231_15801.txt")

temp$TT_TU[temp$TT_TU < -200] <- NA
temp$time <- ymd_h(temp$MESS_DATUM, tz = "CET")

wind$time <- ymd_h(wind$MESS_DATUM, tz = "CET")

# plot(temp$time, temp$TT_TU)
plot(wind$time, wind$F)
df$temperature <- NA
df$wind <- NA

for(i in 1:nrow(df)){
  tidx <- which.min(abs(temp$time - (df$time[i]-2*3600)))
  widx <- which.min(abs(wind$time - (df$time[i]-2*3600)))
  # df$time[i]
  # temp[tidx,]
  # wind[widx,]
  df$temperature[i] <- temp$TT_TU[tidx]
  df$wind[i] <- wind$F[widx]
}

df$yday <- yday(df$time)
summary(df)
table(df$site)

table(df$platforms)


g1 <- gam(mean_insect~
            s(as.numeric(yday), k = 5)+
            s(as.numeric(min_since_sunset), k = 5)+
            s(grassheight, k = 5)+
            s(temperature, k = 5)+
            s(wind, k = 5)+
            as.factor(year)+site,
          data = df[df$site != "GS",],
          family = negbin(theta = 0.62),
          method = "REML")
summary(g1)
# plot(g1)

corrplot(cor(df[,c("yday", "temperature", "wind", "min_since_sunset")]), method = "number")

save(df, g1, file = "insect_gam.robj")
load("insect_gam.robj")

dates <- unique(date(df$time))
i = 1
for(i in 1:length(dates)){
  with(df[date(df$time) == dates[i],], plot(min_since_sunset, wind,
                                            main = dates[i]))
}

with(df[date(df$time) == "2020-08-16",], plot(min_since_sunset, wind))

# suspect dates
df[date(df$time) == "2020-08-13",]

# "2020-08-14", "2020-08-18",  - only two points
# "2021-08-13", "2021-08-03", "2021-07-23", "2021-08-20", "2021-08-07", "2021-07-29", - few points

# ggplot(df, aes(min_since_sunset, temperature, col = site))+geom_point()+facet_wrap(~date(time))
# ggplot(df, aes(min_since_sunset, wind, col = site))+geom_point()+facet_wrap(~date(time))



names(df)









