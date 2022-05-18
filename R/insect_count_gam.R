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
tmp <- df20 %>% group_by(ID, meadow, location, loc, datetime, grassheight) %>%
  summarise(insects = mean(insects),
            # total_insects = sum(insects),
            platforms = n())
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
tmp2 <- df21 %>% group_by(ID, loc, location, datetime, grassheight) %>%
  summarise(insects = mean(insects),
            # total_insects = sum(insects),
            platforms = n())
summary(tmp2)

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
hist(df$insects, breaks = 1000, xlim = c(0,10))

sum(df20$insects)

sum(df$insects == 0)/nrow(df) # 68% of minutes monitored are empty

sum(df$insects >= 10)/nrow(df) # 2.1% of minutes contain swarms

#######################################################################
## join buzz count

load("../../../Dropbox/MPI/BatsandGrass/Data/buzz_count.robj")
buzz_df$year <- as.numeric(buzz_df$year)

# monitoring data
m <- full_join(df, buzz_df[,-c("loc", "date", "time", "yday")],
               by = c("year", "datetime", "meadow", "location"))

which(!is.na(m$insects) & !is.na(m$buzz))

## get sunset times
dates <- unique(date(m$datetime))
dates <- dates[order(dates)]
sunset <- suncalc::getSunlightTimes(date = seq.Date(dates[1], dates[length(dates)], by = 1), lat = 47.6904, lon = 9.1869, tz = "CET")
m$sunset <- ymd_hms("2000-01-01 12:00:00", tz = "CET")

i = 17068
for(i in 1:nrow(m)){
  idx <- which.min(abs(m$datetime[i] - sunset$sunset))
  m$sunset[i] <- sunset$sunset[idx]
}

m$min_since_sunset <- as.numeric(m$datetime - (m$sunset+2*3600))/60
range(m$min_since_sunset)
m[(m$min_since_sunset) < -500,]
hist((m$min_since_sunset), breaks = 1000)

plot(m$datetime[1:100], m$min_since_sunset[1:100])

plot(m$datetime, m$insects)
m <- m[m$datetime > ymd("2020-01-01"),]


## add weather data
temp <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_tu_stunde_20070401_20211231_00044.txt")
wind <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_ff_stunde_20201001_20211231_15801.txt")

temp$TT_TU[temp$TT_TU < -200] <- NA
temp$time <- ymd_h(temp$MESS_DATUM, tz = "CET")

wind$time <- ymd_h(wind$MESS_DATUM, tz = "CET")

# plot(temp$time, temp$TT_TU)
plot(wind$time, wind$F)
m$temperature <- NA
m$wind <- NA

for(i in 1:nrow(m)){
  tidx <- which.min(abs(temp$time - (m$datetime[i]-2*3600)))
  widx <- which.min(abs(wind$time - (m$datetime[i]-2*3600)))
  # m$time[i]
  # temp[tidx,]
  # wind[widx,]
  m$temperature[i] <- temp$TT_TU[tidx]
  m$wind[i] <- wind$F[widx]
}

m$yday <- yday(m$datetime)
summary(m)
table(m$meadow)

m <- m[m$meadow != "Guettingen",]

M <- m %>% group_by(meadow, datetime, yday, year, grassheight,
                    temperature, wind, min_since_sunset) %>%
  summarise(insects = mean(insects),
            buzz = mean(buzz),
            count = n())

g1 <- gam(insects~
            s(as.numeric(yday))+
            s(as.numeric(min_since_sunset))+
            s(grassheight)+
            s(temperature)+
            s(wind, k = 10)+
            as.factor(year)+meadow,
          data = M,
          family = negbin(theta = 0.62),
          method = "REML")
summary(g1)
plot(g1)
gam.check(g1)

M$buzz %>% table()
b1 <- gam(buzz~
            s(as.numeric(yday))+
            s(as.numeric(min_since_sunset))+
            s(grassheight)+
            s(temperature)+
            s(wind, k = 10)+
            as.factor(year)+meadow,
          data = M,
          family = negbin(theta = 0.62),
          method = "REML")
summary(b1)
plot(b1)
gam.check(b1)


corrplot(cor(M[,c("yday", "temperature", "wind", "min_since_sunset")]), method = "number")

save(M, m, g1, file = "insect_gam.robj")

load("insect_gam.robj")

dates <- unique(date(m$time))
i = 1
for(i in 1:length(dates)){
  with(m[date(m$time) == dates[i],], plot(min_since_sunset, wind,
                                            main = dates[i]))
}

with(m[date(m$time) == "2020-08-16",], plot(min_since_sunset, wind))

# suspect dates
m[date(m$time) == "2020-08-13",]

# "2020-08-14", "2020-08-18",  - only two points
# "2021-08-13", "2021-08-03", "2021-07-23", "2021-08-20", "2021-08-07", "2021-07-29", - few points

# ggplot(m, aes(min_since_sunset, temperature, col = site))+geom_point()+facet_wrap(~date(time))
# ggplot(m, aes(min_since_sunset, wind, col = site))+geom_point()+facet_wrap(~date(time))



names(m)









