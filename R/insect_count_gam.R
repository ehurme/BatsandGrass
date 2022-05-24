# Insect count

## load libraries
library(pacman)
p_load(data.table, lubridate, tidyverse, ggplot2,
       mgcv, suncalc, janitor, corrplot, GGally)

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
buzz_df$buzz %>% table

# monitoring data
m <- full_join(df, buzz_df[,-c("loc", "date", "time", "yday")],
               by = c("year", "datetime", "meadow", "location"))

which(!is.na(m$insects) & !is.na(m$buzz))

## get sunset times
dates <- unique(date(m$datetime))
dates <- na.omit(dates[order(dates)])
sunset <- suncalc::getSunlightTimes(date = seq.Date(dates[1], dates[length(dates)], by = 1), lat = 47.6904, lon = 9.1869, tz = "CET")
m$sunset <- ymd_hms("2000-01-01 12:00:00", tz = "CET")

i = 17068
for(i in 1:nrow(m)){
  idx <- which.min(abs(m$datetime[i] - sunset$sunset))
  m$sunset[i] <- sunset$sunset[idx]
}

m$min_since_sunset <- as.numeric(m$datetime - (m$sunset+2*3600))/60
range(m$min_since_sunset)
m <- m[-which(m$min_since_sunset < -30),]
# m[which(m$min_since_sunset < 0),] %>% View

hist((m$min_since_sunset), breaks = 1000)

plot(m$datetime[1:100], m$min_since_sunset[1:100])

plot(m$datetime, m$insects)
m <- m[m$datetime > ymd("2020-01-01"),]


## add weather data
temp <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_tu_stunde_19710101_20211231_02712.txt")
wind <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_ff_stunde_19590701_20211231_02712.txt")

temp$TT_TU[temp$TT_TU < -200] <- NA
temp$time <- ymd_h(temp$MESS_DATUM, tz = "CET")

wind$time <- ymd_h(wind$MESS_DATUM, tz = "CET")

# plot(temp$time, temp$TT_TU)
plot(wind$time, wind$F)
m$temperature <- NA
m$wind <- NA
i = 1

for(i in 1:nrow(m)){
  tidx <- which.min(abs(temp$time - (m$datetime[i]-2*3600)))
  widx <- which.min(abs(wind$time - (m$datetime[i]-2*3600)))
  m$datetime[i]
  temp[tidx,]
  wind[widx,]
  m$temperature[i] <- temp$TT_TU[tidx]
  m$wind[i] <- wind$F[widx]
}

m$yday <- yday(m$datetime)
summary(m)
table(m$meadow)

m <- m[m$meadow != "Guettingen",]

############################################################
# prep data for models

M <- m %>% group_by(meadow, datetime, yday, year, grassheight,
                    temperature, wind, min_since_sunset) %>%
  summarise(insects = round(max(insects),0),
            buzz = round(max(buzz),0),
            count = n())
M$insects %>% range(na.rm = TRUE)

### Summarize insect data
sum(M$insects > 0, na.rm = TRUE)/length(which(!is.na(M$insects)))
sum(m$insects > 0, na.rm = TRUE)/length(which(!is.na(m$insects)))

sum(m$insects >= 10, na.rm = TRUE)/length(which(!is.na(m$insects)))
sum(M$insects >= 10, na.rm = TRUE)/length(which(!is.na(M$insects)))

layout(1)
M$insects %>% hist(breaks = 1000, main = "")
M$insects %>%
  na.omit %>%
  # log %>%
  density(bw = 0.5, n = 1000) %>%
  plot
M$meadow %>% table
M$meadow <- as.factor(M$meadow)
M$year <- as.factor(M$year)

# some grass heights seem like outliers
hist(M$grassheight[M$grassheight <80])
M <- M[-which(M$grassheight > 80),]


# plot data
psych::pairs.panels(M[,c("yday", "min_since_sunset", "grassheight", "temperature", "wind", "insects", "buzz")])
x11()

ggplot(m, aes(min_since_sunset, temperature, col = meadow))+geom_point()+facet_wrap(~date(datetime))
# ggplot(m, aes(min_since_sunset, wind, col = site))+geom_point()+facet_wrap(~date(time))

# remove dates with few points
which(table(date(M$datetime)) < 100) %>% names



g1 <- gam(insects~
            s(yday)+
            s(min_since_sunset)+
            s(grassheight)+
            s(temperature)+
            s(wind)+
            year+ #, bs = "re")+
            meadow,# bs = "re"),
          data = M,
          family = ziP(),#negbin(theta = 0.62),
          method = "REML")

g1$outer.info

summary(g1)
#layout(rbind(c(0,1,2), c(3,4,5)))
plot(g1, pages = 1, unconditional = TRUE, rug = TRUE,
     all.terms = TRUE, seWithMean = TRUE,
     scale = 0, shift = coef(g1)[1])
     # col = rgb(0,0,0,.01), pch = 1)
layout(rbind(c(1,2), c(3,4)))
gam.check(g1, pages = 1)
concurvity(g1, full = FALSE)

thb <- g1$family$getTheta()
g0 <- gam(insects~
            s(yday)+
            s(min_since_sunset)+
            s(grassheight)+
            s(temperature)+
            # s(wind)+
            s(year, bs = "re")+
            s(meadow, bs = "re"),
          data = M,
          family=ziP(theta=thb))

g0 %>% summary
plot(g0, pages = 1, unconditional = TRUE, rug = TRUE, scale = 0)
gam.check(g0, pages = 1)

g2 <- gam(insects~
            s(yday)+
            s(min_since_sunset)+
            s(grassheight)+
            s(temperature)+
            # s(wind)+
            s(year, bs = "re")+
            s(meadow, bs = "re"),
          data = M,
          family = ziP(b = .3),#negbin(theta = 0.62),
          method = "REML")

AIC(g0, g1, g2)
g0$outer.info

############################################################
# summarize buzz data
####

sum(M$buzz, na.rm = TRUE)/length(which(!is.na(M$buzz)))
sum(m$buzz, na.rm = TRUE)/length(which(!is.na(m$buzz)))

m$buzz[which(!is.na(m$buzz))]

M$buzz %>% table()
m$buzz %>% table()

layout(rbind(1,2))
M$insects %>% hist(breaks = 1000, main = "")
M$buzz %>% hist(breaks = 1000, main = "")

Mday <- M %>% group_by(meadow, year, yday) %>%
  summarise(temp = mean(temperature, na.rm = TRUE),
            wind = mean(wind, na.rm = TRUE),
            grassheight = mean(grassheight, na.rm = TRUE),
            insects = mean(insects, na.rm = TRUE),
            buzz = mean(buzz, na.rm = TRUE))
Mday

layout(rbind(1,2,3))
plot(Mday$yday, Mday$temp, col = Mday$year)
plot(Mday$yday, Mday$wind, col = Mday$year)
plot(Mday$yday, Mday$grassheight, col = Mday$year)

layout(rbind(1,2))
plot(Mday$yday, Mday$insects, col = Mday$year)
plot(Mday$yday, Mday$buzz, col = Mday$year)

b1 <- gam(buzz~
            s(yday)+
            s(min_since_sunset)+
            s(grassheight)+
            s(temperature)+
            # s(wind)+
            as.factor(year)+meadow,
          data = M,
          family = ziP(),#negbin(theta = 0.62),
          method = "REML")
b1$outer.info
summary(b1)
plot(b1, pages = 1, unconditional = TRUE, scale = 0)
M$buzz %>% table

layout(cbind(c(1,2), c(3,4)))
gam.check(b1)

range(predict(b1,type="response")[b1$y==0])

par(mfrow=c(2,2))
plot(predict(b1,type="response"),residuals(b1))
plot(predict(b1,type="response"),b1$y);abline(0,1,col=2)
plot(b1$linear.predictors,b1$y)
qq.gam(b1,rep=20,level=1)

thb <- b1$family$getTheta()
b0 <- gam(buzz~
            s(yday)+
            s(min_since_sunset)+
            s(grassheight)+
            s(temperature)+
            # s(wind)+
            as.factor(year)+meadow,
          data = M,
          family=ziP(theta=thb))

b0 %>% summary
plot(b0, pages = 1, unconditional = TRUE, scale = 0, rug = TRUE)
gam.check(b0, pages=1)

b2 <- gam(buzz~
            s(as.numeric(yday))+
            s(as.numeric(min_since_sunset))+
            s(grassheight)+
            s(temperature)+
            # s(wind)+
            as.factor(year)+meadow,
          data = M,
          family=ziP(b=.3))

b2 %>% summary

AIC(b0, b1, b2)


save(M, m, g0, g1, g2, b0, b1, b2, file = "../../../Dropbox/MPI/BatsandGrass/Data/insect_gam.robj")

load("../../../Dropbox/MPI/BatsandGrass/Data/insect_gam.robj")

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





names(m)









