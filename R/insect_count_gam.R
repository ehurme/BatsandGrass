# Insect count ----

# 1 load libraries ----

library(pacman)
p_load(data.table, lubridate, tidyverse, ggplot2,
       mgcv, suncalc, janitor, corrplot, GGally,
       hablar,  # prevents summarise from returning -Inf when getting max(NA)
       ggpubr,
       MASS, lme4)

# 2 Load data ---------------------------------------------------------------
## 2.1 2020 ----

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

## 2.2 2021 ----

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
df$meadow[df$loc == "UK"] <- "Uni"
df$meadow[df$loc == "V"] <- "Voliere"
df$meadow[df$loc == "W"] <- "Wollmatingen"
df$meadow[df$loc == "M"] <- "Mill"

## how many pictures have insects?
hist(df$insects, breaks = 1000, xlim = c(0,10))

sum(df20$insects)

sum(df$insects == 0)/nrow(df) # 68% of minutes monitored are empty

sum(df$insects >= 10)/nrow(df) # 2.1% of minutes contain swarms
summary(df$insects)

## 2.3 Join buzz count ----

load("../../../Dropbox/MPI/BatsandGrass/Data/buzz_count.robj")
buzz_df$year <- as.numeric(buzz_df$year)
buzz_df$buzz %>% table
buzz_df$loc <- substr(buzz_df$loc, 1, (nchar(buzz_df$loc)-1))

buzz_df$meadow %>% table
df$meadow %>% table

buzz_df$buzz %>% is.na %>% which
(buzz_df$buzz != 0) %>% which %>% length/nrow(buzz_df)
max(buzz_df$buzz)
m <- full_join(df[,c(2:3, 5:7,9)],
               buzz_df[,-c("date", "loc", "time", "yday", "filenames")],
               by = c("year", "datetime", "meadow", "location"))
m %>% nrow # 54141
m$buzz %>% table
m$buzz %>% sum(na.rm = TRUE) #429
m$insects %>% table
m$insects %>% sum(na.rm = TRUE) # 44948

which(!is.na(m$insects) & !is.na(m$buzz)) %>% length
# m[which(!is.na(m$insects) & is.na(m$buzz)),] %>% View()
table(paste0(m$datetime, m$meadow)) %>% hist
which(table(paste0(m$datetime, m$meadow)) == 4)
m[which(m$datetime == ymd_hms("2021-07-20 22:01:00") &
          m$meadow == "Uni"),]
m[which(m$datetime == ymd_hms("2020-08-11 22:44:00") &
          m$meadow == "StKatharina"),]
m %>% nrow() # 61454
## 2.4 add sunset times ----
dates <- unique(date(m$datetime))
dates <- na.omit(dates[order(dates)])
sunset <- suncalc::getSunlightTimes(date = seq.Date(dates[1], dates[length(dates)], by = 1), lat = 47.6904, lon = 9.1869, tz = "CET")
m$sunset <- ymd_hms("2000-01-01 12:00:00", tz = "CET")

i = 17068
for(i in 1:nrow(m)){
  idx <- which.min(abs(m$datetime[i] - sunset$sunset))
  m$sunset[i] <- sunset$sunset[idx]
}

m$min_since_sunset <- round(as.numeric(m$datetime - (m$sunset+2*3600))/60, 0)
range(m$min_since_sunset, na.rm = TRUE)
# m[which(m$min_since_sunset < 0),] %>% View

hist((m$min_since_sunset), breaks = 1000)

plot(m$datetime, m$insects)
m <- m[m$datetime > ymd("2020-01-01"),]


## 2.5 add weather data ----
temp <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_tu_stunde_19710101_20211231_02712.txt")
wind <- fread("../../../Dropbox/MPI/BatsandGrass/Data/produkt_ff_stunde_19590701_20211231_02712.txt")

temp$TT_TU[temp$TT_TU < -200] <- NA
temp$time <- ymd_h(temp$MESS_DATUM, tz = "CET")
temp <- temp[temp$time > min(m$datetime, na.rm = TRUE),]

wind$time <- ymd_h(wind$MESS_DATUM, tz = "CET")
wind <- wind[wind$time > min(m$datetime, na.rm = TRUE),]

plot(wind$time, wind$F)
m$temperature <- NA
m$wind <- NA
i = 1

for(i in 1:nrow(m)){
  try({
    tidx <- which.min(abs(temp$time - (m$datetime[i]-2*3600)))
    widx <- which.min(abs(wind$time - (m$datetime[i]-2*3600)))
    m$datetime[i]
    temp[tidx,]
    wind[widx,]
    m$temperature[i] <- temp$TT_TU[tidx]
    m$wind[i] <- wind$F[widx]
  })
}
m$yday <- yday(m$datetime)
# save(m, df, df21, df20, file = "insect_buzz_with_weather.robj")

summary(m)
table(m$meadow)

m <- m[m$meadow != "Guettingen",]
m$meadow[m$meadow == "St.Katharina"] <- "StKatharina"
m$meadow[m$meadow == "Institutswiese"] <- "Institute"
m$meadow[m$meadow == "Scheunenwiese"] <- "Scheune"
m$meadow %>% table

## 2.6 prep data for models ----
m$datetime <- round_date(m$datetime, unit = "min")
table(paste0(m$datetime, m$meadow)) %>% hist
m[which(m$datetime == ymd_hms("2020-08-11 22:44:00") &
          m$meadow == "StKatharina"),] %>% View()
m %>% nrow # 54139
m <- m[which(!is.na(m$meadow)),]

M <- m %>% group_by(meadow, datetime, yday, year, min_since_sunset) %>%
  summarise(insects = round(max(s(insects)),0),
            buzz = round(max(s(buzz)),0),
            count = n(),
            grass = max(s(grassheight)),
            temp = max(s(temperature)),
            windsp = max(s(wind)))
summary(M)
summary(m)

which(duplicated(paste0(M$meadow, M$datetime)))

# remove recordings past
M_clean <- M[M$min_since_sunset <= 150,]
M_clean <- M_clean[M_clean$min_since_sunset >= -30,]
paste0(M_clean$meadow, date(M_clean$datetime)) %>% table %>% length
  hist(main = "minutes sampled", breaks = 30)

M_clean$meadow[which(!is.na(M_clean$buzz))] %>% table
paste0(M_clean$meadow[which(!is.na(M_clean$buzz))],
       date(M_clean$datetime[which(!is.na(M_clean$buzz))])) %>% table  %>% length

# idx <- which(paste0(M_clean$meadow, date(M_clean$datetime)) == "Scheune2020-08-10")
# M_clean[idx,] %>% View()

## 2.7 Summarize insect data ----
sum(M_clean$insects > 0, na.rm = TRUE)/length(which(!is.na(M_clean$insects)))
## 49.9% of 16980 minutes monitored have insects present (combined between cameras at a site)
sum(m$insects > 0, na.rm = TRUE)/length(which(!is.na(m$insects)))
## 31.7% of 42324 minutes monitored (multiple platforms) had insects present

sum(M_clean$insects >= 10, na.rm = TRUE)/length(which(!is.na(M_clean$insects)))
## 4.2% of 16980 minutes have swarms
max(M_clean$insects, na.rm = TRUE) # 105 max swarm size
sum(m$insects >= 10, na.rm = TRUE)/length(which(!is.na(m$insects)))
## 2% of 42324 minutes monitored (multiple platforms) have swarms of insects

## 2.8 Summarize buzz data ----
sum(M_clean$buzz > 0, na.rm = TRUE)/length(which(!is.na(M_clean$buzz)))
## 1.9% of 14025 minutes have buzzes
sum(m$buzz > 0, na.rm = TRUE)/length(which(!is.na(m$buzz)))
## 0.9% of 33618 minutes have buzzes

### seems high. How many points are filtered out?
## m
c(which(m$min_since_sunset[which(!is.na(m$buzz))] < -30),
  which(m$min_since_sunset[which(!is.na(m$buzz))] > 150)) %>% length
# 7112 out
c(which(m$min_since_sunset[which(!is.na(m$buzz))] > -30),
  which(m$min_since_sunset[which(!is.na(m$buzz))] < 150)) %>% length
# 59896 in
## M
c(which(M$min_since_sunset[which(!is.na(M$buzz))] < -30),
  which(M$min_since_sunset[which(!is.na(M$buzz))] > 150)) %>% length
# 3717 out
c(which(M$min_since_sunset[which(!is.na(M$buzz))] > -30),
  which(M$min_since_sunset[which(!is.na(M$buzz))] < 150)) %>% length
# 31643 in

31643/59896
#1/2 is right

m$buzz[which(!is.na(m$buzz))]

M_clean$buzz %>% table()
m$buzz %>% table()


### Figure 1
#### Histograms insects buzzes
layout(c(1,2))
par(mar = c(4,4,1,2))
M_clean$insects %>% hist(breaks = 100, main = "",
                         xlab = "insects per min")
M_clean$buzz %>% hist(breaks = 100, main = "", xlab = "buzzes per min")

M_clean$insects %>% log() %>% hist(breaks = 100, main = "", xlab = "log(insects per min)")
M_clean$buzz %>% log() %>% hist(breaks = 100, main = "", xlab = "log(buzzes per min)")


# 3 Insect summary ----
Mday <- M_clean %>% group_by(meadow, year, yday) %>%
  summarise(temperature = mean(temp, na.rm = TRUE),
            wind = mean(windsp, na.rm = TRUE),
            grassheight = mean(grass, na.rm = TRUE),
            start = min(datetime),
            end = max(datetime),
            length = n(),
            insect_sum = sum(insects, na.rm = TRUE),
            insect_max = max(s(insects)),
            insect_peak = NA, # datetime[which.max(insects)],
            insect_start = min(s(datetime)),
            insect_end = max(s(datetime)),
            insect_duration = length(which(!is.na(insects))),
            swarm_sum = sum(insects >= 10, na.rm = TRUE),
            buzz_sum = sum(buzz, na.rm = TRUE),
            buzz_max = max(s(buzz)),
            buzz_peak = NA, # datetime[which.max(buzz)],
            buzz_start = min(s(datetime)),
            buzz_end = max(s(datetime)),
            buzz_duration = sum(!is.na(buzz)))
i = 1

## 3.1 time btwn pks ----
for(i in 1:nrow(Mday)){
  idx <- which(Mday$meadow[i] == M_clean$meadow &
                 date(Mday$start[i]) == date(M_clean$datetime))
  datetime <- M_clean$datetime[idx]
  try(Mday$insect_peak[i] <- as.character(datetime[which.max(M_clean$insects[idx])]))
  try(Mday$buzz_peak[i] <- as.character(datetime[which.max(M_clean$buzz[idx])]))
}
Mday$time_btw_peaks <- ((ymd_hms(Mday$buzz_peak) - ymd_hms(Mday$insect_peak))/60) %>% as.numeric

Mday$time_btw_peaks %>% summary
layout(1)
hist(Mday$length, xlab = "minutes monitored", main = "")
Mday <- Mday[Mday$length > 100,]

Mday$insect_max[Mday$insect_max < 0] <- NA
Mday$buzz_max[Mday$buzz_max < 0] <- NA

Mday$swarm_per_min <- Mday$swarm_sum/Mday$insect_duration
Mday$insect_per_min <- Mday$insect_sum/Mday$insect_duration
Mday$buzz_per_min <- Mday$buzz_sum/Mday$buzz_duration

summary(Mday)

Mday$meadow %>% unique

sum(Mday$insect_sum)/sum(Mday$insect_duration)
sum(Mday$buzz_sum)/sum(Mday$buzz_duration)

p1 <- ggplot(Mday, aes(y = insect_per_min, x = meadow))+geom_violin()+
  ylab("insects per min")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2 <- ggplot(Mday, aes(y = buzz_per_min, x = meadow))+geom_violin()+
  ylab("buzzes per min")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggarrange(p1,p2, ncol = 1)

p1 <- ggplot(Mday, aes(y = insect_sum, x = meadow))+geom_violin()+
  ylab("# of insects")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2 <- ggplot(Mday, aes(y = buzz_sum, x = meadow))+geom_violin()+
  ylab("# of buzzes")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggarrange(p1,p2, ncol = 1)


ggplot(Mday, aes(y = insect_sum, x = buzz_sum))+geom_point()+
  geom_smooth(method = "lm", col = "black")+
  ylab("# of insects")+ xlab("# of buzzes")+
  theme_classic()

hist(Mday$insect_sum %>% log)

## 3.2 Model insect vs buzz ----
fit <- glm(insect_sum ~ buzz_sum+temperature+wind+grassheight,
           data = Mday[Mday$insect_sum > 0,])
fitl <- glm(log(insect_sum) ~ buzz_sum+temperature+wind+grassheight,
            data = Mday[Mday$insect_sum > 0,])
fitlm <- lmer(log(insect_sum)~buzz_sum+temperature+wind+grassheight+
              (1|meadow),
              data = Mday[Mday$insect_sum > 0,])
fitnb <- glm.nb(insect_sum~buzz_sum+temperature+wind+grassheight,
           data = Mday[Mday$insect_sum > 0,])

summary(fitl)
AIC(fit, fitl, fitlm, fitnb)


with(Mday[Mday$insect_sum > 0 & Mday$buzz_sum > 0,],
     difftime(buzz_peak, insect_peak, units = "hours")) %>%
       as.numeric %>% hist(breaks = 100, main = "",
                           xlab = "hours between insect and buzz peaks")

with(Mday[Mday$insect_sum > 0 & Mday$buzz_sum > 0,],
     difftime(buzz_peak, insect_peak, units = "hours")) %>%
  as.numeric %>% summary

p1 <- ggplot(Mday, aes(x = yday, y = wind, col = factor(year)))+
  geom_point()+theme_classic()
p2 <- ggplot(Mday, aes(x = yday, y = temperature, col = factor(year)))+
  geom_point()+theme_classic()
p3 <- ggplot(Mday, aes(x = yday, y = grassheight, col = factor(year)))+
  geom_point()+theme_classic()
ggarrange(p1,p2,p3, ncol = 1)


ggplot(Mday, aes(x = swarm_sum/insect_duration,
                 y = buzz_sum/buzz_duration))+
  geom_point()+geom_smooth(method = "lm")

ggplot(Mday, aes(x = insect_sum/insect_duration,
                 y = buzz_sum/buzz_duration))+
  geom_point()+geom_smooth(method = "lm")
## 3.3 pairs plot ----
psych::pairs.panels(Mday[,c("yday", "temperature", "wind", "grassheight",
                            "insect_per_min", "swarm_per_min", "buzz_per_min")])

## 3.4 Save data ----
save(m, M, M_clean, Mday, file = "clean_merged_data.robj")
load("clean_merged_data.robj")


layout(rbind(1,2,3))
plot(Mday$yday, Mday$temperature, col = Mday$year)
plot(Mday$yday, Mday$wind, col = Mday$year)
plot(Mday$yday, Mday$grassheight, col = Mday$year)

layout(rbind(1,2))
plot(Mday$yday, Mday$insects, col = Mday$year)
plot(Mday$yday, Mday$buzz, col = Mday$year)

M_clean$insects %>% range(na.rm = TRUE)

layout(1)
M_clean$insects %>% hist(breaks = 1000, main = "")
M_clean$insects %>%
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



# 4 insect GAM ----
g1 <- gam(insects~
            #s(yday)+
            s(min_since_sunset)+
            s(grass)+
            s(temp)+
            s(windsp)+
            #s(buzz)+
            factor(year)+ #, bs = "re")+
            meadow,# bs = "re"),
          data = M_clean,
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
            #s(yday)+
            s(min_since_sunset, k = 5)+
            s(grass, k = 5)+
            s(temp, k = 5)+
            s(windsp, k = 5)+
            factor(year)+
            meadow,
          data = M_clean,
          family=ziP(theta=thb))

g0 %>% summary
plot(g0, pages = 1, unconditional = TRUE, rug = TRUE, scale = 0)
gam.check(g0, pages = 1)

AIC(g1, g0)

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


layout(rbind(1,2))
M$insects %>% hist(breaks = 1000, main = "")
M$buzz %>% hist(breaks = 1000, main = "")




### day models
d1 <- gam(insects~
            s(yday)+
            s(temp)+
            s(wind)+
            s(grassheight)+
            # s(buzz)+
            meadow+
            year,
          data = Mday)
plot(d1, pages = 1, #unconditional = TRUE, rug = TRUE,
     #all.terms = TRUE, #seWithMean = TRUE,
     scale = 0, shift = coef(d1)[1])
gam.check(d1)

Mday$insects %>% round(0) %>%  table

concurvity(d1, full = FALSE)
  # temp and day
  # temp and wind
  # wind and day
















# 6 Buzz GAM ----
## 6.1 By time ----

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


save(M, m, Mday, g0, g1, g2, b0, b1, b2, file = "../../../Dropbox/MPI/BatsandGrass/Data/insect_gam.robj")

load("../../../Dropbox/MPI/BatsandGrass/Data/insect_gam.robj")




