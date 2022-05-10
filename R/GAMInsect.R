

GAMInsects <-bdf.final %>%
  filter (meadow.x %in% c ("Hockgraben","StKatharina","Scheune","Uni","Tannenhof","Voliere")) %>%
  #filter(minaftersunset2 >=-50)%>%
  filter(wind<=71)
  #filter(found_insects >=66)%>%
  #filter(date=="20200817")%>%
 #filter(Treatment=="pre")
  #filter(Treatment %in% c("mowing","post"))%>%
GAMInsects$found_insects <- as.numeric(GAMInsects$found_insects)
GAMInsects$meadow.x <- as.factor(GAMInsects$meadow.x)
#GAMInsects$platform <- as.factor(GAMInsects$platform)
GAMInsects$Treatment <- as.factor(GAMInsects$Treatment)
GAMInsects$totalcount <- as.numeric(GAMInsects$totalcount)
GAMInsects$swarm <- as.numeric(GAMInsects$swarm)
GAMInsects$daysafteraugust2 <- as.numeric(GAMInsects$daysafteraugust2)
GAMInsects$wind <- as.numeric(GAMInsects$wind)
GAMInsects$moonphase <- as.numeric(GAMInsects$moonphase)

GAMInsects$truebatdata[is.na(GAMInsects$totalcount)] <- 0
GAMInsects$mean.bats[is.na(GAMInsects$mean.bats)] <- 0

###
with(GAMInsects[GAMInsects$meadow.x.x == "StKatharina" & GAMInsects$date.x.x == "20200811",],
       plot(minaftersunset2, mean.insects,
                      # xlim = c(0,500),
                      col = rgb(0,1,0,.5)))
with(GAMInsects[GAMInsects$meadow.x.x == "StKatharina" & GAMInsects$date.x.x == "20200811",],
     points(minaftersunset2, mean.bats/50,
          # xlim = c(0,500),
          col = rgb(1,0,0,.5)))

plot(ymd(GAMInsects$date.x.x), GAMInsects$temperature)
plot(ymd(GAMInsects$date.x.x), GAMInsects$mean.insects)

####
loc <- "Scheune"
date <- "20200812"

with(GAMInsects[GAMInsects$meadow.x.x == loc & GAMInsects$date.x.x == date,],
     plot(minaftersunset2, mean.insects,
          # xlim = c(0,500),
          col = rgb(0,1,0,.5)))
with(GAMInsects[GAMInsects$meadow.x.x == loc & GAMInsects$date.x.x == date,],
     points(minaftersunset2, mean.bats/10,
            # xlim = c(0,500),
            col = rgb(1,0,0,.5)))


unique(paste0(GAMInsects$date.x.x, GAMInsects$meadow.x.x))

ggplot(GAMInsects, aes(x = minaftersunset2))+geom_point()

GAM1 <- gam(mean.insects ~
              s(daysafteraugust2, k=5, df=5) +
              s(minaftersunset2, k=5, df=5) +
              s(temperature, k=5, df=5),
              #s(platform, k=5, bs="re", df=5) ,
            data = GAMInsects,
            family = negbin(theta = 0.62),
            method = "REML", scale=0)

GAMInsects$meadow.x
GAM2 <- gam(mean.insects  ~
              s(totalcount) +
              s(daysafteraugust2) +
              s(minaftersunset2) +
              s(temperature)+
              s(wind)+
              s(grassheight)+
              s(meadow.x,bs = 're'),
            data = GAMInsects[GAMInsects$totalcount < 200,],
            family = negbin(theta = 0.62,link = "log"),
            method="REML",scale = 0)
plot(GAM2)

ggplot(data = GAMInsects, aes(y = mean.insects, x = minaftersunset2))+
  geom_point()


GAM3 <- gam(mean.insects  ~
              s(daysafteraugust2,k=5) +
              s(minaftersunset2,k=5) +
              s(temperature,k=5) +
              s(grassheight,k=5) +
              # s(platform, k=5, bs="re") +
              s(Treatment, bs="fs"),
            data = GAMInsects,
          family = negbin(theta = 0.62, link = "log"),method="REML",scale = 0)
plot(GAM3)

GAM4 <- gam(mean.insects ~
              Treatment +
              s(daysafteraugust2, k=5) +
              s(temperature, k=5) +
              s(platform, k=5, bs="re")+
              s(grassheight, k=5),
            data = GAMInsects,
            family = negbin(theta = 0.62),
            method="REML", scale=0)

AIC(GAM2,GAM3,GAM4)

par(mfrow = c(1, 2))
plot(GAM8, all.terms=TRUE)
plot(GAM1)
plot(GAMtest4, residuals = TRUE)
plot(GAMtest8$gam, pages=1)
summary(GAM2)
summary(GAMtest5)
gam.check(GAMtest4)
gam.check(GAMtest5)

#GAM4



GAM2 <- gam(mean.insects  ~  s(TT_TU,k=4) + s(minaftersunset2,k=4) + s(platform,k=4,bs="re") , data = GAMInsects,
            family = negbin(theta = 1.45,link = "log"),method="REML",scale = 0)

plot(GAM2,
     select = 1,
     ylab="insect density" ,
     xlab="temperature",
     shade=TRUE,
     shade.col="lightpink",
     main="season",
     ylim=c(-10,10))

plot(GAM7, select = 2,
     ylab="insect density",xlab="minutes after sunset",
     shade=TRUE,shade.col="lightpink",
     main="night",ylim=c(-2,2.5))

GAM3 <- gam(mean.insects  ~s(daysafteraugust2,k=4) +
              s(minaftersunset2,k=4) +
              s(TT_TU,k=4) +
              s(grassheight,k=4) +
              s(platform,k=4,bs="re") + Treatment, data = GAMInsects,
            family = negbin(theta = 1.45,link = "log"),method="REML",scale = 0)


GAM4 <- gam(mean.insects  ~s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=3) + s (grassheight,k=3) + s(platform,k=3,bs="re") + Treatment + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 0.95,link = "log"),method="REML",scale = 0)


GAM5 <- gam(mean.insects  ~s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=3) + s (grassheight,k=3) + s(platform,k=3,bs="re") + s(wind,k=3)+ s(moonphase,k=3) + meadow.x.x + Treatment , data = GAMInsects,
            family = negbin(theta = 0.95,link = "log"),method="REML",scale = 0)

GAM7 <- gam(mean.insects  ~ relevel(Treatment,"pre") + s(daysafteraugust2,k=4) + s(minaftersunset2,k=4) + s(TT_TU,k=4) + s(grassheight,k=4) + s(platform,k=4,bs="re") + s(wind,k=4) + s(moonphase,k=4) + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0)


#fit best model with k

GAM8 <- gam(mean.insects  ~ relevel(Treatment,"pre") + s(daysafteraugust2,k=9) + s(minaftersunset2,k=50) + s(TT_TU,k=30) + s(grassheight,k=20) + s(platform,k=5,bs="re") + s(wind,k=9) + s(moonphase,k=12) + meadow.x.x, data = GAMInsects, family = negbin(theta = 0.95),method="REML",scale=0)


GAM9 <- gam(mean.insects  ~ relevel(Treatment,"pre") + s(daysafteraugust2,k=5) + s(platform,k=5,bs="re") + s(wind,k=10) + s(moonphase,k=13) + meadow.x.x, data = GAMInsects, family = negbin(theta = 0.95),method="REML",scale=0)

GAM10 <- gam(mean.insects  ~ relevel(Treatment,"pre") + s(minaftersunset2,k=50) + s(platform,k=5,bs="re") + s(wind,k=9) + s(moonphase,k=12) + meadow.x.x, data = GAMInsects, family = negbin(theta = 0.95),method="REML",scale=0)

GAM11 <- gam(mean.insects  ~ relevel(Treatment,"pre") + s(TT_TU,k=30) + s(platform,k=5,bs="re") + s(wind,k=9) + s(moonphase,k=12) + meadow.x.x, data = GAMInsects, family = negbin(theta = 0.95),method="REML",scale=0)



gam.check(GAM7)
plot(GAM8, residuals = TRUE, pch = 1) #check/avoid overfitting with sp

#check for concurvity
concurvity(GAM9, full = FALSE)




GAM7 <- gam(mean.insects  ~  relevel(Treatment,"pre") + s(platform,bs="re") + s(daysafteraugust2,k=4) + s(minaftersunset2,k=4) + s(temperature,k=4) + s(grassheight,k=4) + s(wind,k=4) + s(moonphase,k=4) + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0)

GAM8 <- gam(mean.insects  ~  s(platform,bs="re") + s(daysafteraugust2,k=4) + s(minaftersunset2,k=4) + s(temperature,k=4) + s(grassheight,k=4) + s(wind,k=4) + s(moonphase,k=4) + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0)

GAM9 <- gam(mean.insects  ~ relevel(Treatment,"pre") + s(platform,bs="re") + s(daysafteraugust2,k=4) + s(minaftersunset2,k=4) + s(temperature,k=4) + s(grassheight,k=4) + s(wind,k=4) + s(moonphase,k=4) + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0)

hist(bdf.final$mean.insects, breaks= 100)
plot(GAM2, scale=0)
summary(GAM7)
boxplot(GAM5b,xlim=Treatment,ylim=mean.insects,data=GAMInsects)

AIC(GAM3,GAM7)

plot(GAM8, select = 1,ylab="residuals (insects)" , xlab="days after august 10th", shade=TRUE,shade.col="lightpink",main="season",ylim=c(-20,20))
plot(GAM8, select = 2, ylab="residuals (insects)",xlab="minutes after sunset", shade=TRUE,shade.col="lightpink",main="night",ylim=c(-5,5))
plot(GAM8, select = 3, ylab="residuals (insects)",xlab="temperature", shade=TRUE,shade.col="lightpink",ylim=c(-5,5))
plot(GAM8, select = 4, ylab="residuals (insects)",xlab="grassheight", shade=TRUE,shade.col="lightpink",ylim=c(-5,5))
plot(GAM8, select = 5, ylab="residuals (insects)",xlab="wind", shade=TRUE,shade.col="lightpink",ylim=c(-5,5))
plot(GAM8, select = 6, ylab="residuals (insects)",xlab="moonphase", shade=TRUE,shade.col="lightpink",ylim=c(-10,10)) #alle tage miteinbeziehen!!

overdispersion <- GAM6$deviance /GAM6$df.residual
overdispersion

predict(GAM4)
plot(GAM7, seWithMean = TRUE,shift = coef(GAM7)[1])


GAM7 <- gam(mean.insects  ~  relevel(Treatment,"pre") + relevel(meadow.x,"Hockgraben") + s(daysafteraugust2,k=4) + s(minaftersunset2,k=4) + s(temperature,k=4) + s(grassheight,k=4) + s(wind,k=4) + s(moonphase,k=4) + meadow.x., data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0)



GAM8 <- gam(mean.insects  ~  s(daysafteraugust2,k=3) + s(minaftersunset2,k=6)  + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0,df=5)

GAM9 <- gam(mean.insects  ~  s(daysafteraugust2,k=4) + s(minaftersunset2,k=6) + s(temperature,k=5) + s(grassheight,k=6) + s(wind,k=6) + s(moonphase,k=6) + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0,df=5)

GAM10 <- gam(mean.insects  ~  relevel(Treatment,"pre")  + s(daysafteraugust2,k=4) + s(minaftersunset2,k=6) + s(temperature,k=5) + s(grassheight,k=6) + s(wind,k=6) + s(moonphase,k=6) + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0,df=5)





cor(GAMInsects [,c("mean.insects","daysafteraugust2","minaftersunset2","wind","temperature","grassheight","moonphase") ], method = "pearson", use = "complete.obs")
gam.check(GAM8)
summary(GAM7)
AIC(GAM9,GAM10)
plot.gam(GAM7)


#erstellen delta aic

GAM7 <- gam(mean.insects  ~  relevel(meadow.x,"Hockgraben") + s(daysafteraugust2,k=4) + s(minaftersunset2,k=4) + s(temperature,k=4) + s(grassheight,k=4) + s(wind,k=4) + s(moonphase,k=4) + + s(meadow.x,k=5,bs="re"), data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0)

GAM8 <- gam(mean.insects  ~  relevel(Treatment,"pre") + s(daysafteraugust2,k=4) + s(minaftersunset2,k=4) + s(temperature,k=4) + s(grassheight,k=4) + s(wind,k=4) + s(moonphase,k=4) + + s(meadow.x,k=5,bs="re"), data = GAMInsects,
            family = negbin(theta = 1.45),method="REML",scale=0)



plot(GAM7,all.terms=TRUE)
AIC(GAMbat8,GAMbat9,GAMbat10,GAMbat11)
install.packages("AICcmodavg")
library(AICcmodavg)
library(lme4)


models <- list(GAM7, GAM8, GAM9, GAM10,GAM11)
all_ms_glm <- lapply(models, glm.convert)
model.names <- c('7', '8', '9', '10','11')
cand.set = models
modnames = model.names
AIC(models)
AICcmodavg::aictab(cand.set = all_ms_glm,modnames = model.names,second.ord=TRUE,nobs=NULL,sort=TRUE)

