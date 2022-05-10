library(pacman)
p_load(data.table, lubridate, tidyverse, mgcv)
bdf.final <- fread("../../../Dropbox/MPI/BatsandGrass/Data/bdf.final.csv")

bdf.final$mean.insects <- as.numeric(gsub(",", ".", gsub("\\.", "", bdf.final$mean.insects)))
bdf.final$mean.bats <- as.numeric(gsub(",", ".", gsub("\\.", "", bdf.final$mean.bats)))
bdf.final$mean.swarm <- as.numeric(gsub(",", ".", gsub("\\.", "", bdf.final$mean.swarm)))
bdf.final$minaftersunset2 <- as.numeric(gsub(",", ".", gsub("\\.", "", bdf.final$minaftersunset2)))

GAMbata <-bdf.final %>%
  filter (meadow.x%in% c ("Hockgraben","StKatharina","Institut","Voliere","Scheune","Tannenhof","Uni","StKatharina2","Hockgraben2","Mill")) %>%
  filter(wind<=71)  %>%
  #filter(minaftersunset2 >=-50)%>%
  #filter(found_insects >=66)%>%
  #filter(date=="20200817")%>%
  filter(Treatment %in% c("pre","mowing","post"))
 #filter(totalcount < 300)
GAMbata$mean.bats <- as.numeric(GAMbata$mean.bats)
GAMbata$meadow.x <- as.factor(GAMbata$meadow.x)
#GAMbata$platform <- as.factor(GAMbata$platform)
GAMbata$Treatment <- as.factor(GAMbata$Treatment)
GAMbata$totalcount <- as.numeric(GAMbata$totalcount)
GAMbata$wind <- as.numeric(GAMbata$wind)

##GAMbat$swarm <- as.numeric(GAMbat$swarm)


GAMbatb <-bdf.final %>%
  filter (meadow.x.x %in% c ("Hockgraben2","StKatharina2","Mill","Mindelsee","Guettingen","Bildwiese"))
 # filter(totalcount < 300)
#filter("mean.bats","mean.insects","minaftersunset2", "daysafteraugust2","Treatment","temperature","grassheight","moonphase","platform","wind")
#filter(minaftersunset2 >=-50)%>%
#filter(found_insects >=66)%>%
#filter(date=="20200817")%>%
#filter(Treatment=="post")%>%
# filter(totalcount < 300)
GAMbatb$mean.insects <- as.numeric(GAMbatb$found_insects)
GAMbatb$meadow.x.x <- as.factor(GAMbatb$meadow.x.x)
GAMbatb$platform <- as.factor(GAMbatb$platform)
GAMbatb$Treatment <- as.factor(GAMbatb$Treatment)
GAMbatb$totalcount <- as.numeric(GAMbatb$totalcount)
GAMbatb$mean.bats <- as.numeric(GAMbatb$mean.bats)
##GAMbat$swarm <- as.numeric(GAMbat$swarm)

#batcalls

GAMbat1 <- gam(mean.bats  ~  s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) +
                 s(mean.insects,k=5) , data = GAMbata,
               family = negbin(theta = 1.0,),method="REML",scale = 0)
plot(GAMbat1)

GAMbat2 <- gam(mean.bats  ~s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) +
                 s(platform ,k=5,bs="re") , data = GAMbata,
               family = negbin(theta = 1.0,link="log"),method="REML",scale = 0)

GAMbat3 <- gam(mean.bats  ~ s(found_insects,k=5) + s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(platform ,k=5,bs="re") + s(temperature ,k=5)  + meadow.x.x + Treatment , data = GAMbata, family = negbin(theta =1.0 ,link="log"),method="REML",scale = 0)

GAMbat4 <- gam(mean.bats  ~ s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(grassheight,k=5) + s(temperature ,k=5)  + meadow.x.x + Treatment + platform, data = GAMbata, family = negbin(theta = 1.0), method="REML",scale = 0)

GAMbat5 <- gam(mean.bats  ~ s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(grassheight,k=5) + s(temperature ,k=5)  + meadow.x.x + Treatment + platform, data = GAMbata, family = negbin(theta = 0.5), method="REML",scale = 0)


GAMbat6 <- gam(mean.bats  ~ relevel(Treatment,"pre") + s(mean.insects,k=3) + s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=3)+ s(grassheight,k=3) + s(platform,k=3,bs="re") + s(wind,k=3) + s(moonphase,k=3) + meadow.x.x, data = GAMbata,
            family = negbin(theta = 0.95),method="REML",scale=0)

#Noctules
GAMbat7 <- gam(Nycnoc  ~ s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) , data = GAMbata,
               family = negbin(theta = 0.95),method="REML",scale=0)

#Pippip
GAMbat8 <- gam(Pippip  ~ relevel(Treatment,"pre") + s(mean.insects,k=3) + s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=3)+ s(grassheight,k=3) + s(platform,k=3,bs="re") + s(wind,k=3) + s(moonphase,k=3) + meadow.x.x, data = GAMbata,
               family = negbin(theta = 0.95),method="REML",scale=0)







#scent experiment
GAMbat7 <- gam(mean.bats  ~ relevel(Treatment,"scent") + s(mean.insects,k=5)  + s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(temperature,k=5) + s(grassheight,k=5) + s(platform,k=5,bs="re") + s(wind,k=5) + s(moonphase,k=5) , data = GAMbatb,
             family = negbin(theta = 0.95),method="REML",scale=0)


#pre vs. post mowing
GAMbat8 <- gam(mean.bats  ~ relevel(Treatment,"mowing") +  s(mean.insects,k=3)  + s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=3) + s(grassheight,k=3) + s(platform,k=3,bs="re") + s(wind,k=3) + s(moonphase,k=3) , data = GAMbata,
               family = negbin(theta = 0.95),method="REML",scale=0)



gam.check(GAMbat8)
overdispersion <- GAMbat7$deviance /GAMbat7$df.residual
overdispersion


p<- predict(GAMbat3, GAMbat4, type = "link", se.fit = TRUE)
plot(residuals.gam())


par(mfrow = c(1, 2))




plot(GAMbat10,all.terms=TRUE)

plot(GAMbat12, select = 1,ylab="bat activity (calls per minute)" , xlab="insects", shade=TRUE,shade.col="lightpink",ylim=c(-5,5))
plot(GAMbat12, select = 2,ylab="residuals (bat activity (calls per minute))" , xlab="days after august 10th", shade=TRUE,shade.col="lightpink",main="season",ylim=c(-70,20))
plot(GAMbat12, select = 3, ylab="residuals (bat activity (calls per minute))",xlab="minutes after sunset", shade=TRUE,shade.col="lightpink",main="night",ylim=c(-10,5))
plot(GAMbat10, select = 4, ylab="residuals (bat activity (calls per minute))",xlab="temperature", shade=TRUE,shade.col="lightpink",ylim=c(-8,3))
plot(GAMbat10, select = 5, ylab="residuals (bat activity (calls per minute))",xlab="grassheight", shade=TRUE,shade.col="lightpink",ylim=c(-10,15))
plot(GAMbat10, select = 6, ylab="residuals (bat activity (calls per minute))",xlab="wind", shade=TRUE,shade.col="lightpink",ylim=c(-10,10))
plot(GAMbat10, select = 7, ylab="residuals (bat activity (calls per minute))",xlab="moonphase", shade=TRUE,shade.col="lightpink",ylim=c(-25,50))



plot(GAMtest4, residuals = TRUE)
plot(GAMtest8$gam, pages=1)
summary(GAMbat7)
summary(GAMbat8)
gam.check(GAMtest4)
gam.check(GAMbat8)
AIC(GAMbat7,GAMbat8,GAMbat9)
#with nas

 # delta aic

GAMbat8 <- gam(mean.bats  ~  s(mean.insects,k=3)+s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=5) + s(grassheight,k=5) + s(wind,k=5) + s(moonphase,k=5) + s(meadow.x,k=5,bs="re"), data = GAMbata,
               family = negbin(theta = 1.35),method="REML",scale=0,df=5)



GAMbat9 <- gam(mean.bats  ~  s(mean.insects,k=3)+s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=5) + s(grassheight,k=5) + s(wind,k=5) + s(meadow.x,k=5,bs="re"), data = GAMbata,
               family = negbin(theta = 1.35),method="REML",scale=0,df=5)



GAMbat10 <- gam(mean.bats  ~  s(mean.insects,k=3)+s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=5) + s(wind,k=5) + s(moonphase,k=5) + s(meadow.x,k=5,bs="re"), data = GAMbata,
               family = negbin(theta = 1.35),method="REML",scale=0,df=5)


GAMbat11 <- gam(mean.bats  ~  s(mean.insects,k=3)+s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(grassheight,k=5) + s(wind,k=5) + s(moonphase,k=5) + s(meadow.x,k=5,bs="re"), data = GAMbata,
                family = negbin(theta = 1.35),method="REML",scale=0,df=5)

GAMbat12 <- gam(mean.bats  ~  s(mean.insects,k=3)+s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=5) +s(meadow.x,k=5,bs="re"), data = GAMbata,
                family = negbin(theta = 1.35),method="REML",scale=0,df=5)

AIC(GAMbat8,GAMbat9,GAMbat10,GAMbat11)
install.packages("AICcmodavg")
library(AICcmodavg)
library(lme4)


models <- list(GAMbat8, GAMbat9, GAMbat10, GAMbat11,GAMbat12)
all_ms_glm <- lapply(models, glm.convert)
model.names <- c('8', '9', '10', '11','12')
cand.set = models
modnames = model.names
AIC(models)
AICcmodavg::aictab(cand.set = all_ms_glm,modnames = model.names,second.ord=TRUE,nobs=NULL,sort=TRUE)

#anpassen der smoothings
gam.check(GAMbat9)
summary(GAMbat13)

#GAMbat9 = pre

GAMbat12 <- gam(mean.bats  ~ relevel(Treatment,"pre")+ relevel(meadow.x,"Hockgraben")+s(mean.insects,k=3)+s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=5) + s(grassheight,k=5) + s(wind,k=5) + s(moonphase,k=5) + s(meadow.x,k=5,bs="re"), data = GAMbata,
                family = negbin(theta = 0.3),method="REML",scale=0,df=5)

GAMbat13 <- gam(mean.bats  ~ relevel(Treatment,"pre")+ s(mean.insects,k=3)+s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=5) + s(grassheight,k=5) + s(wind,k=5) + s(moonphase,k=5) + s(meadow.x,k=5,bs="re"), data = GAMbata,
                family = negbin(theta = 0.3),method="REML",scale=0,df=5)
AIC(GAMbat10,GAMbat11,GAMbat12)

plot(GAMbat12,all.terms=TRUE)
