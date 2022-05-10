GAMswarm <-bdf.final %>% 
  filter (meadow.x %in% c ("Hockgraben","StKatharina","Scheune","Uni","Tannenhof","Voliere")) %>%
  filter(wind<=71) %>% 
  #filter(minaftersunset2 >=-50)%>%
  #filter(found_insects >=66)%>%
  #filter(date=="20200817")%>%
  filter(Treatment=="pre")
  #filter(totalcount < 300)
GAMswarm$found_insects <- as.numeric(GAMswarm$found_insects)
GAMswarm$meadow.x <- as.factor(GAMswarm$meadow.x)
GAMswarm$platform <- as.factor(GAMswarm$platform)
GAMswarm$Treatment <- as.factor(GAMswarm$Treatment)
GAMswarm$totalcount <- as.numeric(GAMswarm$totalcount)
GAMswarm$mean.swarm <- as.numeric(GAMswarm$mean.swarm)
GAMswarm$daysafteraugust2 <- as.numeric( GAMswarm$daysafteraugust2)
GAMswarm$swarm <- as.numeric( GAMswarm$swarm)




swarm1 <- gam(swarm  ~  s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(temperature,k=5) + s(platform,k=5,bs="re") , data = GAMInsects,
            family = negbin(theta = 0.62),method="REML",scale=0)


swarm2 <- gam(swarm  ~  s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(platform,k=5,bs="re"), data = GAMInsects,
            family = negbin(theta = 0.62,link = "log"),method="REML",scale = 0)


swarm3 <- gam(swarm  ~s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(temperature,k=5) + s (grassheight,k=5) + s(platform,k=5,bs="re") + Treatment + meadow.x.x, data = GAMInsects,
            family = negbin(theta = 0.62,link = "log"),method="REML",scale = 0)


swarm4 <- gam(swarm  ~ Treatment + meadow.x.x + s(daysafteraugust2,k=5) + s(temperature,k=5) + s(platform,k=5,bs="re")+ s(grassheight,k=5), data = GAMswarm,
            family = negbin(theta = 0.62),method="REML",scale=0)

AIC(swarm4,swarm3)

par(mfrow = c(1, 2))
plot(GAMtest5, all.terms=TRUE)
plot(GAM1)
plot(GAMtest4, residuals = TRUE)
plot(GAMtest8$gam, pages=1)
summary(GAM2)
summary(GAMtest5)
gam.check(GAMtest4)
gam.check(GAMtest5)

swarm3 <- gam(swarm  ~s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=3) + s (grassheight,k=3) + s(platform,k=3,bs="re") + Treatment, data = GAMInsects,
              family = negbin(theta = 0.62,link = "log"),method="REML",scale = 0)

overdispersion <- swarm3$deviance / swarm3$df.residual
overdispersion 

swarm3 <- gam(swarm  ~s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(temperature,k=5) + s (grassheight,k=5) + s(platform,k=5,bs="re") + Treatment, data = GAMInsects,
              family = negbin(theta = 0.62,link = "log"),method="REML",scale = 0)

overdispersion <- swarm4$deviance / swarm4$df.residual
overdispersion 

swarm5 <- gam(swarm  ~s(daysafteraugust2,k=7) + s(minaftersunset2,k=7) + s(temperature,k=7) + s (grassheight,k=7) + s(platform,k=7,bs="re") + Treatment, data = GAMInsects,
              family = negbin(theta = 0.62,link = "log"),method="REML",scale = 0)

overdispersion <- swarm5$deviance / swarm5$df.residual
overdispersion 



swarm3 <- gam(swarm  ~s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(temperature,k=5) + s (grassheight,k=5) + s(platform,k=5,bs="re") + Treatment, data = GAMInsects,
              family = negbin(theta = 0.01,link = "log"),method="REML",scale = 0)

swarm4 <- gam(swarm  ~s(daysafteraugust2,k=5) + s(minaftersunset2,k=5) + s(temperature,k=5) + s (grassheight,k=5) + s(platform,k=5,bs="re") + Treatment, data = GAMInsects,
              family = negbin(theta = 0.5,link = "log"),method="REML",scale = 0)






swarm6 <- gam(mean.swarm  ~s(daysafteraugust2,k=10) + s(minaftersunset2,k=10) + s(temperature,k=10) + s (grassheight,k=10) + s(platform,k=10,bs="re") + Treatment, data = GAMInsects,
              family = negbin(theta = 1.0,link = "log"),method="REML",scale=-1)




swarm5 <- gam(swarm  ~s(daysafteraugust2,k=3) + s(minaftersunset2,k=5) + s(platform,k=3,bs="re")  + s(temperature,k=3) + s(moonphase,k=3) + s(wind,k=3) + meadow.x, data = GAMswarm,
              family = negbin(theta = 1.0,link = "log"),method="REML",scale=-1)

swarm6 <- gam(swarm  ~s(daysafteraugust2,k=3) + s(minaftersunset2,k=5) + s(platform,k=3,bs="re")  + s(temperature,k=3) + s(moonphase,k=3) + s(wind,k=3) + meadow.x, data = GAMswarm,
              family = negbin(theta = 1.5,link = "log"),method="REML",scale=0,df=5)

 
 swarm7 <- gam(swarm  ~s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(platform,k=3,bs="re")  + s(temperature,k=3) + s(grassheight,k=3) + s(moonphase,k=3) + s(wind,k=3) + meadow.x, data = GAMswarm,
              family = negbin(theta = 1.8,link = "log"),method="REML",scale=0,df=5)


 
 
#pre and post
swarm8 <- gam(swarm  ~ relevel(Treatment,"pre") + s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=3) + s(platform,k=3,bs="re") + s(grassheight , k=3) +s(moonphase,k=3) + s(wind,k=3) + s(meadow.x,k=3,bs="re"), data = GAMswarm,
            family = negbin(theta = 1.8),method="REML",scale=0,df=5)

#pre
swarm9 <- gam(swarm  ~ relevel(meadow.x,"Hockgraben") + s(daysafteraugust2,k=3) + s(minaftersunset2,k=3) + s(temperature,k=3) + s(platform,k=3,bs="re") + s(grassheight , k=3) +s(moonphase,k=3) + s(wind,k=3) + s(meadow.x,k=3,bs="re"), data = GAMswarm,
              family = negbin(theta = 1.8),method="REML",scale=0,df=5)



install.packages("AICcmodavg")
library(AICcmodavg)
library(lme4)


models <- list(swarm8,swarm9, swarm10, swarm11,swarm12)
all_ms_glm <- lapply(models, glm.convert)
model.names <- c('8', '9', '10', '11','12')
cand.set = models
modnames = model.names
AIC(models)
AICcmodavg::aictab(cand.set = all_ms_glm,modnames = model.names,second.ord=TRUE,nobs=NULL,sort=TRUE)
gam.check(swarm5)

overdispersion <- swarm8$deviance / swarm8$df.residual
overdispersion 
AIC(swarm8,swarm9,swarm10)

plot(swarm9, select = 1,ylab="swarming propability (residuals)" , xlab="days after august 10th", shade=TRUE,shade.col="lightpink",main="season",ylim=c(-25,20))
plot(swarm8, select = 2, ylab="swarming propability (residuals)",xlab="minutes after sunset", shade=TRUE,shade.col="lightpink",main="night",ylim=c(-10,10))
plot(swarm8, select = 3, ylab="swarming propability (residuals)",xlab="temperature", shade=TRUE,shade.col="lightpink",ylim=c(-10,10))
plot(swarm8, select = 5, ylab="swarming propability (residuals)",xlab="grassheight", shade=TRUE,shade.col="lightpink",ylim=c(-10,5))
plot(swarm8, select = 6, ylab="swarming propability (residuals)",xlab="moonphase", shade=TRUE,shade.col="lightpink",ylim=c(-20,25))
plot(swarm8, select = 7, ylab="swarming propability (residuals)",xlab="wind", shade=TRUE,shade.col="lightpink",ylim=c(-10,10))
plot(swarm8, all.terms=TRUE,ylim=c(-20,30))


plot(swarm6, select = 4, ylab="insect abundance",xlab="grassheight", shade=TRUE,shade.col="lightpink")

plot(GAM3, all.terms=TRUE)
summary(swarm9)

