library(tidyr)
library(dplyr)
library(lubridate)
#install.packages("lubridate")

  
# remove the unnecessary columns 
finalcall <- dfTadarida_3bdf%>%
  select(c(Filename, Date, platform, CallNum, StTime, Fmax, Fmin, Eptser, Nyclei, Nycnoc, Pipkuh, Pipnat, Pippip, Pippyg, noise, uniqueID, time4, highestProb, actualProb))

bdf <- finalcall%>%  
  mutate(batpresence = highestProb,remove=FALSE, batpresence = if_else(is.na(highestProb), "No", "Yes" ))%>% 
  mutate(counts_total = highestProb,remove=FALSE, counts_total =  if_else(batpresence == "No", 0, 1)) %>%
  mutate(countsbyspecies = highestProb,remove=FALSE,countsbyspecies =  if_else(batpresence == "No", 0, 1))

#delete harmonics Nycnoc
bdfharmonics <- bdf  %>% 
  filter(highestProb == "Nycnoc") %>% 
  filter(Fmax > 17 & Fmax < 24) 
 
#clean out unnecessary columns and add subset Nycnoc calls back to dataframe with all other species calls
bdf.clean <- bdf%>% 
  filter(highestProb!="Nycnoc")
  bdf.clean <- rbind(bdf.clean, bdfharmonics)
  #dplyr::select(-c(time, splitrealtime, StTime2, StTime))

#delete harmonics Nyclei
bdfharmonicsNyclei <- bdf.clean  %>% 
  filter(highestProb == "Nyclei") %>% 
  filter(Fmax > 25 & Fmax < 28)

#clean out unnecessary columns and add subset Nycnoc calls back to dataframe with all other species calls
bdf.clean2 <- bdf.clean%>% 
  filter(highestProb!="Nyclei")
bdf.clean2 <- rbind(bdf.clean2, bdfharmonicsNyclei)
#dplyr::select(-c(time, splitrealtime, StTime2, StTime))

#delete harmonics Pippip
bdfharmonicsPippip <- bdf.clean2  %>% 
  filter(highestProb == "Pippip") %>% 
  filter(Fmax > 41 & Fmax < 48)

#clean out unnecessary columns and add subset Nycnoc calls back to dataframe with all other species calls
bdf.clean3 <- bdf.clean2%>% 
  filter(highestProb!="Pippip")
bdf.clean3 <- rbind(bdf.clean3, bdfharmonicsPippip)
#dplyr::select(-c(time, splitrealtime, StTime2, StTime))

#delete harmonics Pippip
bdfharmonicsPippyg <- bdf.clean3  %>% 
  filter(highestProb == "Pippyg") %>% 
  filter(Fmax > 51 & Fmax < 61)

#clean out unnecessary columns and add subset Nycnoc calls back to dataframe with all other species calls
bdf.clean4 <- bdf.clean3 %>% 
  filter(highestProb!="Pippyg")
bdf.clean4 <- rbind(bdf.clean4, bdfharmonicsPippyg)
#dplyr::select(-c(time, splitrealtime, StTime2, StTime))

#delete harmonics Pippip
bdfharmonicsPipnat <- bdf.clean4  %>% 
  filter(highestProb == "Pipnat") %>% 
  filter(Fmax > 37 & Fmax < 42)

#clean out unnecessary columns and add subset Nycnoc calls back to dataframe with all other species calls
bdf.clean5 <- bdf.clean4 %>% 
  filter(highestProb!="Pipnat")
bdf.clean5 <- rbind(bdf.clean5, bdfharmonicsPipnat)
#dplyr::select(-c(time, splitrealtime, StTime2, StTime))

#delete harmonics Pippip
bdfharmonicsPipkuh <- bdf.clean5  %>% 
  filter(highestProb == "Pipkuh") %>% 
  filter(Fmax > 36 & Fmax < 41)

#clean out unnecessary columns and add subset Nycnoc calls back to dataframe with all other species calls
bdf.clean6 <- bdf.clean5 %>% 
  filter(highestProb!="Pipkuh")
bdf.clean6 <- rbind(bdf.clean6, bdfharmonicsPipkuh)
#dplyr::select(-c(time, splitrealtime, StTime2, StTime))

#delete harmonics Pippip
bdfharmonicsEptser <- bdf.clean6  %>% 
  filter(highestProb == "Eptser") %>% 
  filter(Fmax > 25 & Fmax < 28)

#clean out unnecessary columns and add subset Nycnoc calls back to dataframe with all other species calls
bdf.clean7 <- bdf.clean6 %>% 
  filter(highestProb!="Eptser")
bdf.clean7 <- rbind(bdf.clean7, bdfharmonicsEptser)
#dplyr::select(-c(time, splitrealtime, StTime2, StTime))



# this gives the calls per minute and creates a new column for every species, and only one row per minute
bdf.summarise <- bdf.clean7 %>%
  group_by(uniqueID, highestProb) %>%
  summarise(countsbyspecies = n())%>%
  spread(key=highestProb, value=countsbyspecies, fill=0)%>%
  mutate(totalcount = Eptser + Nyclei + Nycnoc + Pipkuh + Pipnat + Pippip + Pippyg) %>%
  mutate_at(vars(Eptser:totalcount), replace_na, '0') 

# mutate the two dataframes and replace NA as 0 counts. 
bdf.final2 <- full_join(x=insectandswarm, y=bdf.summarise, by=c("uniqueID")) %>% 
  unite(col = "minaftersunset" , c("date", "sunset"),sep = "-", remove = FALSE)%>% #erstelle min after sunset and days after august
  relocate(date, sunset, minaftersunset)  %>% 
  rename("event" = "day/event")%>% 
  mutate(daysafteraugust = date) %>% 
mutate(trueinsectdata = found_insects,remove=FALSE,trueinsectdata = if_else(is.na(trueinsectdata),"no","yes"))%>% 
mutate(truebatdata = totalcount,remove=FALSE,truebatdata = if_else(is.na(totalcount),"no","yes"))%>%
  mutate(trueswarmdata = swarm,remove=FALSE,trueswarmdata =if_else(is.na(swarm),"no","yes"))%>%
#mutate(platforms.working = trueinsectdata, remove=FALSE)%>%
mutate(uniqueID2 = paste(date , meadow,sep = "-"),remove=FALSE)

save(bdf.final2, file = "/Users/melinadietzer/Desktop/df.Rda")

#batdata-audiomoths working
groupby <- bdf.final2 %>%
  filter(truebatdata== "yes")%>%
  group_by(meadow,date) %>%
  summarise(audiomoths.working = n_distinct(platform)) %>%
  mutate(uniqueID2 = paste(date , meadow,sep = "-"))


#insectdata-cameras working
groupby2 <- bdf.final2 %>%
  filter(trueinsectdata== "yes")%>%
  group_by(meadow,date) %>%
  summarise(cameras.working = n_distinct(platform)) %>%
  mutate(uniqueID2 = paste(date , meadow,sep = "-"))

#swarmdata-cameras working
groupby3 <- bdf.final2 %>%
  filter(trueswarmdata== "yes")%>%
  group_by(meadow,date) %>%
  summarise(cameras.working.swarm = n_distinct(platform)) %>%
  mutate(uniqueID2 = paste(date , meadow,sep = "-"))

bdf.final.x <- full_join(x=bdf.final2,y=groupby,by=c("uniqueID2"))
bdf.final.y <- full_join(x=bdf.final.x,y=groupby2,by=c("uniqueID2")) 
bdf.final.z <- full_join(x=bdf.final.y,y=groupby3,by=c("uniqueID2")) 
 
bdf.final.100 <- bdf.final.z %>% 
group_by(date.x, meadow.x, mtime2) %>% 
  summarise(mean.insects = (sum(found_insects)/cameras.working),sunset,minaftersunset,ID,uniqueID,mtime,mtime2,platform,filename,found_insects,location, grassheight, temperature,wind,moonphase,Treatment,event,processed,swarm,Eptser,Nyclei,Nycnoc,Pipkuh,Pipnat,Pippip,Pippyg,totalcount,daysafteraugust,trueinsectdata,truebatdata,,trueswarmdata,uniqueID2,audiomoths.working, meadow.x, cameras.working,cameras.working.swarm)

#edwards ergänzung
bdf.final.a <- bdf.final.100 %>% 
  distinct(mean.insects,.keep_all = TRUE)
  
bdf.final.y$audiomoths.working <- as.numeric(bdf.final.y$audiomoths.working)
bdf.final.y$totalcount <- as.numeric(bdf.final.y$totalcount)
bdf.final.z$swarm <- as.numeric(bdf.final.z$swarm)


bdf.final.b1 <- bdf.final.z %>% 
  group_by(date.x, meadow.x, mtime2) %>% 
  summarise(mean.bats = (sum(totalcount)/audiomoths.working),uniqueID)

#edwards ergänzung
bdf.final.b <- bdf.final.b1 %>% 
  distinct(mean.bats,.keep_all = TRUE)


bdf.final.c1 <- bdf.final.z %>% 
  group_by(date.x, meadow.x, mtime2) %>% 
  summarise(mean.swarm = (sum(swarm)/cameras.working.swarm),uniqueID)

#edwards ergänzung
bdf.final.c <- bdf.final.c1 %>% 
  distinct(mean.swarm,.keep_all = TRUE)

#join
bdf.final.xy <- full_join(x=bdf.final.a, y=bdf.final.b,by="uniqueID") 
bdf.final <- full_join(x=bdf.final.xy, y=bdf.final.c,by="uniqueID") 
 


#erstelle min after sunset
bdf.final$minaftersunset <- format(as.POSIXct(bdf.final$minaftersunset, format= "%Y%m%d-%H:%M:%S"), format = "%Y-%m-%d %H:%M:%S") 
bdf.final$mtime <- format(as.POSIXct(bdf.final$mtime, format= "%Y%m%d-%H:%M:%S"), format = "%Y-%m-%d %H:%M:%S") 
bdf.final$minaftersunset2 <- as.numeric(difftime(bdf.final$mtime,bdf.final$minaftersunset,unit="min"))
bdf.final$minaftersunset2 <-round(bdf.final$minaftersunset2,digits = 1)

#erstelle days after August
bdf.final$daysafteraugust <- format(as.POSIXct(bdf.final$daysafteraugust, format= "%Y%m%d") ,format = "%Y-%m-%d") 
bdf.final$date.x <- format(as.POSIXct(bdf.final$date.x, format= "%Y%m%d"), format = "%Y-%m-%d") 
bdf.final$daysafteraugust2 <- as.numeric(difftime(bdf.final$daysafteraugust,as.Date("2020-08-10"),unit="days"))
bdf.final$daysafteraugust2<-round(bdf.final$daysafteraugust2,digits = 0)









batstatistic <- bdf.final %>% 
  filter (meadow %in% c ("Hockgraben","Uni","StKatharina","Tannenhof","Institut","Voliere","Scheune","Hockgraben2","StKatharina2","Mill","Bildwiese","Guettingen","Hegne","Wollmatingen","Unikurve")) %>%
  summarise(truebatdata)%>% 
  mutate(batcount = truebatdata, batcount=if_else(truebatdata=="yes",1,0)) %>% 
  group_by(batcount,truebatdata)%>%
  summarise(batcount =n())

batstatistic <- bdf.final %>% 
  filter (meadow %in% c ("Hockgraben","Uni","StKatharina","Tannenhof","Institut","Voliere","Scheune","Hockgraben2","StKatharina2","Mill","Bildwiese","Guettingen","Hegne","Wollmatingen","Unikurve")) %>%
  summarise(truebatdata,totalcount)%>% 
  mutate(batcount = truebatdata, batcount=if_else(truebatdata=="yes",1,0)) %>% 
  group_by(totalcount,batcount)%>% 
  summarise(batcount =n())

#mutate(if_else(trueinsectdata =="yes",platforms.working = n_distinct(platform),0))

#erstelle if there is data or not
#erstelle Treatment scent_experiment; scent_measurement 
# mutate(Treatment2 = Treatment) %>% 
#mutate( Treatment2 = if_else(minaftersunset2 == 0:60, "scent_experiment", "scent_measurement" )) 




bdf.finalohnena <- bdf.final %>% 
  mutate(mean.bats2=mean.bats) 
  bdf.finalohnena$mean.bats2[is.na( bdf.finalohnena$mean.bats2)] <- 0


bdf.final2 <- bdf.final %>% 
  group_by(Treatment=="pre",mean.insects,date.x,mtime2.x) %>% 
  summarise(Treatment=="pre",mean.insects=n()) %>% 
  summarise(mean.insects=n(),Treatment=="pre")

write_csv2(bdf.final,file = "/Users/melinadietzer/bdf.final.csv")
# you can now check maybe the minutes in which you have a lot of calls, if that is true calls or something weird.  Also feel free to kick out more columns form the bdf.final which you don't need using the same code as before (select(c(...))



idx <- which(duplicated(paste0(GAMInsects$meadow.x.x, GAMInsects$mtime2.x)))
mean.bdf<- GAMInsects[-idx,]

bdf_new_mean_1 <- bdf_new_mean%>%
  distinct(VO2_mean, .keep_all = TRUE) #remove the double columns for the same minute



bdf <- finalcall%>%  
  mutate(batpresence = highestProb,remove=FALSE, batpresence = if_else(is.na(highestProb), "No", "Yes" ))%>% 
  mutate(counts_total = highestProb,remove=FALSE, counts_total =  if_else(batpresence == "No", 0, 1)) %>%
  mutate(countsbyspecies = highestProb,remove=FALSE,countsbyspecies =  if_else(batpresence == "No", 0, 1))





