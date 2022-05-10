library(dplyr)
library(tidyr)
library(readr)
library(hms)
library(ggplot2)
library(cowplot)
library(chron)
library(tuneR)
library(tidyselect)
#install.packages("tidyverse")
#library(lubridate)
#install.packages("lubridate")


#load data

setwd("~/ownCloud")

#get the "real" time
dfTadarida1 <- readr::read_csv2("bdfTadarida.csv") %>%
  separate(Filename, into=c("Date", "Filename2"), sep="-", remove=FALSE) %>%
  separate(Filename2, into=c("platform","time","Split"), sep="_") %>% 
  separate(Split, into=c("SplitNo","ext")) %>% 
  dplyr::select(-ext) %>% 
  mutate(SplitNo = as.numeric(SplitNo)) %>% 
  mutate(splitrealtime = ((SplitNo-1)*5)) %>% 
  mutate(time = as.POSIXct(format(strptime(time, format="%H%M%S"), format = "%H:%M:%S"), format="%H:%M:%S")) %>% 
  mutate(time2 = time + splitrealtime) %>% 
  mutate(StTime2 = StTime/1000) %>% 
  mutate(time3 = time2 + StTime2)  %>% 
  mutate(time4 = time3)%>% 
    relocate(time4, time3,time2, time, splitrealtime, StTime2, StTime) 

dfTadarida1$time4 <-round(dfTadarida1$time4,"mins")

dfTadarida1$time <- format(strptime(dfTadarida1$time, format= "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")
dfTadarida1$time3 <- format(strptime(dfTadarida1$time3, format= "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")
dfTadarida1$time4 <- format(strptime(dfTadarida1$time4, format= "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S")

#add UniqueID
dfTadaridabdf <- dfTadarida1 %>% 
unite(col = "uniqueID" , c("Date", "platform", "time4"), sep = "-",remove = FALSE) 



#select the species with the highest probability 
dfTadarida_2bdf <- dfTadaridabdf %>% dplyr::select(Eptser:Pippyg) %>% 
  mutate(actualProb = apply(., 1, max)) %>% 
  mutate(highestProb = names(.)[apply(., 1, which.max)]) %>% 
  dplyr::select(highestProb, actualProb)

dfTadaridabdf <- cbind(dfTadaridabdf, dfTadarida_2bdf)

#filter
dfTadarida_3bdf <- dfTadaridabdf  %>% 
  filter(actualProb > 0.35)   %>% 
  filter(noise < 0.35)  
  #filter(highestProb == "Nycnoc") %>% 
  #filter(Fmax > 17 & Fmax < 22)

#clean out unnecessary columns and add subset Nycnoc calls back to dataframe with all other species calls
#dfTadarida.clean <- dfTadaridabdf %>% filter(highestProb!="Nycnoc")
#dfTadarida.clean <- rbind(dfTadarida.clean, dfTadarida_3bdf) %>% 
  #dplyr::select(-c(time, splitrealtime, StTime2, StTime))

#removes date portion from timestamp
#dfTadarida1$time <- format(strptime(dfTadarida1$time, format="%H%M%S"), format = "%H:%M:%S")



