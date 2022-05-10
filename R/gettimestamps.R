library(dplyr)
library(tidyr)
library(readr)
library(hms)
library(ggplot2)
library(cowplot)
library(chron)
library(tuneR)
library(tidyselect)
library(tidyverse)
library(lubridate)
library(remotes)
install.packages("lubridate")




#load data
setwd("/Users/melinadietzer/")

counts3 <- read_csv2("bdf_Insects_final.csv") %>%
separate(path, into=c("path", "path2"), sep="84gb/",) %>%
  separate(path2, into=c("date","path3"), sep="-",) %>% 
  separate(path3, into=c("platform", "filename"), sep="/") %>%
  unite(col = "ID" , c("date", "platform", "filename"), sep = "-")



#add timestamps for photos
paths <- list.files(path = "/Volumes/LACIE SHARE/Master/MasterArbeit/Auswertung/Photos_Melina_fieldwork/FotosfürIrek", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
meta <- as.data.frame(file.info(paths)) %>% 
  tibble::rownames_to_column("path") %>% 
  dplyr::select(path,mtime) %>% 
  separate(path, into = c("a","b","c","d","e","f","g", "h", "i", "j"), sep="/") %>% #extra="merge"
  dplyr::select(-c(a,b,c,d,e,f,g,h)) %>% 
  drop_na() %>% #NAs created in the g (filename) column for rows that refer to the folder rather than the files within -- remove with drop_na
  unite(col = "ID", i:j, sep = "-")

#merged <- merge(counts3, meta, by.x ="date", by.y = "mtime") %>%
 # mutate(time = format(as.POSIXct(mtime), format = "%H:%M:%S")) %>%
  #mutate(time = hms(time)) #make sure this is accurate...

merged2 <- merge(meta, counts3, by="ID")
str(merged2)

merged3 <- merged2 %>%
  tidyr::separate(ID, into = c("date", "platform", "filename"), sep = "-", remove = FALSE)%>%
  mutate(mtime2 =mtime) 
  #mutate(wetterID = mtime,remove=FALSE)

merged3$mtime2 <-round(merged3$mtime2,"mins")
 
merged3$mtime2 <- format(strptime(merged3$mtime2, format= "%Y-%m-%d %H:%M:%S"), format = "%H:%M:%S") 
#merged3$wetterID <-round(merged3$wetterID,"mins")
#merged3$wetterID <- as.character(merged3$wetterID)
write_csv(merged3, file = "/Users/melinadietzer/AuswertungFotosR/merged3.csv")  


#add uniqueID
mergedbdf <- merged3 %>%
  relocate(ID, date, mtime, mtime2)  %>% 
  unite(col = "uniqueID" , c("date", "platform", "mtime2"), sep = "-",remove = FALSE)
#write_csv(mergedbdf, file = "/Users/melinadietzer/AuswertungFotosR/mergedbdf.csv")  
#mergedbdf$wetterID <- as.character(mergedbdf$wetterID)

#addinsectswarm
swarmbdf <- mergedbdf %>% 
mutate(swarm = found_insects,remove=FALSE, swarm = if_else(found_insects >= 10, "1", "0" )) %>% 
rename("meadow" = "platform")%>% 
mutate(meadow = recode(meadow, "Im" = "Institute_1")) %>% 
mutate(meadow = recode(meadow, "Ir" = "Institute_2")) %>% 
mutate(meadow = recode(meadow, "HGm" = "Hockgraben_1")) %>% 
mutate(meadow = recode(meadow, "HGr" = "Hockgraben_2")) %>% 
mutate(meadow = recode(meadow, "Um" = "Uni_1")) %>% 
mutate(meadow = recode(meadow, "Ur" = "Uni_2")) %>%   
mutate(meadow = recode(meadow, "Sm" = "Scheune_1")) %>%   
mutate(meadow = recode(meadow, "Sr" = "Scheune_2")) %>%  
mutate(meadow = recode(meadow, "SKm" = "StKatharina_1")) %>%   
mutate(meadow = recode(meadow, "SKr" = "StKatharina_2")) %>%  
mutate(meadow = recode(meadow, "Tm" = "Tannenhof_1")) %>%   
mutate(meadow = recode(meadow, "Tr" = "Tannenhof_2")) %>%  
mutate(meadow = recode(meadow, "Vm" = "Voliere_1")) %>%   
mutate(meadow = recode(meadow, "Vr" = "Voliere_2")) %>%  
mutate(meadow = recode(meadow, "H1" = "Hegne_1")) %>%   
mutate(meadow = recode(meadow, "H2" = "Hegne_2")) %>%  
mutate(meadow = recode(meadow, "H3" = "Hegne_3")) %>%   
mutate(meadow = recode(meadow, "H4" = "Hegne_4")) %>%  
mutate(meadow = recode(meadow, "H5" = "Hegne_5")) %>%   
mutate(meadow = recode(meadow, "W1" = "Wollmatingen_1")) %>%  
mutate(meadow = recode(meadow, "W2" = "Wollmatingen_2")) %>%  
mutate(meadow = recode(meadow, "W3" = "Wollmatingen_3")) %>%  
mutate(meadow = recode(meadow, "UK1" = "Unikurve_1")) %>%  
mutate(meadow = recode(meadow, "UK2" = "Unikurve_2")) %>% 
mutate(meadow = recode(meadow, "UK3" = "Unikurve_3")) %>% 
mutate(meadow = recode(meadow, "UK4" = "Unikurve_4")) %>% 
mutate(meadow = recode(meadow, "UK5" = "Unikurve_5")) %>% 
mutate(meadow = recode(meadow, "GS1" = "Guettingen_1")) %>% 
mutate(meadow = recode(meadow, "GS2" = "Guettingen_2")) %>% 
mutate(meadow = recode(meadow, "GS3" = "Guettingen_3")) %>% 
mutate(meadow = recode(meadow, "GS4" = "Guettingen_4")) %>% 
mutate(meadow = recode(meadow, "GS5" = "Guettingen_5")) %>% 
mutate(meadow = recode(meadow, "GW1" = "Guettingen_1")) %>% 
mutate(meadow = recode(meadow, "GW2" = "Guettingen_2")) %>% 
mutate(meadow = recode(meadow, "GW3" = "Guettingen_3")) %>% 
mutate(meadow = recode(meadow, "GW4" = "Guettingen_4")) %>% 
mutate(meadow = recode(meadow, "GW5" = "Guettingen_5")) %>% 
mutate(meadow = recode(meadow, "HGS1" = "Hockgraben2_1")) %>% 
mutate(meadow = recode(meadow, "HGS2" = "Hockgraben2_2")) %>% 
mutate(meadow = recode(meadow, "HGS3" = "Hockgraben2_3")) %>% 
mutate(meadow = recode(meadow, "HGS4" = "Hockgraben2_4")) %>% 
mutate(meadow = recode(meadow, "HGS5" = "Hockgraben2_5")) %>% 
mutate(meadow = recode(meadow, "HGW1" = "Hockgraben2_1")) %>% 
mutate(meadow = recode(meadow, "HGW2" = "Hockgraben2_2")) %>% 
mutate(meadow = recode(meadow, "HGW3" = "Hockgraben2_3")) %>% 
mutate(meadow = recode(meadow, "HGW4" = "Hockgraben2_4")) %>% 
mutate(meadow = recode(meadow, "HGW5" = "Hockgraben2_5")) %>% 
mutate(meadow = recode(meadow, "MS1" = "Mill_1")) %>% 
mutate(meadow = recode(meadow, "MS2" = "Mill_2")) %>% 
mutate(meadow = recode(meadow, "MS3" = "Mill_3")) %>% 
mutate(meadow = recode(meadow, "MS4" = "Mill_4")) %>% 
mutate(meadow = recode(meadow, "MS5" = "Mill_5")) %>% 
mutate(meadow = recode(meadow, "MW1" = "Mill_1")) %>% 
mutate(meadow = recode(meadow, "MW2" = "Mill_2")) %>% 
mutate(meadow = recode(meadow, "MW3" = "Mill_3")) %>% 
mutate(meadow = recode(meadow, "MW4" = "Mill_4")) %>% 
mutate(meadow = recode(meadow, "MW5" = "Mill_5")) %>% 
mutate(meadow = recode(meadow, "MSS1" = "Mindelsee_1")) %>% 
mutate(meadow = recode(meadow, "MSS2" = "Mindelsee_2")) %>% 
mutate(meadow = recode(meadow, "MSS3" = "Mindelsee_3")) %>% 
mutate(meadow = recode(meadow, "MSS4" = "Mindelsee_4")) %>% 
mutate(meadow = recode(meadow, "MSS5" = "Mindelsee_5")) %>% 
mutate(meadow = recode(meadow, "MSW1" = "Mindelsee_1")) %>% 
mutate(meadow = recode(meadow, "MSW2" = "Mindelsee_2")) %>% 
mutate(meadow = recode(meadow, "MSW3" = "Mindelsee_3")) %>% 
mutate(meadow = recode(meadow, "MSW4" = "Mindelsee_4")) %>% 
mutate(meadow = recode(meadow, "MSW5" = "Mindelsee_5")) %>% 
mutate(meadow = recode(meadow, "BWS1" = "Bildwiese_1")) %>% 
mutate(meadow = recode(meadow, "BWS2" = "Bildwiese_2")) %>% 
mutate(meadow = recode(meadow, "BWS3" = "Bildwiese_3")) %>% 
mutate(meadow = recode(meadow, "BWS4" = "Bildwiese_4")) %>% 
mutate(meadow = recode(meadow, "BWS5" = "Bildwiese_5")) %>% 
mutate(meadow = recode(meadow, "BWW1" = "Bildwiese_1")) %>% 
mutate(meadow = recode(meadow, "BWW2" = "Bildwiese_2")) %>% 
mutate(meadow = recode(meadow, "BWW3" = "Bildwiese_3")) %>% 
mutate(meadow = recode(meadow, "BWW4" = "Bildwiese_4")) %>% 
mutate(meadow = recode(meadow, "BWW5" = "Bildwiese_5")) %>% 
mutate(meadow = recode(meadow, "MOES1" = "Moeggingen_1")) %>% 
mutate(meadow = recode(meadow, "MOES2" = "Moeggingen_2")) %>% 
mutate(meadow = recode(meadow, "MOES3" = "Moeggingen_3")) %>% 
mutate(meadow = recode(meadow, "MOES4" = "Moeggingen_4")) %>% 
mutate(meadow = recode(meadow, "MOES5" = "Moeggingen_5")) %>% 
mutate(meadow = recode(meadow, "MOEW1" = "Moeggingen_1")) %>% 
mutate(meadow = recode(meadow, "MOEW2" = "Moeggingen_2")) %>% 
mutate(meadow = recode(meadow, "MOEW3" = "Moeggingen_3")) %>% 
mutate(meadow = recode(meadow, "MOEW4" = "Moeggingen_4")) %>% 
mutate(meadow = recode(meadow, "MOEW5" = "Moeggingen_5")) %>% 
  mutate(meadow = recode(meadow, "SKS1" = "StKatharina2_1")) %>% 
  mutate(meadow = recode(meadow, "SKS2" = "StKatharina2_2")) %>% 
  mutate(meadow = recode(meadow, "SKS3" = "StKatharina2_3")) %>% 
  mutate(meadow = recode(meadow, "SKS4" = "StKatharina2_4")) %>% 
  mutate(meadow = recode(meadow, "SKS5" = "StKatharina2_5")) %>% 
  mutate(meadow = recode(meadow, "SKW1" = "StKatharina2_1")) %>% 
  mutate(meadow = recode(meadow, "SKW2" = "StKatharina2_2")) %>% 
  mutate(meadow = recode(meadow, "SKW3" = "StKatharina2_3")) %>% 
  mutate(meadow = recode(meadow, "SKW4" = "StKatharina2_4")) %>% 
  mutate(meadow = recode(meadow, "SKW5" = "StKatharina2_5"))  %>%
separate(meadow, into=c("meadow", "platform"), sep="_",)



insectandswarm <- swarmbdf%>%   
  select(-c(path, visual_inspection, more_conservative, comments))  
 

insectstatistic2 <- bdf.final %>% 
  mutate(insectpresent = found_insects, remove =FALSE, insectpresent = if_else(found_insects == 0, 0, 1)) %>%
group_by(insectpresent,found_insects) %>% 
dplyr::summarise(insectpresent=n())

 
  mutate(totalfotoscount =insectpresent,remove=FALSE,sum(totalfotoscount=n()),sum(insectpresent=n())) %>% 
  summarise(n=n())


#write_csv(merged3, file = "/Users/melinadietzer/AuswertungFotosR/merged3.csv")  
#mutate--> zeit in kategirien einteilen (21-21.15 = A )

merged4 <- merged3%>%
  group_by(date, platform, location,grassheight)%>%
  summarise(n=n())
  #mean(temperature~date)
  #summarize(n=mean())

merged4
#merged4$date <- as.POSIXct(merged4$date)
merged4$n <- as.numeric(merged4$n)
merged4$platform <- as.factor(merged4$platform)
merged4$location <- as.factor(merged4$location)
merged4$grassheight <- as.numeric(merged4$grassheight)
#merged4$temperature <- as.factor(merged4$temperature)
data.frame(merged4)
write_csv2(merged4,file="/Users/melinadietzer/AuswertungFotosR/merged4.csv")


  
#Plot
p <- GAMInsects%>%
  GAMInsects$mean.insects <- as.numeric( GAMInsects$mean.insects) %>%
  #filter(exp %in% c("Respiro", "Cali"))%>%# in 1 spalte nach 2 filtern
 # filter(platform=="SKm") %>%
  #filter(date=="20200811") %>%
  ggplot(aes(GAMInsects)) +
  geom_bar(aes(x=meadow.x.x, y = mean.insects), color="red", size =1, alpha=1)
  #geom_point(aes(x= date, y =temperature), color="blue", size =0.5, alpha=5)
  #geom_point(aes(x=dt, y = temperature.iba*10), color="green", size =1, alpha=0.7)+
  # geom_point(aes(x=dt, y = temp.control*10), color="purple", size =1, alpha=0.7)+
  # geom_point(aes(x=dt, y = temperature*10), color="orange", size =1, alpha=0.7)+
  # geom_point(aes(x=dt, y = bpm), color="blue", size =1, alpha=0.7)+
  #geom_point(aes(x=dt, y = VO2*200), color="red", size =1, alpha=0.7)+
  #geom_point(aes(x=dt, y = VCO2*200), color="black", size =1, alpha=0.7)+
  #scale_y_continuous(limits=c(0,650),"bpm" , sec.axis=sec_axis(~ ./10, name = "Temperature in degree C")) +
  #theme(axis.title.y = element_text(color = "blue"),
        #axis.title.y.right = element_text(color = "black"), 
        #panel.background = element_rect(fill = 'white' , colour = 'black'))
  #geom_hline(yintercept=0, linetype="dashed", color = "black")
#p+ggtitle(Respiro$batID)
p

#boxplot
library(ggplot2)
library(cowplot)
p <- merged4%>%
  filter(platform %in% c("GS1","GS2","GS3","GS4","GS5"))%>%
  ggplot(aes(merged4)) +
  geom_boxplot(aes(x=date, y = n),color=location)
#cor.test(midle,edge)
p

#barplot
setwd("/Volumes/Elements/Auswertung/")
read_delim(MutatetAll.RData)

library(ggplot2)
library(cowplot)
bdf.final$totalcount <- as.numeric(bdf.final$totalcount)
p <- bdf.final%>%
  filter(meadow %in% c("Bildwiese", "Hockgraben","StKatharina","StKatharina2","Hockgraben2","Uni","Mill","Tannenhof","Mindelsee","Scheune","Voliere","Guettingen","Moeggingen"))%>%
  #filter(location =="middle")%>%
  #filter(date =="20200927")%>%
  filter(platform == "1")%>%
  ggplot(aes(p)) +
  geom_line(aes(x=date, y = totalcount, color=meadow), size =1, alpha=1)+
scale_y_continuous(limits=c(0,1000))+
theme(axis.title.y = element_text(color = "blue"),
      axis.title.y.right = element_text(color = "blue"), 
      panel.background = element_rect(fill = 'white' , colour = 'black'))
p<-p+labs(x="date")

p<-p+ggtitle("St.Katharina-Insectactivity")
p
summary(p)


  geom_point(aes(x= date, y = as.numeric(grassheight)), color="blue", size =5, alpha=5)+
  #geom_point(aes(x= date, y = as.numeric(temperature)), color="blue", size =0.5, alpha=5)+
  labs(x="date", y="insects",title="Wollmatingen midle vs edge")+
  #geom_point(aes(x= date, y =temperature), color="blue", size =1, alpha=5)+
  scale_y_continuous(limits=c(0,250))
 # mod_merged4 <- lm(merged4$n ~ merged4$date)
#summary(mod_merged4)
p

#random
  #theme(legend.position = "none")
 # geom_text(aes(label = lbl), size = 4, position = position_stack(vjust = 0.5)) +
  #scale_fill_manual(“”, labels=c(“normothermic_warm” = “Thermoregulating warm”, #“normothermic_cold” = “Transition”, “thermoconform_cold” = “Thermoconforming cold”, “thermoconform_warm” = “Thermoconforming warm”), values = c(“brown2",“pink2”,“deepskyblue2",“royalblue3”))+
  #scale_x_discrete(labels=c(“April”= “Post-hibernation\nn=432”, “May”= “Pregnancy\nn=1521”,  “October”= “Pre-hibernation\nn=1183”))+
 # scale_x_discrete(labels=c(“April”= “Post-hibernation\nn=3”, “May”= “Pregnancy\nn=13”,  “October”= “Pre-hibernation\nn=9”))+
 # scale_y_continuous(labels = scales::percent_format())+
 # labs(y = “Percent”,x = “Season”, title = “”)+
 # theme_bw()+ theme(panel.grid = element_blank())+
 # theme(axis.text.x = element_text(size=14),
      #  axis.text.y = element_text(size=14),
        #axis.title.y = element_text(size=14),
       # axis.title.x = element_text(size=14),
      #  plot.title = element_text(size = 20, face = “bold”),
       # legend.text = element_text(size = 12))


p <- merged4 %>%
  #filter(platform=="Tm")%>%
  #filter(date=="20200811") %>%
  ggplot(aes(merged3)) +
  geom_point(aes(x= date, y = n))+
  theme(axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "blue"), 
       panel.background = element_rect(fill = 'white' , colour = 'black'))
p<-p+labs(x="date")
p<-p+labs(y="total number Insects")
p<-p+ggtitle("Tannenhof-Mitte")

p





#Streudiagram
p <- ggplot(merged3, aes(x=mtime, y=found_insects)) +
  geom_point()
p


#ab 21 Uhr = merged 5
merged5 <- merged3 %>%
  mutate(mtime = format(as.POSIXct(mtime), format = "%H:%M:%S")) %>%
  #mutate(mtime = hms(mtime))%>%
  filter(platform=="Um")%>%
  filter(date=="20200811")%>%
  filter(mtime >= "21:00:00" ,mtime <= "23:59:55")

#Streudiagram mit filter
#df<-df%>%
 # mutate(time = format(as.POSIXct(mtime), format = "%H:%M:%S")) %>%
  #filter(mtime >= "21:00:00" ,mtime <= "23:59:55")%>%
  #df$time <- as.POSIXct(df$time, format = "%H:%M",tz="GMT")

#lineplot-Tm_11.08 + Tr
library(ggplot2)
library(cowplot)


p <- bdf.final %>%
  filter(meadow =="Hockgraben2")%>%
  ggplot(aes(bdf.final)) +
  geom_line(aes(x= date, y = totalcount))
p
 #lineplotVergleich Mitte/Rand

lims <- as.POSIXct(strptime(c("2020-08-11 20:35:00","2020-08-11 23:59:55"), format = "%Y-%m-%d %H:%M:%S")) 
merged3$hours <-format(as.POSIXct(merged3$mtime, format = "%Y-%m-%d %H:%M:%S", tz="CET", "%H:%M:%S"))
p <- merged3 %>%
  #mutate(mtime = format(as.POSIXct(mtime), format = "%H:%M:%S")) %>%
  filter(platform %in% c("Um","Ur"))%>%
  filter(date=="20200811") %>%
  filter(hours >= "21:00:44" ,hours <= "22:00:46")%>%
  ggplot(aes(merged3)) +
  geom_line(aes(x= mtime, y = found_insects, color=location), size =1, alpha=1)+
  geom_point(aes(x= mtime, y = found_insects) ,size =0.5, alpha=0)+
  geom_line(aes(x= mtime, y =temperature), color="blue", size =1, alpha=5)+
  #geom_line(aes(x= mtime, y =wind), color="yellow", size =1, alpha=5)+
  scale_y_continuous(limits=c(0,100))+
  #scale_x_datetime(breaks=NULL, labels= format("%H:%M"), limits=lims)+
  theme(axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "blue"), 
        panel.background = element_rect(fill = 'white' , colour = 'black'))
p<-p+labs(x="time")

p<-p+ggtitle("Uni-2020-08-20")
p

#lineplot SKm/Skr
lims <- as.POSIXct(strptime(c("2020-08-11 20:35:00","2020-08-11 23:59:55"), format = "%Y-%m-%d %H:%M:%S")) 
merged3$hours <-format(as.POSIXct(merged3$mtime, format = "%Y-%m-%d %H:%M:%S", tz="CET", "%H:%M:%S"))
p <- GAMtest3 %>%
  #mutate(mtime = format(as.POSIXct(mtime), format = "%H:%M:%S")) %>%
  filter(platform=="St.Katharina")%>%
  filter(date=="20200811") %>%
  #filter(hours > "20:59:00" ,hours <= "23:59:55")%>%
  ggplot(aes(merged3)) +
  geom_line(aes(x= mtime2.x, y = mean.insects), color="red", size =1, alpha=5)+
  scale_y_continuous(limits=c(0,100))+
  #scale_x_datetime(breaks=NULL, labels= format("%H:%M"), limits=lims)+
  theme(axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "blue"), 
        panel.background = element_rect(fill = 'white' , colour = 'black'))
p<-p+labs(x="time")

p<-p+ggtitle("St.Katharina-Rand-2020-08-11")
p


#lineplot Um/Ur
lims <- as.POSIXct(strptime(c("2020-08-11 20:35:00","2020-08-11 23:59:55"), format = "%Y-%m-%d %H:%M:%S")) 
merged3$hours <-format(as.POSIXct(merged3$mtime, format = "%Y-%m-%d %H:%M:%S", tz="CET", "%H:%M:%S"))
 p <- bdf.final %>%
  filter(meadow=="Tannenhof")%>%
  filter(date=="20200811") %>%
  #filter(hours >= "20:35:00" ,hours <= "23:59:55")%>%
  ggplot(aes(bdf.final)) +
  geom_line(aes(x= minaftersunset2, y = swarm), color="red", size =1, alpha=5)+
  #scale_y_continuous(limits=c(0,1000))+
  #scale_x_datetime(breaks=NULL, labels= format("%H:%M"), limits=lims)+
  theme(axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "blue"), 
        panel.background = element_rect(fill = 'white' , colour = 'black'))
#p<-p+labs(x="time")

#p<-p+ggtitle("Uni-2020-08-11")
p

#test2


p <- newbat %>%
 
  ggplot(aes(merged3)) +
  geom_line(aes(x= mtime, y = found_insects), color="red", size =1, alpha=5)+
  scale_y_continuous(limits=c(0,10))+
  #scale_x_datetime(breaks=NULL, labels= format("%H:%M"), limits=lims)+
  theme(axis.title.y = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "black"), 
        panel.background = element_rect(fill = 'white' , colour = 'black'))
p






#test
bdf.final$totalcount <- as.numeric(bdf.final$totalcount)
p <- bdf.final %>% 
  filter(totalcount <300) %>%
  filter(meadow =="Hockgraben")%>%
  filter(date =="20200813")%>%
  filter(platform %in% c("1","2"))%>%
  ggplot(aes(p)) +
  geom_line(aes(x=minaftersunset2, y =totalcount ,color=platform), size =1, alpha=0.7)
 
p

  #barplot 

library(ggplot2)
library(cowplot)
library(plyr)
bdf.final$totalcount <- as.numeric(bdf.final$totalcount)
bdf.final$mean.bats <- as.numeric(bdf.final$mean.bats)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

#Pre vs post mowing insects
#durchschnitt aller nächte pre and post 


bdfinsectplotpre <-bdf.final %>% 
  filter (meadow.x.x %in% c("Hockgraben","StKatharina","Uni","Voliere","Institute","Scheune","Tannenhof","Hegne","Wollmatingen")) %>% 
  filter(Treatment =="pre") %>% 
  dplyr::group_by(Treatment) %>% 
  dplyr::summarize(suminsects=sum(mean.insects)/35,sd(mean.insects),n(),meaninsects = (mean(mean.insects))) 
 
bdfinsectplotmowing <-bdf.final %>% 
  filter (meadow.x.x %in% c("Hockgraben","StKatharina","Uni","Voliere","Institute","Scheune","Tannenhof","Hegne","Wollmatingen")) %>% 
  filter(Treatment =="mowing") %>% 
  dplyr::group_by(Treatment) %>% 
  dplyr::summarize(suminsects=sum(mean.insects)/6,sd(mean.insects),n(),meaninsects = (mean(mean.insects))) 

new <-bind_rows(bdfinsectplotpre,bdfinsectplotmowing,bdfinsectplotpost)
newsd <-new %>% 
  mutate(lowersd=suminsects) %>% 
  mutate(highersd=suminsects)
newsd$lowersd <- c(270.270,242.126,221.924)
newsd$highersd <- c(275.729,246.173,225.830)



bdfinsectplotpost <-bdf.final %>% 
  filter (meadow.x.x %in% c("Hockgraben","StKatharina","Uni","Voliere","Institute","Scheune","Tannenhof","Hegne","Wollmatingen")) %>% 
  filter(Treatment =="post") %>% 
  dplyr::group_by(Treatment) %>% 
  dplyr::summarize(suminsects=sum(mean.insects)/15,sd(mean.insects),n(),meaninsects = (mean(mean.insects)))

  

insectmowing <- newsd 
insectmowing$Treatment <- factor( insectmowing$Treatment, levels=c("pre","mowing","post")) 

ggplot(data=insectmowing,aes(x=Treatment, y=suminsects,fill = Treatment)) +
  geom_point() + 
  geom_errorbar(aes(ymin=lowersd,ymax=highersd), width=1)





  
p <- scentvswaterinsects 
  #filter(meadow.x.x =="Uni")
  #filter(totalcount < 400) %>%
  #filter(location =="middle")%>%
  #filter(date =="20200927")%>%
  #filter(platform %in% c("1","2","3","4","5"))%>%
  bdf.final$totalcount <- as.numeric(bdf.final$totalcount)
bdf.final$mean.bats <- as.numeric(bdf.final$mean.bats)
bdf.final$mean.insects <- as.numeric(bdf.final$mean.insects)

data_summary(bdf.final,varname="mean.insects", 
                    groupnames=c("Treatment", "date.x.x"))
# Convert dose to a factor variable
#p$dose=as.factor(df3$dose)
head(p)
bdf.final$totalcount <- as.numeric(bdf.final$totalcount)
bdf.final$mean.insects <- as.numeric(bdf.final$mean.insects)
   ggplot(data=p,aes(x=Treatment, y=mean.count,fill = Treatment)) +
  #geom_errorbar(aes(ymin=totalcount, ymax=totalcount+sd), width=.2,na.rm=TRUE,position = position_dodge(0.9))+
  geom_bar(stat="identity",position = position_dodge() )+
  #facet_wrap(~platform)+
  #scale_y_continuous(limits=c(0,50))+
  theme(axis.title.y = element_text(color = "black"),
        axis.title.y.right = element_text(color = "black"), 
        panel.background = element_rect(fill = 'white' , colour = 'black'))+
  #scale_x_discrete(labels =c("scent","water")) +
ggtitle("Mindelsee-scent-vs-water")+
xlab("")
p









#STATISTIK
summarize(n=n(),bpmmean= mean(bpm),sdbpm= sd(bpm), minbpm=min(bpm), maxbpm= max(bpm),tempmean= mean(temperature.ib), tempsd= sd(temperature.ib), tempmin= min(temperature.ib),tempmax= max(temperature.ib), ambmean= mean(ambient), ambsd= sd(ambient), ambmin= min(ambient),ambmax= max(ambient))
