library(readr)
library(data.table)
setwd("/Volumes/LaCie/Master/MasterArbeit/Auswertung/tadaridaoutput/")

files_Tadarida <- dir("J:/monitoring_2021/Tadarida.output.2021", full.names = TRUE) #where you have your files
dataframe <- do.call(rbindlist(),lapply(files_Tadarida,fread))
df <- fread(files_Tadarida[1])

hist(df$noise, breaks = 1000)
hist(df$Amp1[df$noise < 0.1], breaks = 10000, xlim = c(0,50))
plot(df$Amp1, df$Nycnoc)
hist(df$Dur, breaks = 10000, xlim = c(0,20))
hist(df$Fmax, breaks = 10000)
plot(df$Pippip, df$Fmax)


bdfTadarida <- dataframe
filtereddf <- bdfTadarida %>%
  filter(noise < 0.35)


write_csv2(bdfTadarida, file = "/Volumes/LaCie/Master/MasterArbeit/Auswertung/bdfTadarida.csv")

setwd("~/ownCloud")
read_csv2()