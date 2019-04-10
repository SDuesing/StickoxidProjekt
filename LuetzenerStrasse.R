#Lützener Straße 

library(openair)
library(reshape2)
library(ggplot2)
library("gplots")
scalingFactor <- 2
table <- read.table("file:///X:/p3_working/SDuesing-home/Data/LfULG/StickoxidProjekt/StationOverview.dat",sep="\t",header=T,stringsAsFactors = T)
names(table)<- c("site","date","Radiation","p","NO","NO2","nox","O3","rH","T","ws","wd")


luetzener <- table[table$site=="DESN077",]
luetzener$date <- as.POSIXct(strptime(luetzener$date,format = "%Y-%m-%d %H:%M:%S",tz="UTC"),tz="UTC")-3600
luetzener$wd[luetzener$wd==555] <- NA

png("Luetzener_zusammenfassung.png",width = 16*scalingFactor,height=9*scalingFactor,units="cm",res=300,family = "serif",bg="transparent",pointsize = 12)
summaryPlot(luetzener)
dev.off()