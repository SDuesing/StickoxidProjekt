#Trend in Bermannstraﬂe
library(openair)
library(reshape2)
library(ggplot2)
library("gplots")
scalingFactor <- 2
table <- read.table("file:///X:/p3_working/SDuesing-home/Data/LfULG/StickoxidProjekt/StationOverview.dat",sep="\t",header=T,stringsAsFactors = T)
names(table)<- c("site","date","Radiation","p","NO","NO2","nox","O3","rH","T","ws","wd")


bergStrasse <- table[table$site=="DESN084",]
bergStrasse$date <- as.POSIXct(strptime(bergStrasse$date,format = "%Y-%m-%d %H:%M:%S",tz="UTC"),tz="UTC")-3600
bergStrasse$wd[bergStrasse$wd==555] <- NA

png("Dresden-bergStraﬂe_zusammenfassung.png",width = 16*scalingFactor,height=9*scalingFactor,units="cm",res=300,family = "serif",bg="transparent",pointsize = 12)
summaryPlot(bergStrasse)
dev.off()

png("Dresden-bergStraﬂe_generell.png",width = 16*scalingFactor,height=9*scalingFactor,units="cm",res=300,family = "serif",bg="transparent",pointsize = 12)
timeVariation(bergStrasse[,2:ncol(bergStrasse)])
dev.off()

png("Dresden-bergStraﬂe_saisonal.png",width = 16*scalingFactor,height=9*scalingFactor,units="cm",res=300,family = "serif",bg="transparent",pointsize = 12)
timePlot(bergStrasse[,2:ncol(bergStrasse)],type = "season",group=T,pollutant = c("nox","NO","NO2"))
dev.off()

png("Dresden-bergStraﬂe_j‰hrlich.png",width = 16*scalingFactor,height=9*scalingFactor,units="cm",res=300,family = "serif",bg="transparent",pointsize = 12)
timePlot(bergStrasse[,2:ncol(bergStrasse)],type = "year",stack = T,group=T,pollutant = c("nox","NO","NO2"))
dev.off()
for (n in names(bergStrasse[,3:ncol(bergStrasse)])){
  png(paste("Dresden-bergStraﬂe_CalendarPlot_",n,".png",sep=""),width = 16*scalingFactor,height=9*scalingFactor,units="cm",res=300,family = "serif",bg="transparent",pointsize = 12)
  calendarPlot(bergStrasse[,2:ncol(bergStrasse)],pol=n,key.header = n)
  dev.off()
}

for (pol in c("nox","NO","NO2")){
  i=1
  par(mfrow=c(3,1))
  for (y in c(2016,2017,2018)){
    subTable <- selectByDate(bergStrasse, year=y)
    plot(subTable$date,subTable[,pol],type="l",col=i,ylim=c(0,max(na.omit(bergStrasse[,pol]))))
    i=i+1
    }
}
dev.off()

bergStrasseVor <- selectByDate(bergStrasse, start = "2016-01-01", end = "2018-01-31")
bergStrasseNach <- selectByDate(bergStrasse, start = "2018-02-01", end = "2018-12-31")
png("HIST_bergstraﬂe_NO.png",width = 10,height=10,family ="serif",pointsize = 12,bg="transparent",units="cm",res=300)
hist(bergStrasseVor$NO, breaks=40, col=rgb(0,0,1,1/4),probability= T,ylim=c(0,0.025),xaxs="i",yaxs="i",ylab="Wahrscheinlichkeit",xlab=expression(paste(italic("NO")," [µg m"^-3,"]")),main = "NO in bergstraﬂe Dresden")  # first histogram
hist(bergStrasseNach$NO, breaks=40, col=rgb(1,0,0,1/4),add=T,probability=T,ylim=c(0,0.025),xaxs="i",yaxs="i",ylab=NA,xlab=NA)
legend("topright", legend=c("Vor Durchlasshˆhen‰nderung", "Nach Durchlasshˆhen‰nderung"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)),bty="n")
dev.off()
countsNach <- NULL
countsVor <- NULL
for(bins in seq(0,550,10)){
  countsNach <- c(countsNach,length(which(bergStrasseNach$NO<bins+10&bergStrasseNach$NO>=bins))) 
  countsVor <- c(countsVor,length(which(bergStrasseVor$NO<bins+10&bergStrasseVor$NO>=bins)))
}


test <- rbind(data.frame(x=bergStrasseVor$NO[],state ="vor"),data.frame(x=bergStrasseNach$NO[],state="nach"))
png("Bergstraﬂe_Dresden_NO.png",width = 16,height = 9,res=300,units = "cm",family = "serif",pointsize = 12,bg = "transparent")
ggplot(test, aes(x, fill=state)) +
  geom_histogram(aes(y=..density..),breaks=seq(0,550,10), alpha=0.6, 
                 position="identity") +
  ggtitle("Dresden Bergstraﬂe NO") + 
  xlab( expression(paste("m"["NO"]," [µg m"^-3,"]"))) +
  ylab("Wahrscheinlichkeit") + 
  scale_fill_manual(name="Einlasshˆhe",
                    values=c("red","blue"),
                      breaks=c("vor", "nach"),
                      labels=c("1.8 m", "3.6 m")) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14),
        legend.text  = element_text(size=12),
        axis.title.x = element_text(size=14,colour = "black"),
        axis.title.y = element_text(size=14,colour = "black"),
          panel.grid =  element_line(colour = "gray",linetype = 1),
        panel.background = element_blank())
dev.off()
countsNach <- NULL
countsVor <- NULL
for(bins in seq(0,950,25)){
  countsNach <- c(countsNach,length(which(bergStrasseNach$nox<bins+25&bergStrasseNach$nox>=bins))) 
  countsVor <- c(countsVor,length(which(bergStrasseVor$nox<bins+25&bergStrasseVor$nox>=bins)))
}

png("Bergstraﬂe_Dresden_NOx.png",width = 16,height = 9,res=300,units = "cm",family = "serif",pointsize = 12,bg = "transparent")
test <- rbind(data.frame(x=bergStrasseVor$nox[],state ="vor"),data.frame(x=bergStrasseNach$nox[],state="nach"))
ggplot(test, aes(x, fill=state)) +
  geom_histogram(aes(y=..density..),breaks=seq(0,950,25), alpha=0.6, 
                 position="identity") +
  ggtitle("Dresden Bergstraﬂe Nox") + 
  xlab( expression(paste("m"["NOx"]," [µg m"^-3,"]"))) +
  ylab("Wahrscheinlichkeit") + 
  scale_fill_manual(name="Einlasshˆhe",
                    values=c("red","blue"),
                    breaks=c("vor", "nach"),
                    labels=c("1.8 m", "3.6 m")) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14),
        legend.text  = element_text(size=12),
        axis.title.x = element_text(size=14,colour = "black"),
        axis.title.y = element_text(size=14,colour = "black"),
        panel.grid =  element_line(colour = "gray",linetype = 1),
        panel.background = element_blank())

dev.off()
countsNach <- NULL
countsVor <- NULL
for(bins in seq(0,180,5)){
  countsNach <- c(countsNach,length(which(bergStrasseNach$NO2<bins+5&bergStrasseNach$NO2>=bins))) 
  countsVor <- c(countsVor,length(which(bergStrasseVor$NO2<bins+5&bergStrasseVor$NO2>=bins)))
}

test <- rbind(data.frame(x=bergStrasseVor$NO2[],state ="vor"),data.frame(x=bergStrasseNach$NO2[],state="nach"))
png("Bergstraﬂe_Dresden_NO2.png",width = 16,height = 9,res=300,units = "cm",family = "serif",pointsize = 12,bg = "transparent")
ggplot(test, aes(x, fill=state)) +
  geom_histogram(aes(y=..density..),breaks=seq(0,180,5), alpha=0.6, 
                 position="identity") +
  ggtitle("Dresden Bergstraﬂe N02") + 
  xlab( expression(paste("m"["NO2"]," [µg m"^-3,"]"))) +
  ylab("Wahrscheinlichkeit") + 
  scale_fill_manual(name="Einlasshˆhe",
                    values=c("red","blue"),
                    breaks=c("vor", "nach"),
                    labels=c("1.8 m", "3.6 m")) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.title = element_text(size=14),
        legend.text  = element_text(size=12),
        axis.title.x = element_text(size=14,colour = "black"),
        axis.title.y = element_text(size=14,colour = "black"),
        panel.grid =  element_line(colour = "gray",linetype = 1),
        panel.background = element_blank())
dev.off()


res<-corPlot(bergStrasse)
corPlot(bergStrasse,type = "season")
corPlot(bergStrasse,type = "year")
plot(res$clust)
trendLevel(bergStrasse, pol="O3") # daily variation of mean of a pollutant across months 
res.cor <- cor(bergStrasse[complete.cases(bergStrasse),3:ncol(bergStrasse)], method = "pearson")
d.cor <- as.dist(1 - res.cor)
plot(hclust(d.cor, method = "ward.D2"), cex = 0.6)


#heatmap.2(as.matrix(bergStrasse[complete.cases(bergStrasse),3:ncol(bergStrasse)]), scale = "none", col = bluered(100),trace = "none", density.info = "none")
#heatmap.2(as.matrix(bergStrasse[complete.cases(bergStrasse),3:ncol(bergStrasse)]))
# a$counts <- log(a$counts, 10)
# b$counts <- log(b$counts, 10)
# 
# plot(a,ylim=c(0.001,5),col=rgb(0,0,1,1/4))
# plot(b,add=T,ylim=c(0.001,5),col=rgb(1,0,0,1/4))
#summaryPlot(table)
