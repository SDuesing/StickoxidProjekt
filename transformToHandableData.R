library(openair)
library(reshape2)
library("gplots")
f.list <- list.files(path = "X:/p3_working/SDuesing-home/Data/LfULG/StickoxidProjekt/Data/",pattern=".csv")

table.final <- NULL
for (i in 1:length(f.list)){
  table <- read.csv(paste("./Data/",f.list[i],sep=""),quote="'",sep=";",header = TRUE,dec = ".",na.strings = c("-999.0","  -999.00"),fill = TRUE,stringsAsFactors = T,strip.white = TRUE)
  table.final <- rbind(table.final,table)
}
table.final[table.final==-999]<-NA

names(table.final)[3]<-"date"
table.final$date <- as.POSIXct(strptime(as.character(table.final$date),format = "%Y%m%d",tz="UTC"),tz="UTC") #converts Datum to date

table.final.new <- table.final[rep(1:nrow(table.final),each=24),]
table.final.new$date <- table.final.new$date+rep(1:24*3600,nrow(table.final))
final.vector <- vector("numeric",length = nrow(table.final.new))
for(i in 0:23){
final.vector[1:nrow(table.final.new)%%24==i]  <- as.vector(table.final.new[1:nrow(table.final.new)%%24==i,(i+4)])
}


table.final.final <- cbind(table.final.new[,c(1:3,28)],unlist(final.vector))
names(table.final.final)[c(2,4,5)]<-c("variable","Nachweisgrenze","value")

final <- dcast(table.final.final,Station + date ~ variable, value.var = "value")
write.table(final,"StationOverview.dat",col.names = T,row.names = F,append=F,quote=F,sep="\t")

