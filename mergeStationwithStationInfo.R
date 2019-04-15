# merge final u. Stationsinfos


table <- read.table("file:///X:/p3_working/SDuesing-home/Data/LfULG/StickoxidProjekt/StationOverview.dat",sep="\t",header=T,stringsAsFactors = T)
names(table)<- c("site","date","Radiation","p","NO","NO2","nox","O3","rH","T","ws","wd")
table.stations <- read.table("file:///X:/p3_working/SDuesing-home/Data/LfULG/StickoxidProjekt/Data/Stationsbesonderheiten.txt",sep="\t",header=T)
final <- dplyr::full_join(table,table.stations,by="site")
write.table(final, "Stations_Overview_withInformations.dat",col.names = T,row.names = F,append=F,quote=F,sep="\t",dec = ".")
