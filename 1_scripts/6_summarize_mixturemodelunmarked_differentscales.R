library(dplyr)
library(tidyr)

grid.assign<-read.csv("0_data/processed/10_stationgridassignment.csv", header=TRUE)
str(grid.assign)

#bird count data in format suitable for mixture models in unmarked
names<-c("HETH")
for (i in names){
  spp.ct<-read.csv(paste0("0_data/processed/mixture model input files/",i,".csv"), header=TRUE)
  str(spp.ct)#1767 obs (SS, rounds 1-4)
  spp.ct$location<-spp.ct$SS
  spp.ct<-spp.ct%>%separate(location, c("Project","Gridnum","station"),sep="_")
  str(spp.ct)
  spp.ct$station<-as.integer(spp.ct$station)
  
  spp.ct.gridID<-merge(spp.ct, grid.assign, by=c("station"))
  
  #Create unique "landscapes"
  spp.ct.gridID$L2x2<-paste0("L-",spp.ct.gridID$Gridnum,"-",spp.ct.gridID$g2x2)
  spp.ct.gridID$L3x3<-paste0("L-",spp.ct.gridID$Gridnum,"-",spp.ct.gridID$g3x3)
  spp.ct.gridID$L4x4<-paste0("L-",spp.ct.gridID$Gridnum,"-",spp.ct.gridID$g4x4)
  spp.ct.gridID$L5x5<-paste0("L-",spp.ct.gridID$Gridnum,"-",spp.ct.gridID$g5x5)
  #The "L-" prefix for landscape is to keep the unique landscape ID
  #from being converted to a date
  str(spp.ct.gridID)
  
  Summ2x2<-spp.ct.gridID%>%
    group_by(L2x2)%>%
    summarise_at(vars(X1:X4), list(sum=sum), na.rm=TRUE)
  Summ2x2<-data.frame(Summ2x2)
  write.csv(Summ2x2, file=paste0("0_data/processed/different scales/",i,"Nmix.2x2.csv"))
  
  Summ3x3<-spp.ct.gridID%>%
    group_by(L3x3)%>%
    summarise_at(vars(X1:X4), list(sum=sum), na.rm=TRUE)
  Summ3x3<-data.frame(Summ3x3)
  write.csv(Summ3x3, file=paste0("0_data/processed/different scales/",i,".3x3.csv"))
  
  Summ4x4<-spp.ct.gridID%>%
    group_by(L4x4)%>%
    summarise_at(vars(X1:X4), list(sum=sum), na.rm=TRUE)
  Summ4x4<-data.frame(Summ4x4)
  write.csv(Summ4x4, file=paste0("0_data/processed/different scales/",i,".4x4.csv"))
  
  Summ5x5<-spp.ct.gridID%>%
    group_by(L5x5)%>%
    summarise_at(vars(X1:X4), list(sum=sum), na.rm=TRUE)
  Summ5x5<-data.frame(Summ5x5)
  write.csv(Summ5x5, file=paste0("0_data/processed/different scales/",i,".5x5.csv"))
}
#Note that right now, landscape scale counts ignore NA values
#So landscapes missing recordings in a given round will have 
#underestimated counts.

#32 stations don't have a round 2/3/4
#157 stations don't have a round 3/4
#510 stations don't have a round 4

#detection covariate data in format suitable for mixture models in unmarked
names<-c("Julian","Time")
for (i in names){
  spp.ct<-read.csv(paste0("0_data/processed/mixture model input files/",i,".csv"), header=TRUE)
  str(spp.ct)#1767 obs (SS, rounds 1-4)
  spp.ct$location<-spp.ct$SS
  spp.ct<-spp.ct%>%separate(location, c("Project","Gridnum","station"),sep="_")
  str(spp.ct)
  spp.ct$station<-as.integer(spp.ct$station)
  
  spp.ct.gridID<-merge(spp.ct, grid.assign, by=c("station"))
  
  #Create unique "landscapes"
  spp.ct.gridID$L2x2<-paste0("L-",spp.ct.gridID$Gridnum,"-",spp.ct.gridID$g2x2)
  spp.ct.gridID$L3x3<-paste0("L-",spp.ct.gridID$Gridnum,"-",spp.ct.gridID$g3x3)
  spp.ct.gridID$L4x4<-paste0("L-",spp.ct.gridID$Gridnum,"-",spp.ct.gridID$g4x4)
  spp.ct.gridID$L5x5<-paste0("L-",spp.ct.gridID$Gridnum,"-",spp.ct.gridID$g5x5)
  #The "L-" prefix for landscape is to keep the unique landscape ID
  #from being converted to a date
  str(spp.ct.gridID)
  
  Summ2x2<-spp.ct.gridID%>%
    group_by(L2x2)%>%
    summarise_at(vars(X1:X4), list(mean=mean), na.rm=TRUE)
  Summ2x2<-data.frame(Summ2x2)
  write.csv(Summ2x2, file=paste0("0_data/processed/different scales/",i,".2x2.csv"))
  
  Summ3x3<-spp.ct.gridID%>%
    group_by(L3x3)%>%
    summarise_at(vars(X1:X4), list(mean=mean), na.rm=TRUE)
  Summ3x3<-data.frame(Summ3x3)
  write.csv(Summ3x3, file=paste0("0_data/processed/different scales/",i,".3x3.csv"))
  
  Summ4x4<-spp.ct.gridID%>%
    group_by(L4x4)%>%
    summarise_at(vars(X1:X4), list(mean=mean), na.rm=TRUE)
  Summ4x4<-data.frame(Summ4x4)
  write.csv(Summ4x4, file=paste0("0_data/processed/different scales/",i,".4x4.csv"))
  
  Summ5x5<-spp.ct.gridID%>%
    group_by(L5x5)%>%
    summarise_at(vars(X1:X4), list(mean=mean), na.rm=TRUE)
  Summ5x5<-data.frame(Summ5x5)
  write.csv(Summ5x5, file=paste0("0_data/processed/different scales/",i,".5x5.csv"))
}
