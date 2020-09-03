library(dplyr)
library(tidyr)

grid.assign<-read.csv("0_data/processed/10_stationgridassignment.csv", header=TRUE)
str(grid.assign)

#bird data
birds.GLMs<-read.csv("0_data/processed/6b_birdspervisit_readyforGLMs.csv", header=TRUE)
str(birds.GLMs)#1500 obs

birds.GLMs.summ<-birds.GLMs%>%
  group_by(SS)%>%
  summarise_at(vars(ALFL:YRWA), list(max=max))
spp.ct<-data.frame(birds.GLMs.summ)

spp.ct$location<-spp.ct$SS
spp.ct<-spp.ct%>%separate(location, c("Project","Gridnum","station"),sep="_")
str(spp.ct)
spp.ct$station<-as.integer(spp.ct$station)

birds.GridID<-merge(spp.ct, grid.assign, by=c("station"))

#Create unique "landscapes"
birds.GridID$L2x2<-paste0("L-",birds.GridID$Gridnum,"-",birds.GridID$g2x2)
birds.GridID$L3x3<-paste0("L-",birds.GridID$Gridnum,"-",birds.GridID$g3x3)
birds.GridID$L4x4<-paste0("L-",birds.GridID$Gridnum,"-",birds.GridID$g4x4)
birds.GridID$L5x5<-paste0("L-",birds.GridID$Gridnum,"-",birds.GridID$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date

Summ2x2<-birds.GridID%>%
  group_by(L2x2)%>%
  summarise_at(vars(ALFL_max:YRWA_max), list(sum=sum))
Summ2x2<-data.frame(Summ2x2)
write.csv(Summ2x2, file="0_data/processed/different scales/birds.GLMs.2x2.csv")

Summ3x3<-birds.GridID%>%
  group_by(L3x3)%>%
  summarise_at(vars(ALFL_max:YRWA_max), list(sum=sum))
Summ3x3<-data.frame(Summ3x3)
write.csv(Summ3x3, file="0_data/processed/different scales/birds.GLMs.3x3.csv")

Summ4x4<-birds.GridID%>%
  group_by(L4x4)%>%
  summarise_at(vars(ALFL_max:YRWA_max), list(sum=sum))
Summ4x4<-data.frame(Summ4x4)
write.csv(Summ4x4, file="0_data/processed/different scales/birds.GLMs.4x4.csv")

Summ5x5<-birds.GridID%>%
  group_by(L5x5)%>%
  summarise_at(vars(ALFL_max:YRWA_max), list(sum=sum))
Summ5x5<-data.frame(Summ5x5)
write.csv(Summ5x5, file="0_data/processed/different scales/birds.GLMs.5x5.csv")

