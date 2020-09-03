library(dplyr)
library(tidyr)
#150-m scale data from 2016
vegHF.150.2016<-read.csv("0_data/processed/10_vegHF150mscale_2016_stationgridassignment.csv", header=TRUE)
str(vegHF.150.2016)#1500 obs

#Create unique "landscapes"
vegHF.150.2016$L2x2<-paste0("L-",vegHF.150.2016$Gridnum,"-",vegHF.150.2016$g2x2)
vegHF.150.2016$L3x3<-paste0("L-",vegHF.150.2016$Gridnum,"-",vegHF.150.2016$g3x3)
vegHF.150.2016$L4x4<-paste0("L-",vegHF.150.2016$Gridnum,"-",vegHF.150.2016$g4x4)
vegHF.150.2016$L5x5<-paste0("L-",vegHF.150.2016$Gridnum,"-",vegHF.150.2016$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date

Summ2x2<-vegHF.150.2016%>%
  group_by(L2x2)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ2x2<-data.frame(Summ2x2)
write.csv(Summ2x2, file="0_data/processed/different scales/vegHF.150.2016.2x2.csv")

Summ3x3<-vegHF.150.2016%>%
  group_by(L3x3)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ3x3<-data.frame(Summ3x3)
write.csv(Summ3x3, file="0_data/processed/different scales/vegHF.150.2016.3x3.csv")

Summ4x4<-vegHF.150.2016%>%
  group_by(L4x4)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ4x4<-data.frame(Summ4x4)
write.csv(Summ4x4, file="0_data/processed/different scales/vegHF.150.2016.4x4.csv")

Summ5x5<-vegHF.150.2016%>%
  group_by(L5x5)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ5x5<-data.frame(Summ5x5)
write.csv(Summ5x5, file="0_data/processed/different scales/vegHF.150.2016.5x5.csv")

#150-m scale data from 2017
vegHF.150.2017<-read.csv("0_data/processed/10_vegHF150mscale_2017_stationgridassignment.csv", header=TRUE)
str(vegHF.150.2017)#1500 obs

#Create unique "landscapes"
vegHF.150.2017$L2x2<-paste0("L-",vegHF.150.2017$Gridnum,"-",vegHF.150.2017$g2x2)
vegHF.150.2017$L3x3<-paste0("L-",vegHF.150.2017$Gridnum,"-",vegHF.150.2017$g3x3)
vegHF.150.2017$L4x4<-paste0("L-",vegHF.150.2017$Gridnum,"-",vegHF.150.2017$g4x4)
vegHF.150.2017$L5x5<-paste0("L-",vegHF.150.2017$Gridnum,"-",vegHF.150.2017$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date

Summ2x2<-vegHF.150.2017%>%
  group_by(L2x2)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ2x2<-data.frame(Summ2x2)
write.csv(Summ2x2, file="0_data/processed/different scales/vegHF.150.2017.2x2.csv")

Summ3x3<-vegHF.150.2017%>%
  group_by(L3x3)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ3x3<-data.frame(Summ3x3)
write.csv(Summ3x3, file="0_data/processed/different scales/vegHF.150.2017.3x3.csv")

Summ4x4<-vegHF.150.2017%>%
  group_by(L4x4)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ4x4<-data.frame(Summ4x4)
write.csv(Summ4x4, file="0_data/processed/different scales/vegHF.150.2017.4x4.csv")

Summ5x5<-vegHF.150.2017%>%
  group_by(L5x5)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ5x5<-data.frame(Summ5x5)
write.csv(Summ5x5, file="0_data/processed/different scales/vegHF.150.2017.5x5.csv")

#600-m square data from 2016
vegHF.600.2016<-read.csv("0_data/processed/10_vegHF36000m2scale_2016_stationgridassignment.csv", header=TRUE)
str(vegHF.600.2016)#1500 obs

#Create unique "landscapes"
vegHF.600.2016$L2x2<-paste0("L-",vegHF.600.2016$Gridnum,"-",vegHF.600.2016$g2x2)
vegHF.600.2016$L3x3<-paste0("L-",vegHF.600.2016$Gridnum,"-",vegHF.600.2016$g3x3)
vegHF.600.2016$L4x4<-paste0("L-",vegHF.600.2016$Gridnum,"-",vegHF.600.2016$g4x4)
vegHF.600.2016$L5x5<-paste0("L-",vegHF.600.2016$Gridnum,"-",vegHF.600.2016$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date

Summ2x2B<-vegHF.600.2016%>%
  group_by(L2x2)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ2x2B<-data.frame(Summ2x2B)
write.csv(Summ2x2B, file="0_data/processed/different scales/vegHF.600.2016.2x2.csv")

Summ3x3B<-vegHF.600.2016%>%
  group_by(L3x3)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ3x3B<-data.frame(Summ3x3B)
write.csv(Summ3x3B, file="0_data/processed/different scales/vegHF.600.2016.3x3.csv")

Summ4x4B<-vegHF.600.2016%>%
  group_by(L4x4)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ4x4B<-data.frame(Summ4x4B)
write.csv(Summ4x4B, file="0_data/processed/different scales/vegHF.600.2016.4x4.csv")

Summ5x5B<-vegHF.600.2016%>%
  group_by(L5x5)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ5x5B<-data.frame(Summ5x5B)
write.csv(Summ5x5B, file="0_data/processed/different scales/vegHF.600.2016.5x5.csv")


#600-m square data from 2017
vegHF.600.2017<-read.csv("0_data/processed/10_vegHF36000m2scale_2017_stationgridassignment.csv", header=TRUE)
str(vegHF.600.2017)#1500 obs

#Create unique "landscapes"
vegHF.600.2017$L2x2<-paste0("L-",vegHF.600.2017$Gridnum,"-",vegHF.600.2017$g2x2)
vegHF.600.2017$L3x3<-paste0("L-",vegHF.600.2017$Gridnum,"-",vegHF.600.2017$g3x3)
vegHF.600.2017$L4x4<-paste0("L-",vegHF.600.2017$Gridnum,"-",vegHF.600.2017$g4x4)
vegHF.600.2017$L5x5<-paste0("L-",vegHF.600.2017$Gridnum,"-",vegHF.600.2017$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date

Summ2x2B<-vegHF.600.2017%>%
  group_by(L2x2)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ2x2B<-data.frame(Summ2x2B)
write.csv(Summ2x2B, file="0_data/processed/different scales/vegHF.600.2017.2x2.csv")

Summ3x3B<-vegHF.600.2017%>%
  group_by(L3x3)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ3x3B<-data.frame(Summ3x3B)
write.csv(Summ3x3B, file="0_data/processed/different scales/vegHF.600.2017.3x3.csv")

Summ4x4B<-vegHF.600.2017%>%
  group_by(L4x4)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ4x4B<-data.frame(Summ4x4B)
write.csv(Summ4x4B, file="0_data/processed/different scales/vegHF.600.2017.4x4.csv")

Summ5x5B<-vegHF.600.2017%>%
  group_by(L5x5)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ5x5B<-data.frame(Summ5x5B)
write.csv(Summ5x5B, file="0_data/processed/different scales/vegHF.600.2017.5x5.csv")






