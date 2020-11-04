library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

bg.3min.song.birds<-read.csv("0_data/processed/4_Big_Grids_BirdsOnly_3min_AsOfNov5_2020.csv", header=TRUE)
nrow(bg.3min.song.birds)#78860
str(bg.3min.song.birds)#66 variables

bg.3min.song.birds$source_file_name<-str_replace_all(bg.3min.song.birds$source_file_name, fixed("_"), "-")
#change underscores to hyphens so that source_file_name can be included in VISIT

#for some reason, tapply converts the StationKey portion of visit name to 
#a number when StationKey is the first part of visit name
bg.3min.song.birds$VISIT<-paste0(bg.3min.song.birds$Year,"_",
                                 bg.3min.song.birds$Project,"_",#SS recreated from VISIT later
                                 bg.3min.song.birds$Gridnum,"_",#SS recreated from VISIT later
                                 bg.3min.song.birds$StationNum,"_",#SS recreated from VISIT later
                                 bg.3min.song.birds$recording_date,"_",
                                 bg.3min.song.birds$recording_time,"_",
                                 bg.3min.song.birds$method,"_",
                                 bg.3min.song.birds$transcriber,"_",
                                 bg.3min.song.birds$rain,"_",
                                 bg.3min.song.birds$wind,"_",
                                 bg.3min.song.birds$industry_noise,"_",
                                 bg.3min.song.birds$noise,"_",
                                 bg.3min.song.birds$latitude,"_",
                                 bg.3min.song.birds$longitude,"_",
                                 bg.3min.song.birds$source_file_name)

#count birds using just the singing observations
bg.3min.song.birds$birdabund.songonly[is.na(bg.3min.song.birds$birdabund.songonly)]<-0
bg.3min.song.birds$birdabund.songonly[bg.3min.song.birds$species_code=="NONE"]<-1
#reassign birdabund.songonly to =1 if species_code="NONE" so we can easily filter 
#those point count visits. Reassigning birdabund.songonly==NA for species_code==NONE 
#is important but it could also be assigned a value of 0. But if it's left as
#NA, then whole visits will be excluded from the data summary!


bg.3min.song.birds$VISIT.f<-as.factor(as.character(bg.3min.song.birds$VISIT))
tapply.spp<-tapply(bg.3min.song.birds$birdabund.songonly, list(bg.3min.song.birds$VISIT.f, bg.3min.song.birds$species_code), sum, na.rm=TRUE)
tapply.spp<-data.frame(tapply.spp)
tapply.spp$VISIT<-row.names(tapply.spp)
colnames(tapply.spp)#VISIT column is last; put first, then replace NA values with 0
tapply.rearr<-tapply.spp%>%
  select(VISIT, everything())
colnames(tapply.rearr)#VISIT column is now first
species<-names(tapply.rearr[,2:ncol(tapply.rearr)])
for (i in species){
  #tapply.rearr$spp<-tapply.rearr[,i]
  #tapply.rearr$spp[is.na(tapply.rearr$spp)]<-0
  tapply.rearr[,i]<-ifelse(is.na(tapply.rearr[,i]),0,tapply.rearr[,i])
}

#Get rid of any non-bird observations except NONE
#This should be done AFTER data have been summarized for visits,
#just in case one of species we're removing is the only observation 
#for that visit; otherwise whole visit will be deleted (never summarized)
levels(as.factor(bg.3min.song.birds$species_code))
levels(as.factor(bg.3min.song.birds$species_english_name))

tapply.rearr$BADG<-NULL #Badger
tapply.rearr$BCFR<-NULL #Boreal Chorus Frog
tapply.rearr$BEAV<-NULL #Beaver
tapply.rearr$CRCH<-NULL #Unknown Crunch Sound
tapply.rearr$COWW<-NULL #Cow
tapply.rearr$COYT<-NULL #Coyote
tapply.rearr$DOGG<-NULL #Dog
tapply.rearr$HEAI<-NULL #Heavy aircraft noise 
tapply.rearr$HEBA<-NULL #Heavy background noise
tapply.rearr$HEDT<-NULL #Heavy deterrents
tapply.rearr$HENO<-NULL #Heavy continuous industrial noise
tapply.rearr$HERA<-NULL #Heavy rain
tapply.rearr$HETN<-NULL #Heavy train
tapply.rearr$HETR<-NULL #Heavy traffic
tapply.rearr$HEWI<-NULL #Heavy wind
tapply.rearr$LIAI<-NULL #Light aircraft noise 
tapply.rearr$LIBA<-NULL #Light background noise
tapply.rearr$LIDT<-NULL #Light deterrents
tapply.rearr$LINO<-NULL #Light continuous industrial noise
tapply.rearr$LIRA<-NULL #Light rain
tapply.rearr$LITN<-NULL #Light train
tapply.rearr$LITR<-NULL #Light traffic
tapply.rearr$LIWI<-NULL #Light wind
tapply.rearr$MOAI<-NULL #Moderate aircraft noise 
tapply.rearr$MOBA<-NULL #Moderate background noise
tapply.rearr$MODT<-NULL #Moderate deterrents
tapply.rearr$MONO<-NULL #Moderate continuous industrial noise
tapply.rearr$MORA<-NULL #Moderate rain
tapply.rearr$MOTN<-NULL #Moderate train
tapply.rearr$MOTR<-NULL #Moderate traffic
tapply.rearr$MOWI<-NULL #Moderate wind
tapply.rearr$RESQ<-NULL #Red squirrel
tapply.rearr$UNFR<-NULL #Unknown frog
tapply.rearr$UNMA<-NULL #Unknown mammal
tapply.rearr$WETO<-NULL #Western toad
tapply.rearr$WTDE<-NULL #White-tailed deer
tapply.rearr$WOLF<-NULL #Wolf
tapply.rearr$WOFR<-NULL #Wood frog
write.csv(tapply.rearr, file = "0_data/processed/5_singingbird_3min_abundpervisit_asofNov5.csv")  


tapply.spp<-read.csv("0_data/processed/5_singingbird_3min_abundpervisit_asofNov5.csv",header=TRUE)
tapply.spp.wide<-tapply.spp%>%separate(VISIT, c("Year","Project",
                                                "Gridnum","StationNum",
                                                "recording_date",
                                                "recording_time",
                                                "method",
                                                "transcriber",
                                                "rain",
                                                "wind",
                                                "industry_noise",
                                                "noise",
                                                "latitude",
                                                "longitude",
                                                "source_file_name"),sep="_")



tapply.spp.wide$SS<-paste0(tapply.spp.wide$Project,"_",tapply.spp.wide$Gridnum,"_",tapply.spp.wide$StationNum)
tapply.spp.wide$GridnumZ<-ifelse(as.numeric(tapply.spp.wide$Gridnum)<10, paste0("000",tapply.spp.wide$Gridnum), paste0("00",tapply.spp.wide$Gridnum))
tapply.spp.wide$StationnumZ<-ifelse(as.numeric(tapply.spp.wide$StationNum)<10, paste0("00",tapply.spp.wide$StationNum), 
                           ifelse(as.numeric(tapply.spp.wide$StationNum)<100, paste0("0",tapply.spp.wide$StationNum), tapply.spp.wide$StationNum))
tapply.spp.wide$Site<-paste0(tapply.spp.wide$Project,"-",tapply.spp.wide$GridnumZ)
tapply.spp.wide$StationKey<-paste0(tapply.spp.wide$Project,"-",tapply.spp.wide$GridnumZ,"-",tapply.spp.wide$StationnumZ)

tapply.spp.wide$DateTime<-paste0(tapply.spp.wide$recording_date," ",tapply.spp.wide$recording_time)
tapply.spp.wide$lubridated<-ymd_hms(tapply.spp.wide$DateTime, tz=Sys.timezone())
tapply.spp.wide$Year<-year(tapply.spp.wide$lubridated)
tapply.spp.wide$Month<-month(tapply.spp.wide$lubridated)
tapply.spp.wide$Day<-day(tapply.spp.wide$lubridated)
tapply.spp.wide$Hour<-hour(tapply.spp.wide$lubridated)
tapply.spp.wide$Minute<-minute(tapply.spp.wide$lubridated)
tapply.spp.wide$Second<-second(tapply.spp.wide$lubridated)
write.csv(tapply.spp.wide, file="0_data/processed/6_birdspervisit_visitparsed_asofNov5.csv")

#filter out recordings from night-time (outside of 4-10 AM)
#remove recordings before May 19 and after July 11
#for now this is being done manually.

tapply.spp.wide<-read.csv("0_data/processed/7_birdspervisit_visitparsed_filtereddatetime_asofNov5.csv", header=TRUE)
#cross-tabulate to get the number of visits per station and number of stations
#transcribed per site
mytable.visitsXstation<-table(tapply.spp.wide[,c("SS")]) 
mytable1<-as.data.frame(mytable.visitsXstation)
mytable1$SS<-mytable1$Var1
mytable1$Visits<-mytable1$Freq
mytable1$Var1<-NULL
mytable1$Freq<-NULL
write.csv(mytable1, file="0_data/processed/8a_numberofvisitsperstationkey_asofNov5_2020.csv")

tapply.spp.u<-unique(tapply.spp.wide[,c("Site","SS")])
mytable.stationXsite<-table(tapply.spp.u[,c("Site")])  
mytable2<-as.data.frame(mytable.stationXsite)
mytable2$Site<-mytable2$Var1
mytable2$Stations<-mytable2$Freq
mytable2$Var1<-NULL
mytable2$Freq<-NULL
write.csv(mytable2, file="0_data/processed/8b_numberofstationspersite_asofNov5_2020.csv")
#As of November 5, 2020, 7 stations have <2 visits within the dates and times specified
#As of November 5, 2020, 60 stations have <3 visits within the dates and times specified

