library(tidyr)
library(lubridate)
library(stringr)

bg.3min.song.birds<-read.csv("0_data/processed/4_Big_Grids_BirdSongOnly_3min_AsOfJune30_2020.csv", header=TRUE)
nrow(bg.3min.song.birds)#41895
str(bg.3min.song.birds)#60 var


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
                                 bg.3min.song.birds$longitude)

bg.3min.song.birds$VISIT.f<-as.factor(as.character(bg.3min.song.birds$VISIT))
tapply.spp<-tapply(bg.3min.song.birds$birdabund, list(bg.3min.song.birds$VISIT.f, bg.3min.song.birds$species_code), sum, na.rm=TRUE)
tapply.spp<-data.frame(tapply.spp)
tapply.spp$VISIT<-row.names(tapply.spp)
write.csv(tapply.spp, file = "0_data/processed/5_singingbird_3min_abundpervisit.csv")  


tapply.spp<-read.csv("0_data/processed/5_singingbird_3min_abundpervisit.csv",header=TRUE)
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
                                                "longitude"),sep="_")



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
write.csv(tapply.spp.wide, file="0_data/processed/6_birdspervisit_visitparsed.csv")

#cross-tabulate to get the number of visits per station and number of stations
#transcribed per site
mytable.visitsXstation<-table(tapply.spp.wide[,c("SS")]) 
mytable1<-as.data.frame(mytable.visitsXstation)
mytable1$SS<-mytable1$Var1
mytable1$Visits<-mytable1$Freq
mytable1$Var1<-NULL
mytable1$Freq<-NULL
write.csv(mytable1, file="0_data/processed/7_numberofvisitsperstationkey.csv")

tapply.spp.u<-unique(tapply.spp.wide[,c("Site","SS")])
mytable.stationXsite<-table(tapply.spp.u[,c("Site")])  
mytable2<-as.data.frame(mytable.stationXsite)
mytable2$Site<-mytable2$Var1
mytable2$Stations<-mytable2$Freq
mytable2$Var1<-NULL
mytable2$Freq<-NULL
write.csv(mytable2, file="0_data/processed/8_numberofstationspersite.csv")

#If just the strictly-3-minute recordings are used
#The following numbers of stations have transcribed recordings on each grid
#BG-0001 BG-0002 BG-0003 BG-0004 BG-0005 BG-0006 BG-0007 BG-0008 BG-0009 
#90       1       2       3     100      97      96      99      97 
#BG-0010 BG-0011 BG-0012 BG-0013 BG-0014 BG-0015 BG-0016 BG-0017 BG-0018 
#95      97     100      98     100      98      92      93      96 
#BG-0019 
#32

#Big Grids 2, 3, and 4 seem suspiciously undone while most other grids are
#doing just dandy

#If either the strictly-3-minute recordings or 3-minute + 7-minute recordings are used
#The following numbers of stations have transcribed recordings on each grid
#BG-0001 BG-0002 BG-0003 BG-0004 BG-0005 BG-0006 BG-0007 BG-0008 BG-0009 
#96      98      83     100     100      97      96      99      97 
#BG-0010 BG-0011 BG-0012 BG-0013 BG-0014 BG-0015 BG-0016 BG-0017 BG-0018 
#95      97     100      98     100      98      92      93      96 
#BG-0019 
#32 

#Back to the bird data: rearrange data frame and replace NA counts with
#zeroes

birdspervisit_rearranged<-read.csv("0_data/processed/6_birdspervisit_visitparsed.csv", header=TRUE)
#replace NA values in species counts with zeroes
birdspervisit_rearranged[c("ALFL","AMBI","AMCO","AMCR","AMGO","AMRE","AMRO","AMWI",
                "ATSP","BADO","BAOR","BARS","BAWA","BAWW","BBWA","BCCH",
                "BCFR","BHCO","BHVI","BLBW","BLJA","BLPW","BOCH","BOOW",
                "BOWA","BRBL","BRCR","BRSP","BTGN","BTNW","BWHA","BWWA",
                "CANG","CAWA","CCLO","CCSP","CEDW","CHSP","CMWA","COLO",
                "CONI","CONW","CORA","COYE","CSWA","DEJU","EAKI","EAPH",
                "EUST","FOSP","FRGU","GCKI","GCTH","GHOW","GRAJ","GRCA",
                "GRYE","HASP","HEBA","HEDT","HENO","HERA","HETH","HETR",
                "HEWI","HOFI","HOSP","HOWR","LCSP","LEFL","LEYE","LIAI",
                "LIBA","LIDT","LINO","LIRA","LISP","LITR","LIWI","MAWA",
                "MAWR","MEGU","MGWA","MOAI","MOBA","MODO","MODT","MONO",
                "MORA","MOTR","MOWA","MOWI","MYWA","NAWA","NESP","NOBO",
                "NOFL","NOGO","NOPA","NOWA","NRWS","NSWO","OCWA","OSFL",
                "OVEN","PAWA","PAWR","PBGR","PHVI","PISI","PIWO","PSFL",
                "PUFI","RBGR","RBNU","RCKI","RESQ","REVI","RNGR","RUBL",
                "RUGR","RWBL","SACR","SAPH","SAVS","SCTA","SEWR","SORA",
                "SOSA","SOSP","SPSA","SPTO","SWSP","SWTH","TEWA","TOSO",
                "TRES","UNAM","UNBL","UNCV","UNFI","UNFL","UNKN","UNOW",
                "UNPA","UNSH","UNSP","UNSW","UNTE","UNTH","UNTR","UNVI",
                "UNWA","UNWO","UNYE","VATH","VEER","WAVI","WBBE","WBNU",
                "WCSP","WETA","WEWP","WISN","WIWA","WIWR","WOFR","WTDE",
                "WTSP","WWCR","YBFL","YBSA","YEWA","YHBL","YRWA"
)][is.na(birdspervisit_rearranged[c("ALFL","AMBI","AMCO","AMCR","AMGO","AMRE","AMRO","AMWI",
                         "ATSP","BADO","BAOR","BARS","BAWA","BAWW","BBWA","BCCH",
                         "BCFR","BHCO","BHVI","BLBW","BLJA","BLPW","BOCH","BOOW",
                         "BOWA","BRBL","BRCR","BRSP","BTGN","BTNW","BWHA","BWWA",
                         "CANG","CAWA","CCLO","CCSP","CEDW","CHSP","CMWA","COLO",
                         "CONI","CONW","CORA","COYE","CSWA","DEJU","EAKI","EAPH",
                         "EUST","FOSP","FRGU","GCKI","GCTH","GHOW","GRAJ","GRCA",
                         "GRYE","HASP","HEBA","HEDT","HENO","HERA","HETH","HETR",
                         "HEWI","HOFI","HOSP","HOWR","LCSP","LEFL","LEYE","LIAI",
                         "LIBA","LIDT","LINO","LIRA","LISP","LITR","LIWI","MAWA",
                         "MAWR","MEGU","MGWA","MOAI","MOBA","MODO","MODT","MONO",
                         "MORA","MOTR","MOWA","MOWI","MYWA","NAWA","NESP","NOBO",
                         "NOFL","NOGO","NOPA","NOWA","NRWS","NSWO","OCWA","OSFL",
                         "OVEN","PAWA","PAWR","PBGR","PHVI","PISI","PIWO","PSFL",
                         "PUFI","RBGR","RBNU","RCKI","RESQ","REVI","RNGR","RUBL",
                         "RUGR","RWBL","SACR","SAPH","SAVS","SCTA","SEWR","SORA",
                         "SOSA","SOSP","SPSA","SPTO","SWSP","SWTH","TEWA","TOSO",
                         "TRES","UNAM","UNBL","UNCV","UNFI","UNFL","UNKN","UNOW",
                         "UNPA","UNSH","UNSP","UNSW","UNTE","UNTH","UNTR","UNVI",
                         "UNWA","UNWO","UNYE","VATH","VEER","WAVI","WBBE","WBNU",
                         "WCSP","WETA","WEWP","WISN","WIWA","WIWR","WOFR","WTDE",
                         "WTSP","WWCR","YBFL","YBSA","YEWA","YHBL","YRWA")])] <- 0

birdspervisit_rearranged.f<-birdspervisit_rearranged[c("SS","Year","latitude","longitude","Project","Gridnum","StationNum",
                                                       "recording_date","recording_time","method","transcriber","rain","wind",
                                                       "industry_noise","noise","GridnumZ","StationnumZ","Site","StationKey",
                                                       "DateTime","lubridated","Month","Day","Hour","Minute","Second",
                                                       "ALFL","AMBI","AMCO","AMCR","AMGO","AMRE","AMRO","AMWI",
                                                       "ATSP","BADO","BAOR","BARS","BAWA","BAWW","BBWA","BCCH",
                                                       "BCFR","BHCO","BHVI","BLBW","BLJA","BLPW","BOCH","BOOW",
                                                       "BOWA","BRBL","BRCR","BRSP","BTGN","BTNW","BWHA","BWWA",
                                                       "CANG","CAWA","CCLO","CCSP","CEDW","CHSP","CMWA","COLO",
                                                       "CONI","CONW","CORA","COYE","CSWA","DEJU","EAKI","EAPH",
                                                       "EUST","FOSP","FRGU","GCKI","GCTH","GHOW","GRAJ","GRCA",
                                                       "GRYE","HASP","HEBA","HEDT","HENO","HERA","HETH","HETR",
                                                       "HEWI","HOFI","HOSP","HOWR","LCSP","LEFL","LEYE","LIAI",
                                                       "LIBA","LIDT","LINO","LIRA","LISP","LITR","LIWI","MAWA",
                                                       "MAWR","MEGU","MGWA","MOAI","MOBA","MODO","MODT","MONO",
                                                       "MORA","MOTR","MOWA","MOWI","MYWA","NAWA","NESP","NOBO",
                                                       "NOFL","NOGO","NOPA","NOWA","NRWS","NSWO","OCWA","OSFL",
                                                       "OVEN","PAWA","PAWR","PBGR","PHVI","PISI","PIWO","PSFL",
                                                       "PUFI","RBGR","RBNU","RCKI","RESQ","REVI","RNGR","RUBL",
                                                       "RUGR","RWBL","SACR","SAPH","SAVS","SCTA","SEWR","SORA",
                                                       "SOSA","SOSP","SPSA","SPTO","SWSP","SWTH","TEWA","TOSO",
                                                       "TRES","UNAM","UNBL","UNCV","UNFI","UNFL","UNKN","UNOW",
                                                       "UNPA","UNSH","UNSP","UNSW","UNTE","UNTH","UNTR","UNVI",
                                                       "UNWA","UNWO","UNYE","VATH","VEER","WAVI","WBBE","WBNU",
                                                       "WCSP","WETA","WEWP","WISN","WIWA","WIWR","WOFR","WTDE",
                                                       "WTSP","WWCR","YBFL","YBSA","YEWA","YHBL","YRWA"
)]
write.csv(birdspervisit_rearranged.f, file="0_data/processed/6b_birdspervisit_readyforGLMs.csv")
#at this point you can use this file to run GLMs or mixed-effects models




