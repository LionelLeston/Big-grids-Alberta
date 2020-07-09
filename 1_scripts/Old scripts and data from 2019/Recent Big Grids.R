#START AT LINE 244: everything before is data prep
biggrids2018<-read.csv("Big_Grids_2018_WILDTRAX_REPORT.csv", header=TRUE)
str(biggrids2018)#9586

biggrids2018$VISIT<-paste0(biggrids2018$data_set,"_",
                           biggrids2018$site,"_",
                           biggrids2018$station,"_",
                         biggrids2018$recording_date,"_",
                         biggrids2018$recording_time,"_",
                         biggrids2018$rain,"_",
                         biggrids2018$wind,"_",
                         biggrids2018$industry_noise,"_",
                         biggrids2018$noise,"_",
                         biggrids2018$latitude,"_",
                         biggrids2018$longitude)

biggrids2018.b<-biggrids2018[!biggrids2018$abundance=="CI 1",]
str(biggrids2018.b)#9581
biggrids2018.c<-biggrids2018.b[!biggrids2018.b$abundance=="CI 3",]
str(biggrids2018.c)#9580

biggrids2018.c$ABUND<-ifelse(biggrids2018.c$abundance=="ONE",1,
                             ifelse(biggrids2018.c$abundance=="TWO",2,0))#TMTC set to 2

tapply.spp<-tapply(biggrids2018.c$ABUND, list(biggrids2018.c$VISIT, biggrids2018.c$species_code), sum, na.rm=TRUE)
tapply.spp<-data.frame(tapply.spp)
tapply.spp$VISIT<-row.names(tapply.spp)
#write.csv(tapply.spp, file = "tapply.spp.csv")  
#data check: create "spp_count.biggrids15_18"
library(tidyr)
bg15_18.wide<-tapply.spp%>%separate(VISIT, c("data_set",
                                             "site",
                                             "station",
                                             "recording_date",
                                             "recording_time",
                                             "rain",
                                             "wind",
                                             "industry_noise",
                                             "noise",
                                             "latitude",
                                             "longitude"),sep="_")
bg15_18.wide$VISIT<-paste0(bg15_18.wide$data_set,
                           bg15_18.wide$site,
                        bg15_18.wide$station,
                        bg15_18.wide$recording_date,
                        bg15_18.wide$recording_time)

library(lubridate)
library(stringr)

bg15_18.wide$DateTime<-paste0(bg15_18.wide$recording_date," ",bg15_18.wide$recording_time)
#bg15_18.wide$DateTime<-str_sub(bg15_18.wide$DateTime, 1, str_length(bg15_18.wide$DateTime)-3)
bg15_18.wide$lubridated<-ymd_hms(bg15_18.wide$DateTime, tz=Sys.timezone())
bg15_18.wide$Year<-year(bg15_18.wide$lubridated)
bg15_18.wide$Month<-month(bg15_18.wide$lubridated)
bg15_18.wide$Day<-day(bg15_18.wide$lubridated)
bg15_18.wide$Hour<-hour(bg15_18.wide$lubridated)
bg15_18.wide$Minute<-minute(bg15_18.wide$lubridated)
bg15_18.wide$Second<-second(bg15_18.wide$lubridated)
write.csv(bg15_18.wide, file="tapply.spp.csv")


