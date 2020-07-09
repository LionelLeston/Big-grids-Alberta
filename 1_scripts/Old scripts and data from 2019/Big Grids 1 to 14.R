#START AT LINE 244: everything before is data prep
biggrids14<-read.csv("BigGridRecordingsFEB27.csv", header=TRUE)
str(biggrids14)#55662

#There is no abundance column but there is an individual_ID column
#with nonzero values for individual birds and zero values for weather 
#variables
biggrids14$abund<-1

biggrids14$VISIT<-paste0(biggrids14$StationKey,"_",
                         biggrids14$Year,"_",
                         biggrids14$Round,"_",
                         biggrids14$RECORDING_DATE,"_",
                         biggrids14$RECORD_TIME,"_",
                         biggrids14$Replicate,"_",
                         biggrids14$Observer,"_",
                         biggrids14$Rain,"_",
                         biggrids14$Wind,"_",
                         biggrids14$Industry,"_",
                         biggrids14$Noise,"_",
                         biggrids14$Latitude,"_",
                         biggrids14$Longitude,"_",
                         biggrids14$EASTING,"_",
                         biggrids14$NORTHING)

tapply.spp<-tapply(biggrids14$abund, list(biggrids14$VISIT, biggrids14$SPECIES), sum, na.rm=TRUE)
tapply.spp<-data.frame(tapply.spp)
tapply.spp$VISIT<-row.names(tapply.spp)
write.csv(tapply.spp, file = "tapply.spp.biggrids14.csv")  


library(tidyr)
biggrids14<-read.csv("spp_count.biggrids14.csv",header=TRUE)
bg14.wide<-biggrids14%>%separate(VISIT, c("StationKey",
                                          "Year",
                                          "Round",
                                          "RECORDING_DATE",
                                          "RECORD_TIME",
                                          "Replicate",
                                          "Observer",
                                          "Rain",
                                          "Wind",
                                          "Industry",
                                          "Noise",
                                          "Latitude",
                                          "Longitude",
                                          "EASTING",
                                          "NORTHING"),sep="_")
bg14.wide$VISIT<-paste0(bg14.wide$StationKey,"_",
                        bg14.wide$Round,"_",
                        bg14.wide$RECORDING_DATE,"_",
                        bg14.wide$RECORD_TIME)

library(lubridate)
library(stringr)
bg14.wide$Site<-str_sub(bg14.wide$StationKey, 1, 7)

bg14.wide$DateTime<-paste0(bg14.wide$RECORDING_DATE," ",bg14.wide$RECORD_TIME)
bg14.wide$DateTime<-str_sub(bg14.wide$DateTime, 1, str_length(bg14.wide$DateTime)-3)
bg14.wide$lubridated<-dmy_hms(bg14.wide$DateTime, tz=Sys.timezone())
bg14.wide$Year<-year(bg14.wide$lubridated)
bg14.wide$Month<-month(bg14.wide$lubridated)
bg14.wide$Day<-day(bg14.wide$lubridated)
bg14.wide$Hour<-hour(bg14.wide$lubridated)
bg14.wide$Minute<-minute(bg14.wide$lubridated)
bg14.wide$Second<-second(bg14.wide$lubridated)
write.csv(bg14.wide, file="bg14.wide.csv")
#data check: remove recordings from inappropriate times and dates
#406 recordings earlier than 4:00 AM
#238 recordings earlier than May 26
#147 recordings later than July 7
#4740 recordings left from the 14 Big Grids

#randomly sample 1 replicate
library(dplyr)
bg14.wide.oddDTout<-read.csv("bg14.wide.oddDTout.csv", header=TRUE)
bg14.1repsamp <- bg14.wide.oddDTout %>% group_by(VISIT) %>% sample_n(1)
write.csv(bg14.1repsamp, file="bg14.1repsamp.csv")#data check

