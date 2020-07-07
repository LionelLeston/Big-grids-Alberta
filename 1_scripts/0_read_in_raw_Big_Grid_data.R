#This script reads in and processes the raw Big Grid data available on 
#WildTrax as of June 30, 2020
library(tidyr)
library(dplyr)
bg.2014<-read.csv("0_data/raw/Big_Grids_2014_WILDTRAX_REPORT.csv",header=TRUE)
bg.2015<-read.csv("0_data/raw/Big_Grids_2015_WILDTRAX_REPORT.csv",header=TRUE)
bg.2016<-read.csv("0_data/raw/Big_Grids_2016_WILDTRAX_REPORT.csv",header=TRUE)
bg.2017<-read.csv("0_data/raw/Big_Grids_2017_WILDTRAX_REPORT.csv",header=TRUE)
bg.2018<-read.csv("0_data/raw/Big_Grids_2018_WILDTRAX_REPORT.csv",header=TRUE)
bg.2019<-read.csv("0_data/raw/Big_Grids_2019_WILDTRAX_REPORT.csv",header=TRUE)

bg.all<-bind_rows(bg.2014, bg.2015, bg.2016, bg.2017, bg.2018, bg.2019)
str(bg.all)
levels(as.factor(bg.all$location))#1780 entries
#Big Grid 1: 97
#Big Grid 10: 94
#Big Grid 11: 100
#Big Grid 12: 100
#Big Grid 13: 99
#etc. 
write.csv(bg.all, file="0_data/processed/Big_Grids_AllAsOfJune30_2020.csv")
#location or source file name can be used to get the grid number and station 
#number. For most grids except number 1, numbers increase from west to east
#and from south to north, so station number 1 will be in southwest corner and
#station 100 will be in northeast corner. There are date, time, weater, observer,
#and method descriptor variables in the raw data that will affect the number of
#birds counted in a given recording. Most recordings are for 3 minutes but 
#there are 10-minute recordings and a few 1-minute recordings. Most points in
#a grid are ~600 m apart but they are 1 km apart in grid 1.

#What happened is that some visits were listened to 2 or more times using different
#survey protocols (e.g. visit length). All visits were listened to for at least 3 minutes.
#But some visits were listened to again for just the first minute or additional birds
#were listened for over 7 additional visits. This was probably done to see if certain species
#were missed or if a 1-minute recording could be used instead of 3 minutes and gain the
#same number of individuals or species, or if a 10-minute recording resulted in significantly
#more individuals and species being detected. If the unusual survey protocols are filtered
#out (i.e. 1-minute, 3+7 minutes, 10 minutes) then each station and each visit will
#still have a 3-minute recording.

#Worth noting for habitat:
#I currently have habitat+human footprint AVI for 13 Big Grids (1 to 14 except for 8)
#and habitat but not human footprint AVI for 1 more Big Grid (15)

#There are four more grids we could potentially use in that data are described
#but we would need AVI (alternatively I could use the 2011 Beaudoin layer)

#116 stations from Big Grids 16 and 17 (~48 from BG16, the rest from BG17)
#91 from Big Grid 18 and 29 from Big Grid 19
#The lack of full grids is probably not an issue except for multi-scale papers
#if I use point counts as replicates

#Big Grids 1 to 15 and possibly 18 could be used in a multi-scale paper







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

