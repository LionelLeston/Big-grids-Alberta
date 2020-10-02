#This script reads in and processes the raw Big Grid data available on 
#WildTrax as of June 30, 2020
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

bg.2014<-read.csv("0_data/raw/Big_Grids_2014_WILDTRAX_REPORT.csv",header=TRUE)
bg.2015<-read.csv("0_data/raw/Big_Grids_2015_WILDTRAX_REPORT.csv",header=TRUE)
bg.2016<-read.csv("0_data/raw/Big_Grids_2016_WILDTRAX_REPORT.csv",header=TRUE)
bg.2017<-read.csv("0_data/raw/Big_Grids_2017_WILDTRAX_REPORT.csv",header=TRUE)
bg.2018<-read.csv("0_data/raw/Big_Grids_2018_WILDTRAX_REPORT.csv",header=TRUE)
bg.2019<-read.csv("0_data/raw/Big_Grids_2019_WILDTRAX_REPORT.csv",header=TRUE)

bg.all<-bind_rows(bg.2014, bg.2015, bg.2016, bg.2017, bg.2018, bg.2019)
str(bg.all)

#Alternatively to reading in the individual Big Grid files from different
#years, I could read in "APPENDED_WILDTRAX_REPORT", a combined file that is
#created by Wildtrax at the same time as all files of a particular source
#and format are downloaded.

bg.all<-read.csv("0_data/raw/APPENDED_WILDTRAX_REPORT.csv", header=TRUE)

#location can be used to get the grid number and station 
#number. For most grids except number 1, numbers increase from west to east
#and from south to north, so station number 1 will be in southwest corner and
#station 100 will be in northeast corner. 

#Do not use source file name because some source file names are only
#listed as numbers, and a given number value can correspond to same
#station number in different grids
bg.all<-bg.all%>%separate(location, c("Project","Gridnum","StationNum"),sep="-")
#split "location" into 3 new variables
bg.all$location<-paste0(bg.all$Project,"-",bg.all$Gridnum,"-",bg.all$StationNum)
#recreate bg.all$location once you have your new variables
bg.all$SS<-paste0(bg.all$Project,"_",bg.all$Gridnum,"_",bg.all$StationNum)
#bg.all$SS is the same format as the station identifiers in Peter's vegetation data, so
#it will be used to link bird data to vegetation and human footprint

bg.all$GridnumZ<-ifelse(as.numeric(bg.all$Gridnum)<10, paste0("000",bg.all$Gridnum), paste0("00",bg.all$Gridnum))
bg.all$StationnumZ<-ifelse(as.numeric(bg.all$StationNum)<10, paste0("00",bg.all$StationNum), 
                           ifelse(as.numeric(bg.all$StationNum)<100, paste0("0",bg.all$StationNum), bg.all$StationNum))
bg.all$Site<-paste0(bg.all$Project,"-",bg.all$GridnumZ)
bg.all$StationKey<-paste0(bg.all$Project,"-",bg.all$GridnumZ,"-",bg.all$StationnumZ)
#an alternate station name format that is sometimes used

bg.all$DateTime<-paste0(bg.all$recording_date," ",bg.all$recording_time)
#bg.all$DateTime<-str_sub(bg.all$DateTime, 1, str_length(bg.all$DateTime)-3)
bg.all$lubridated<-ymd_hms(bg.all$DateTime, tz=Sys.timezone())
bg.all$Year<-year(bg.all$lubridated)
bg.all$Month<-month(bg.all$lubridated)
bg.all$Day<-day(bg.all$lubridated)
bg.all$Hour<-hour(bg.all$lubridated)
bg.all$Minute<-minute(bg.all$lubridated)
bg.all$Second<-second(bg.all$lubridated)

#there is an abundance column: ONE=1, TMTC=Too many to count
#CI 1, CI 2, and CI 3 are codes used for amphibians and can be
#filtered out at next stage
#TMTC is used for some bird observations, e.g. lots of contact calls
#or colonial species; TMTC could be conservatively set to 2
#N/A is used for weather observations, e.g. presence of rain or industry noise
bg.all$birdabund<-ifelse(bg.all$abundance=="ONE",1,
                         ifelse(bg.all$abundance=="TMTC",2,NA))

#Before creating variable "VISIT" some transcriber names
#must be edited: the presence of @ and _ will cause problems
#when creating or later parsing "VISIT"

bg.all$transcriber<-as.factor(bg.all$transcriber)
levels(bg.all$transcriber)
levels(bg.all$transcriber)[levels(bg.all$transcriber)=="jf_jet@hotmail.com"] <- "JF Jet"
levels(bg.all$transcriber)[levels(bg.all$transcriber)=="s.shappas@hotmail.com"] <- "S Shappas"
levels(bg.all$transcriber)[levels(bg.all$transcriber)=="bprobinson@live.ca"] <- "BP Robinson"
levels(bg.all$transcriber)[levels(bg.all$transcriber)=="demkoad@gmail.com"] <- "D Emkoad"
levels(bg.all$transcriber)#data check

bg.all$VISIT<-paste0(bg.all$Year,"_",
                     bg.all$Project,"_",#SS recreated from VISIT later
                     bg.all$Gridnum,"_",#SS recreated from VISIT later
                     bg.all$StationNum,"_",#SS recreated from VISIT later
                     bg.all$recording_date,"_",
                     bg.all$recording_time,"_",
                     bg.all$method,"_",
                     bg.all$transcriber,"_",
                     bg.all$rain,"_",
                     bg.all$wind,"_",
                     bg.all$industry_noise,"_",
                     bg.all$noise,"_",
                     bg.all$latitude,"_",
                     bg.all$longitude)

#We may need other columns for filtering the data. 
#For N-mixture models we would limit observations to singing
#rather than calling observations. We'd note if a given individual
#bird vocalized by singing rather than calling in one of the 
#(first) three minutes of the recording. If we also include the 3 minute+
#7-minute transcribed recordings, this filtering will remove any individual
#birds detected only after the first 3 minutes.
bg.all$sang<-ifelse(bg.all$min0_voc=="Song",1,
                    ifelse(bg.all$min1_voc=="Song",1,
                           ifelse(bg.all$min2_voc=="Song",1,0)))

#Seems to work. Alternatively:
#bg.all$sang <- apply(bg.all[,c("min0_voc","min1_voc","min2_voc")] == "Song",1,any)

write.csv(bg.all, file="0_data/processed/1_Big_Grids_AllAsOfSep14_2020.csv")
#formerly saved as "1_Big_Grids_AllAsOfJune30_2020.csv"

#There are date, time, weater, observer,
#and method descriptor variables in the raw data that will affect the number of
#birds counted in a given recording. Most recordings are for 3 minutes but 
#there are 10-minute recordings and a few 1-minute recordings. Most points in
#a grid are ~600 m apart but they are 1 km apart in grid 1.

#What happened is that some visits were listened to 2 or more times using different
#survey protocols (e.g. visit length). All visits were listened to for at least 3 minutes.
#But some visits were listened to again for just the first minute or additional birds
#were listened for over 7 additional minutes. This was probably done to see if certain species
#were missed or if a 1-minute recording could be used instead of 3 minutes and gain the
#same number of individuals or species, or if a 10-minute recording resulted in significantly
#more individuals and species being detected. If the 1-minute and 10-minute survey protocols are filtered
#out then nearly all stations and visits will still have a 3-minute recording.

#However, nearly all stations on Big Grids 2,3,and 4 only had a 3+7 minute recording transcribed.

#From Alex: to view number of visits per station
BG <- bg.all %>% select(location, Year, method, recording_date, transcriber) %>% distinct() %>% group_by(location, Year, method, recording_date, transcriber) %>% count() %>% arrange(n)
write.csv(BG, file="0_data/processed/7c_numberofvisitsperstationkey_Alex.csv")

