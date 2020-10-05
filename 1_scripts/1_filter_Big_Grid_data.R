#This script filters the Big Grid data to those points
#useful in analyses, by removing observations that may 
#have inordinate influence on the methods
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

bg.all<-read.csv("0_data/processed/1_Big_Grids_AllAsOfOct5_2020.csv", header=TRUE)
#previously read in as "1_Big_Grids_AllAsOfJune30_2020.csv" and "1_Big_Grids_AllAsOfSep14_2020.csv"
levels(as.factor(bg.all$method))
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

#bg.3min<-bg.all[bg.all$method=="3m 1SPM",]
#However, nearly all stations on Big Grids 2,3,and 4 only had a 3+7 minute recording transcribed.

#Alternatively, keep the 3+7 and 10 minute recordings as well
bg.3min<-bg.all[!bg.all$method=="1m 1SPM",]
#bg.3min<-bg.3min[!bg.3min$method=="10m 1SPM",]

write.csv(bg.3min, file="0_data/processed/2_Big_Grids_3min_AsOfOct5_2020.csv")

#We just want singing birds for N-mixture models but we also want
#to get recordings in which just calling birds are detected
bg.3min<-read.csv("0_data/processed/2_Big_Grids_3min_AsOfOct5_2020.csv", header=TRUE)
bg.3min$birdabund.songonly<-bg.3min$birdabund
bg.3min$birdabund.songonly[bg.3min$sang==0]<-0
bg.3min$birdabund.songonly[is.na(bg.3min$birdabund)]<-NA
#bg.3min.song<-bg.3min[bg.3min$sang==1,]

#We want to remove individuals who are only first detected after the first 3 minutes
#BUT
#We want to keep observations of "NONE" in which there are no detections in the first 3 minutes of a visit
#as of October 5, 2020 there are 40 individual birds that were heard first after 3 minutes
#So results after filtering should be 40 less than the file with all obs (bg.3min) 
nrow(bg.3min)#78348
bg.3min.extrabirds.rem<-bg.3min[!is.na(bg.3min$min0_start)|!is.na(bg.3min$min1_start)|!is.na(bg.3min$min2_start)|bg.3min$species_code=="NONE",]
nrow(bg.3min.extrabirds.rem)#78308...forty less birds
write.csv(bg.3min.extrabirds.rem, file="0_data/processed/3_Big_Grids_ExtrasRemoved_3min_AsOfOct5_2020.csv")

#When the singing birds from 3-minute recordings are examined
#there are some couple of discrepancies I see:
#bird observations described as "birdabundance=N/A"
#HEWI (heavy wind) observations described as "Song" in the first 3 minutes of vocalizations
#not sure if HEWI was meant to be "HETH" or something else

#While these species_codes could simply be filtered out, it could
#be worth making sure that these codes weren't mistakenly entered 
#in place of the name of a singing bird species.
#There were 11 different transcribers that entered these data.

#Other "song" observations where birdabundance=N/A are for 
#amphibians, whose abundance was measured using "CI 1, CI 2, or CI 3"
bg.3min.song<-read.csv("0_data/processed/3_Big_Grids_ExtrasRemoved_3min_AsOfOct5_2020.csv", header=TRUE)
#Some but not all of the amphibian observations can be filtered out by 
#dropping the birdabund=NA observations. Dropping these observations
#might also remove recordings where no birds happened to be detected
bg.3min.song.birds<-bg.3min.song[!is.na(bg.3min.song$birdabund),]
nrow(bg.3min.song.birds)#69881
nrow(bg.3min.song.birds[bg.3min.song.birds$species_code=="WOFR",])#7
nrow(bg.3min.song.birds[bg.3min.song.birds$species_code=="BCFR",])#32
nrow(bg.3min.song.birds[bg.3min.song.birds$species_code=="NONE",])#0
#There still are amphibian observations as well as #Light (LI+), Moderate (MO+), 
#or Heavy (HE+) Noise (NO), Rain (RA), or Wind (WI) observations described as "songs"
#in the filtered data.
bg.3min.birdsonly<-bg.3min.song[!bg.3min.song$abundance=="CI 1",]
bg.3min.birdsonly<-bg.3min.birdsonly[!bg.3min.birdsonly$abundance=="CI 2",]
bg.3min.birdsonly<-bg.3min.birdsonly[!bg.3min.birdsonly$abundance=="CI 3",]
nrow(bg.3min.birdsonly)#77961

#At least remove amphibian observations where abundance = CI 1, CI 2, or CI 3
write.csv(bg.3min.birdsonly, file="0_data/processed/4_Big_Grids_BirdsOnly_3min_AsOfOct5_2020.csv")


