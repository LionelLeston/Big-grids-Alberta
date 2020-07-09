#This script filters the Big Grid data to those points
#useful in analyses, by removing observations that may 
#have inordinate influence on the methods
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

bg.all<-read.csv("0_data/processed/1_Big_Grids_AllAsOfJune30_2020.csv", header=TRUE)
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

#However, nearly all stations on Big Grids 2,3,and 4 only had a 3+7 minute recording transcribed.
bg.3min<-bg.all[bg.all$method=="3m 1SPM",]
#Alternatively, keep the 3+7 minute recordings as well
bg.3min<-bg.all[!bg.all$method=="1m 1SPM",]
bg.3min<-bg.3min[!bg.3min$method=="10m 1SPM",]

write.csv(bg.3min, file="0_data/processed/2_Big_Grids_3min_AsOfJune30_2020.csv")

#Just singing birds for N-mixture models
bg.3min.song<-bg.3min[bg.3min$sang==1,]
write.csv(bg.3min.song, file="0_data/processed/3_Big_Grids_SongOnly_3min_AsOfJune30_2020.csv")

#When the singing birds from 3-minute recordings are examined
#there are a couple of discrepancies I see:
#one REVI (Red-eyed Vireo) observation described as "birdabundance=N/A"
#one HEWI (heavy wind) observation described as "Song" in the first 3 minutes of vocalizations
#not sure if HEWI was meant to be "HETH" or something else

#After correcting or discarding these 2 observations:
#other "song" observations where birdabundance=N/A are for 
#amphibians, whose abundance was measured using "CI 1, CI 2, or CI 3"

#Some but not all of the amphibian observations can be filtered out by 
#dropping the birdabund=NA observations

bg.3min.song.birds<-bg.3min.song[!is.na(bg.3min.song$birdabund),]
write.csv(bg.3min.song.birds, file="0_data/processed/4_Big_Grids_BirdSongOnly_3min_AsOfJune30_2020.csv")
#There still are 19 Wood Frog and Boreal Chorus Frog observations
#There are also 1140 Light (LI+), Moderate (MO+), or Heavy (HE+)
#Noise (NO), Rain (RA), or Wind (WI) observations described as "songs"
#in the filtered data.
#While these species_codes could simply be filtered out, it could
#be worth making sure that these codes weren't mistakenly entered 
#in place of the name of a singing bird species.
#There were 11 different transcribers that entered these data.