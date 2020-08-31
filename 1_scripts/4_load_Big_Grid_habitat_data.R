load("0_data/raw/bg-data-package.Rdata")
ls()
# "DAT" "DM"  "DMp" "gg"  "OFF" "pv"  "SSH" "vv"  "xy"  "YY"
load("0_data/raw/veghf-summaries.RData")
ls()
#"dd16"  "dd17"  "ddp16" "ddp17" just the items in veghf-summaries

#From Peter: 

# "Once you load them into R, you'll see the following objects:
# 
# - DAT: covariates derived from the 150m veg data to match the AB models
# - OFF: offsets based on survey date/time and veg
# - SSH: surrounding habitat (600x600m) description to match the models terms
# - YY: counts (species as columns)
# 
# - dd16, dd17: list of sparse matrices, you'll be interested in $veg_current (current land cover), either based on 2016 or 2017 human footprint, these give area of land cover in m^2 inside the 600x600m cells (1500 of those). Column definitions are in the attached csv file
# - ddp16, ddp17: same as above but in the 150m radius circle buffers around the centroids of the 600x600m cells (this can be slightly off from the original GPS coordinates, but usually not too much, or in some cases the GPS was wrong for sure...)
# - pv: a of reorganized version of dd17$veg_current
# 
# - xy: coordinates for the 1500 points (Alberta Forestry projection, spatial points object)
# - vv: ID, site, station etc info for the 1500 points that I used
# - gg: nested grouping definitions inside a 10x10 grid (e.g. 2x2, 3x3 etc)

#- DM, DMp: cure4insect based predictions for 600x600m cells or 150m radius points, 
#probably not relevant for you"

#Get Grid Landscape Assignments
str(gg)
gg$StationNum<-gg$station
gg$Station<-gg$station
gg$station<-NULL

#Get Vegetation and Human Footprint, e.g. 2016
str(ddp16$veg_current)#land within 150-m radius circle around each point
ddp16$veg_current@Dimnames
ddp16$veg_current@Dim
ddp16$veg_current@i
ddp16$veg_current@p
ddp16$veg_current@x

str(dd16$veg_current)#land within 600-m square cell around each point

#Convert sparse matrices to data frames: https://slowkow.com/notes/sparse-matrix/
ddp16.dense<-as.matrix(ddp16$veg_current)
ddp16.df<-as.data.frame(ddp16.dense)
str(ddp16.df)
ddp16.df$SS<-row.names(ddp16.df)
ddp16.df.2 <- ddp16.df %>%
  select(SS, everything())
write.csv(ddp16.df.2, file="0_data/processed/9_vegHF150mscale_2016.csv")

ddp16.df.2<-ddp16.df.2%>%separate(SS, c("Project","Gridnum","StationNum"),sep="_")
ddp16.df.2$SS<-paste0(ddp16.df.2$Project,"_",ddp16.df.2$Gridnum,"_",ddp16.df.2$StationNum)
ddp16.df.2 <- ddp16.df.2 %>%
  select(SS, everything())#create StationNum by splitting up SS, recreate SS and put it 
#back at front of data frame
ddp16.df.2.gg<-merge(ddp16.df.2, gg, by=c("StationNum"))
write.csv(ddp16.df.2.gg, file="0_data/processed/10_vegHF150mscale_2016_stationgridassignment.csv")

dd16.dense<-as.matrix(dd16$veg_current)
dd16.df<-as.data.frame(dd16.dense)
str(dd16.df)
dd16.df$SS<-row.names(dd16.df)
dd16.df.2 <- dd16.df %>%
  select(SS, everything())
write.csv(dd16.df.2, file="0_data/processed/9_vegHF36000m2scale_2016.csv")

dd16.df.2<-dd16.df.2%>%separate(SS, c("Project","Gridnum","StationNum"),sep="_")
dd16.df.2$SS<-paste0(dd16.df.2$Project,"_",dd16.df.2$Gridnum,"_",dd16.df.2$StationNum)
dd16.df.2 <- dd16.df.2 %>%
  select(SS, everything())#create StationNum by splitting up SS, recreate SS and put it 
#back at front of data frame
dd16.df.2.gg<-merge(dd16.df.2, gg, by=c("StationNum"))
write.csv(dd16.df.2.gg, file="0_data/processed/10_vegHF36000m2scale_2016_stationgridassignment.csv")


#Get Vegetation and Human Footprint, e.g. 2017
str(ddp17$veg_current)#land within 150-m radius circle around each point
ddp17$veg_current@Dimnames
ddp17$veg_current@Dim
ddp17$veg_current@i
ddp17$veg_current@p
ddp17$veg_current@x

str(dd17$veg_current)#land within 600-m square cell around each point

#Convert sparse matrices to data frames: https://slowkow.com/notes/sparse-matrix/
ddp17.dense<-as.matrix(ddp17$veg_current)
ddp17.df<-as.data.frame(ddp17.dense)
str(ddp17.df)
ddp17.df$SS<-row.names(ddp17.df)
ddp17.df.2 <- ddp17.df %>%
  select(SS, everything())
write.csv(ddp17.df.2, file="0_data/processed/9_vegHF150mscale_2017.csv")

ddp17.df.2<-ddp17.df.2%>%separate(SS, c("Project","Gridnum","StationNum"),sep="_")
ddp17.df.2$SS<-paste0(ddp17.df.2$Project,"_",ddp17.df.2$Gridnum,"_",ddp17.df.2$StationNum)
ddp17.df.2 <- ddp17.df.2 %>%
  select(SS, everything())#create StationNum by splitting up SS, recreate SS and put it 
#back at front of data frame
ddp17.df.2.gg<-merge(ddp17.df.2, gg, by=c("StationNum"))
write.csv(ddp17.df.2.gg, file="0_data/processed/10_vegHF150mscale_2017_stationgridassignment.csv")


dd17.dense<-as.matrix(dd17$veg_current)
dd17.df<-as.data.frame(dd17.dense)
str(dd17.df)
dd17.df$SS<-row.names(dd17.df)
dd17.df.2 <- dd17.df %>%
  select(SS, everything())
write.csv(dd17.df.2, file="0_data/processed/9_vegHF36000m2scale_2017.csv")

dd17.df.2<-dd17.df.2%>%separate(SS, c("Project","Gridnum","StationNum"),sep="_")
dd17.df.2$SS<-paste0(dd17.df.2$Project,"_",dd17.df.2$Gridnum,"_",dd17.df.2$StationNum)
dd17.df.2 <- dd17.df.2 %>%
  select(SS, everything())#create StationNum by splitting up SS, recreate SS and put it 
#back at front of data frame
dd17.df.2.gg<-merge(dd17.df.2, gg, by=c("StationNum"))
write.csv(dd17.df.2.gg, file="0_data/processed/10_vegHF36000m2scale_2017_stationgridassignment.csv")

#You could also use the ABMI model covariates
DAT$SS<-DAT$Key
write.csv(DAT, file="0_data/processed/9_vegHF_ABMIcovar.csv")

DAT.gg<-merge(DAT, gg, by=c("Station"))
write.csv(DAT.gg, file="0_data/processed/10_vegHF_ABMIcovar_stationgridassignment.csv")

#Get QPAD Offsets for GLMs, GAMs, or Mixed-Effects Models
str(OFF)#no station IDs but number of rows=number in DAT and YY files
SS<-DAT$Key
OFF.stationID<-data.frame(SS, OFF)
write.csv(OFF.stationID, file="0_data/processed/11_offsets.csv")

write.csv(YY, file="0_data/processed/11_birdcountdatafromPeter.csv")
#These data are included here because they were part of the package containing offsets,
#vegetation, and human footprint. Depending on how differently Peter processed these data
#this file may or may not be used for our mixed or N-mixture models.
