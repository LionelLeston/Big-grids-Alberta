library(tidyr)
library(lubridate)
library(stringr)

bird.data.GLMs<-read.csv("0_data/processed/6b_birdspervisit_readyforGLMs.csv", header=TRUE)
str(bird.data.GLMs)
#assign round
bird.data.GLMs$round <- ave(bird.data.GLMs$HETH, bird.data.GLMs$SS, FUN = seq_along)
#any bird species column could be used but you want to 
#count rows (rounds) per unique station ID (SS)
bird.data.GLMs<-bird.data.GLMs[bird.data.GLMs$round<5,]

names = c("HETH")
names   ## You could use the following loop if you 
#have abundance of different species in different columns 
#or you could replace "spp" with bird.data.GLMs$HETH in the tapply function

for(i in names){ 
  bird.data.GLMs$spp<-bird.data.GLMs[,c(i)]  #Species as variable
  
  foo<-bird.data.GLMs[,c("spp","SS","round")]%>%
    spread(key=round, value=spp)
  write.csv(foo, paste0("0_data/processed/mixture model input files/",i,".csv"))
}

#Create detection covariates
bird.data.GLMs$Julian<-yday(as.Date(bird.data.GLMs$lubridated))
bird.data.GLMs$Julian.s<-scale(bird.data.GLMs$Julian)
bird.data.GLMs$Julian2.s<-scale(bird.data.GLMs$Julian^2)
bird.data.GLMs$Time<-bird.data.GLMs$Hour+(bird.data.GLMs$Minute/60)
bird.data.GLMs$Time.s<-scale(bird.data.GLMs$Time)
bird.data.GLMs$Time2.s<-scale(bird.data.GLMs$Time^2)

names = c("Julian","Julian.s","Julian2.s","Time","Time.s","Time2.s")

for(i in names){ 
  bird.data.GLMs$spp<-bird.data.GLMs[,c(i)]  #Species as variable
  
  foo<-bird.data.GLMs[,c("spp","SS","round")]%>%
    spread(key=round, value=spp)
  write.csv(foo, paste0("0_data/processed/mixture model input files/",i,".csv"))
}
