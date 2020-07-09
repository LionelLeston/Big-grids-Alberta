library(lme4)
birds.habitatHF<-read.csv("birds.habitatHF.csv", header=TRUE)
birds.habitatHF$Total.Linear<-birds.habitatHF$SeismicLine+
  birds.habitatHF$Pipeline+birds.habitatHF$RailHardSurface+
  birds.habitatHF$RailVegetatedVerge+birds.habitatHF$RoadHardSurface+
  birds.habitatHF$RoadTrailVegetated+birds.habitatHF$RoadVegetatedVerge+
  birds.habitatHF$TransmissionLine

birds.habitatHF$Soft.Linear<-birds.habitatHF$SeismicLine+
  birds.habitatHF$Pipeline+
  birds.habitatHF$RailVegetatedVerge+
  birds.habitatHF$RoadTrailVegetated+birds.habitatHF$RoadVegetatedVerge+
  birds.habitatHF$TransmissionLine

birds.habitatHF$Hard.Linear<-birds.habitatHF$RailHardSurface+
  birds.habitatHF$RoadHardSurface

birds.habitatHF$Total.Harvest<-birds.habitatHF$CCMixwood0+
  birds.habitatHF$CCMixwood1+
  birds.habitatHF$CCMixwood2+
  birds.habitatHF$CCMixwood3+
  birds.habitatHF$CCMixwood4+
  birds.habitatHF$CCMixwoodR+
  birds.habitatHF$CCPine0+
  birds.habitatHF$CCPine1+
  birds.habitatHF$CCPine2+
  birds.habitatHF$CCPine3+
  birds.habitatHF$CCPine4+
  birds.habitatHF$CCPineR+
  birds.habitatHF$CCConif0+
  birds.habitatHF$CCConif1+
  birds.habitatHF$CCConif2+
  birds.habitatHF$CCConif3+
  birds.habitatHF$CCConif4+
  birds.habitatHF$CCConifR+
  birds.habitatHF$CCDecid0+
  birds.habitatHF$CCDecid1+
  birds.habitatHF$CCDecid2+
  birds.habitatHF$CCDecid3+
  birds.habitatHF$CCDecid4+
  birds.habitatHF$CCDecidR

birds.habitatHF$Nonlinear<-birds.habitatHF$Total.Harvest+
  birds.habitatHF$WellSite+birds.habitatHF$MineSite+
  birds.habitatHF$IndustrialSiteRural+
  birds.habitatHF$BorrowpitsDugoutsSumps+
  birds.habitatHF$RuralResidentialIndustrial+
  birds.habitatHF$Urban+
  birds.habitatHF$PeatMine+
  birds.habitatHF$CultivationCropPastureBareground+
  birds.habitatHF$HighDensityLivestockOperation
  
birds.habitatHF$Total.Forwet<-birds.habitatHF$Wetland.Decid1+
  birds.habitatHF$Wetland.Decid2+
  birds.habitatHF$Wetland.Decid3+
  birds.habitatHF$Wetland.Decid4+
  birds.habitatHF$Wetland.DecidR+
  birds.habitatHF$Wetland.Decid0+
  birds.habitatHF$Wetland.Decid5+
  birds.habitatHF$Wetland.Decid6+
  birds.habitatHF$Wetland.Decid7+
  birds.habitatHF$Wetland.Decid8+
  birds.habitatHF$Wetland.Decid9+
  birds.habitatHF$Wetland.Larch1+
  birds.habitatHF$Wetland.Larch2+
  birds.habitatHF$Wetland.Larch3+
  birds.habitatHF$Wetland.Larch4+
  birds.habitatHF$Wetland.LarchR+
  birds.habitatHF$Wetland.Larch0+
  birds.habitatHF$Wetland.Larch5+
  birds.habitatHF$Wetland.Larch6+
  birds.habitatHF$Wetland.Larch7+
  birds.habitatHF$Wetland.Larch8+
  birds.habitatHF$Wetland.Larch9+
  birds.habitatHF$Wetland.BSpr1+
  birds.habitatHF$Wetland.BSpr2+
  birds.habitatHF$Wetland.BSpr3+
  birds.habitatHF$Wetland.BSpr4+
  birds.habitatHF$Wetland.BSprR+
  birds.habitatHF$Wetland.BSpr0+
  birds.habitatHF$Wetland.BSpr5+
  birds.habitatHF$Wetland.BSpr6+
  birds.habitatHF$Wetland.BSpr7+
  birds.habitatHF$Wetland.BSpr8+
  birds.habitatHF$Wetland.BSpr9+
  birds.habitatHF$Swamp.Decid1+
  birds.habitatHF$Swamp.Decid2+
  birds.habitatHF$Swamp.Decid3+
  birds.habitatHF$Swamp.Decid4+
  birds.habitatHF$Swamp.DecidR+
  birds.habitatHF$Swamp.Decid0+
  birds.habitatHF$Swamp.Decid5+
  birds.habitatHF$Swamp.Decid6+
  birds.habitatHF$Swamp.Decid7+
  birds.habitatHF$Swamp.Decid8+
  birds.habitatHF$Swamp.Decid9+
  birds.habitatHF$Swamp.Conif1+
  birds.habitatHF$Swamp.Conif2+
  birds.habitatHF$Swamp.Conif3+
  birds.habitatHF$Swamp.Conif4+
  birds.habitatHF$Swamp.ConifR+
  birds.habitatHF$Swamp.Conif0+
  birds.habitatHF$Swamp.Conif5+
  birds.habitatHF$Swamp.Conif6+
  birds.habitatHF$Swamp.Conif7+
  birds.habitatHF$Swamp.Conif8+
  birds.habitatHF$Swamp.Conif9+
  birds.habitatHF$Swamp.Mixwood1+
  birds.habitatHF$Swamp.Mixwood2+
  birds.habitatHF$Swamp.Mixwood3+
  birds.habitatHF$Swamp.Mixwood4+
  birds.habitatHF$Swamp.MixwoodR+
  birds.habitatHF$Swamp.Mixwood0+
  birds.habitatHF$Swamp.Mixwood5+
  birds.habitatHF$Swamp.Mixwood6+
  birds.habitatHF$Swamp.Mixwood7+
  birds.habitatHF$Swamp.Mixwood8+
  birds.habitatHF$Swamp.Mixwood9+
  birds.habitatHF$Swamp.Pine1+
  birds.habitatHF$Swamp.Pine2+
  birds.habitatHF$Swamp.Pine3+
  birds.habitatHF$Swamp.Pine4+
  birds.habitatHF$Swamp.PineR+
  birds.habitatHF$Swamp.Pine0+
  birds.habitatHF$Swamp.Pine5+
  birds.habitatHF$Swamp.Pine6+
  birds.habitatHF$Swamp.Pine7+
  birds.habitatHF$Swamp.Pine8+
  birds.habitatHF$Swamp.Pine9

  
birds.habitatHF$Total.Decid<-birds.habitatHF$CCDecid0+
  birds.habitatHF$CCDecid1+
  birds.habitatHF$CCDecid2+
  birds.habitatHF$CCDecid3+
  birds.habitatHF$CCDecid4+
  birds.habitatHF$CCDecidR+
  birds.habitatHF$Decid1+
  birds.habitatHF$Decid2+
  birds.habitatHF$Decid3+
  birds.habitatHF$Decid4+
  birds.habitatHF$DecidR+
  birds.habitatHF$Decid0+
  birds.habitatHF$Decid5+
  birds.habitatHF$Decid6+
  birds.habitatHF$Decid7+
  birds.habitatHF$Decid8+
  birds.habitatHF$Decid9

birds.habitatHF$Total.Conif<-birds.habitatHF$CCConif0+
  birds.habitatHF$CCConif1+
  birds.habitatHF$CCConif2+
  birds.habitatHF$CCConif3+
  birds.habitatHF$CCConif4+
  birds.habitatHF$CCConifR+
  birds.habitatHF$Conif1+
  birds.habitatHF$Conif2+
  birds.habitatHF$Conif3+
  birds.habitatHF$Conif4+
  birds.habitatHF$ConifR+
  birds.habitatHF$Conif0+
  birds.habitatHF$Conif5+
  birds.habitatHF$Conif6+
  birds.habitatHF$Conif7+
  birds.habitatHF$Conif8+
  birds.habitatHF$Conif9

birds.habitatHF$Total.Mixwood<-birds.habitatHF$CCMixwood0+
  birds.habitatHF$CCMixwood1+
  birds.habitatHF$CCMixwood2+
  birds.habitatHF$CCMixwood3+
  birds.habitatHF$CCMixwood4+
  birds.habitatHF$CCMixwoodR+
  birds.habitatHF$Mixwood1+
  birds.habitatHF$Mixwood2+
  birds.habitatHF$Mixwood3+
  birds.habitatHF$Mixwood4+
  birds.habitatHF$MixwoodR+
  birds.habitatHF$Mixwood0+
  birds.habitatHF$Mixwood5+
  birds.habitatHF$Mixwood6+
  birds.habitatHF$Mixwood7+
  birds.habitatHF$Mixwood8+
  birds.habitatHF$Mixwood9

birds.habitatHF$Total.Pine<-birds.habitatHF$CCPine0+
  birds.habitatHF$CCPine1+
  birds.habitatHF$CCPine2+
  birds.habitatHF$CCPine3+
  birds.habitatHF$CCPine4+
  birds.habitatHF$CCPineR+
  birds.habitatHF$Pine1+
  birds.habitatHF$Pine2+
  birds.habitatHF$Pine3+
  birds.habitatHF$Pine4+
  birds.habitatHF$PineR+
  birds.habitatHF$Pine0+
  birds.habitatHF$Pine5+
  birds.habitatHF$Pine6+
  birds.habitatHF$Pine7+
  birds.habitatHF$Pine8+
  birds.habitatHF$Pine9

birds.habitatHF$Soft<-birds.habitatHF$SeismicLine+
  birds.habitatHF$Pipeline

birds.habitatHF$Hard<-birds.habitatHF$WellSite+
  birds.habitatHF$RailHardSurface+
  birds.habitatHF$RailVegetatedVerge+
  birds.habitatHF$RoadHardSurface+
  birds.habitatHF$RoadTrailVegetated+
  birds.habitatHF$RoadVegetatedVerge+
  birds.habitatHF$MineSite+
  birds.habitatHF$BorrowpitsDugoutsSumps+
  birds.habitatHF$PeatMine

#Data Exploration
plot(birds.habitatHF$Total.Harvest, birds.habitatHF$CAWA_mean)
plot(birds.habitatHF$Total.Harvest, birds.habitatHF$CAWA_max)
library(lme4)
cor(birds.habitatHF[,c("Total.Conif",
                       "Total.Decid",
                       "Total.Mixwood",
                       "Total.Harvest",
                       "Soft","Hard")])

#Human Footprint Model
birds.habitatHF$NEAR_DIST_SOFT<-ifelse(birds.habitatHF$NEAR_DIST_SEISMIC>birds.habitatHF$NEAR_DIST_PIPELINE,
                                       birds.habitatHF$NEAR_DIST_SEISMIC, birds.habitatHF$NEAR_DIST_PIPELINE)
birds.habitatHF$NEAR_DIST_HARD<-ifelse(birds.habitatHF$NEAR_DIST_ANYROAD>birds.habitatHF$NEAR_DIST_WELL,
                                       birds.habitatHF$NEAR_DIST_ANYROAD, birds.habitatHF$NEAR_DIST_WELL)
birds.habitatHF$NEAR_DIST_SOFT.s<-scale(birds.habitatHF$NEAR_DIST_SOFT, scale=T, center=T)
birds.habitatHF$NEAR_DIST_HARD.s<-scale(birds.habitatHF$NEAR_DIST_HARD, scale=T, center=T)

cor(birds.habitatHF[,c("Total.Conif",
                       "Total.Decid",
                       "Total.Mixwood",
                       "Total.Harvest",
                       "Soft","Hard","NEAR_DIST_SOFT.s","NEAR_DIST_HARD.s")])
#cor (NEARDISTSOFT vs NEARDISTHARD) slightly more than 0.7
spp.list.max<-c("BRCR_max",
                "BTNW_max",
                "CAWA_max",
                "OVEN_max",
                "SWTH_max",
                "WTSP_max")

h2.max<-list()
for (i in spp.list.max) {
  birds.habitatHF$spp<-birds.habitatHF[,i]
  try(h2.max[[i]]<-glmer(spp~Total.Decid+Soft+Hard+(1|Grid), family="poisson", data=birds.habitatHF))
}


spp.list.bin<-c("BRCR_bin",
                "BTNW_bin",
                "CAWA_bin",
                "OVEN_bin",
                "SWTH_bin",
                "WTSP_bin")

h2.bin<-list()
for (i in spp.list.bin) {
  birds.habitatHF$spp<-birds.habitatHF[,i]
  try(h2.bin[[i]]<-glmer(spp~Total.Decid+Soft+Hard+(1|Grid), family="binomial", data=birds.habitatHF))
}

summary(h2.bin$BRCR_bin)
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -2.6445     0.2423 -10.913  < 2e-16 ***
#Total.Decid   1.6866     0.3301   5.110 3.22e-07 ***
#Soft         -1.1050     3.0141  -0.367 0.713912    
#Hard         -7.4885     2.2459  -3.334 0.000855 ***

summary(h2.max$BRCR_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -2.6559     0.2096 -12.673  < 2e-16 ***
#Total.Decid   1.5020     0.2807   5.351 8.72e-08 ***
#Soft         -0.9354     2.7194  -0.344  0.73087    
#Hard         -6.1895     1.9796  -3.127  0.00177 ** 

###Brown Creeper showed similar significant + response to 
#TotalDecid and - responses to soft and hard footprint(local)
#whether it was based on binary or Poisson counts

summary(h2.bin$BTNW_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.6896     0.8132  -6.996 2.63e-12 ***
#Total.Decid   1.7802     0.6615   2.691  0.00712 ** 
#Soft          4.6127     4.3041   1.072  0.28387    
#Hard         -5.5911     3.8700  -1.445  0.14853    
summary(h2.max$BTNW_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.6962     0.7943  -7.172 7.41e-13 ***
#Total.Decid   1.7492     0.6073   2.880  0.00397 ** 
#Soft          4.2975     3.7427   1.148  0.25087    
#Hard         -4.4908     3.3813  -1.328  0.18415  

###Black-throated Green Warblers show similar + 
#reponses to habitat, and n.s. to soft and hard energy footprint
#whether modelled as binary or Poisson

summary(h2.bin$CAWA_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -5.0171     0.5093  -9.851  < 2e-16 ***
#Total.Decid   2.0948     0.6197   3.380 0.000724 ***
#Soft          4.9462     4.3905   1.127 0.259918    
#Hard          0.7638     2.0393   0.375 0.707989    
summary(h2.max$CAWA_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.9633     0.4872 -10.187  < 2e-16 ***
#Total.Decid   2.1502     0.5656   3.802 0.000144 ***
#Soft          2.9824     4.2407   0.703 0.481888    
#Hard          1.0056     1.8458   0.545 0.585880    

###Canada Warbler showed similar + response to habitat and n.s. responses 
#to soft and hard energy footprint when measured as binary or Poisson
summary(h2.bin$OVEN_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.4898     0.2360  -2.075    0.038 *  
#Total.Decid   3.7180     0.3441  10.805   <2e-16 ***
#Soft         -2.2849     1.8846  -1.212    0.225    
#Hard         -0.4162     0.7064  -0.589    0.556    
summary(h2.max$OVEN_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.57257    0.14826  -3.862 0.000113 ***
#Total.Decid  1.19071    0.09967  11.946  < 2e-16 ***
#Soft        -0.92983    0.89518  -1.039 0.298941    
#Hard        -0.44685    0.38699  -1.155 0.248214      

###Ovenbird increased Decid and showed n.s. - response to soft and 
#hard energy footprint whether modelled as binary or Poisson

summary(h2.bin$SWTH_bin)
#                    Estimate Std. Error z value Pr(>|z|)   
#(Intercept)   0.6109     0.1884   3.243  0.00118 ** 
#Total.Decid   0.3842     0.2455   1.565  0.11751    
#Soft         -2.3869     1.7082  -1.397  0.16232    
#Hard         -3.2107     0.6975  -4.603 4.16e-06 ***
summary(h2.max$SWTH_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.14659    0.09572  -1.531    0.126    
#Total.Decid  0.04121    0.11688   0.353    0.724    
#Soft        -0.82828    0.98632  -0.840    0.401    
#Hard        -2.01553    0.45986  -4.383 1.17e-05 *** 

###Swainson's Thrush responded - to hard and n.s. - to soft footprint 
#whether modelled as binary or Poisson distribution

summary(h2.bin$WTSP_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   0.5590     0.2965   1.885   0.0594 .  
#Total.Decid   2.8883     0.4098   7.048 1.82e-12 ***
#Soft          3.5826     2.5800   1.389   0.1650    
#Hard          2.3201     1.1090   2.092   0.0364 *   
summary(h2.max$WTSP_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.03099    0.15371  -0.202    0.840    
#Total.Decid  0.67289    0.08813   7.635 2.25e-14 ***
#Soft         0.41095    0.67323   0.610    0.542    
#Hard         0.24922    0.25102   0.993    0.321  

###White-throated Sparrow shows similar significant + responses to
#deciduous and responds + to soft and hard energy footprint, but only
#significantly so to hard footprint, when modelled as a Poisson distribution.

library(dplyr)
library(tidyr)
#2x2 scale
birds4plot<-birds.habitatHF[!is.na(birds.habitatHF$Grp2x2),]
birds4plot$unit<-paste0(birds4plot$Grid,"_",birds4plot$Grp2x2)
birds4plot.summ<-birds4plot%>%
  group_by(unit)%>%
  summarise_at(vars(ALFL_mean:YWAR_bin), sum)
hab4plot.summ<-birds4plot%>%
  group_by(unit)%>%
  summarise_at(vars(Conif0:CCPine4), mean)
birdshab4plot.summ<-data.frame(birds4plot.summ,hab4plot.summ)
write.csv(birdshab4plot.summ,file="birdshab4plot.summ.csv")


birdshab4plot.summ<-read.csv("birdshab4plot.summ.csv", header=TRUE)
birdshab4plot.summ$Total.Linear<-birdshab4plot.summ$SeismicLine+
  birdshab4plot.summ$Pipeline+birdshab4plot.summ$RailHardSurface+
  birdshab4plot.summ$RailVegetatedVerge+birdshab4plot.summ$RoadHardSurface+
  birdshab4plot.summ$RoadTrailVegetated+birdshab4plot.summ$RoadVegetatedVerge+
  birdshab4plot.summ$TransmissionLine

birdshab4plot.summ$Soft.Linear<-birdshab4plot.summ$SeismicLine+
  birdshab4plot.summ$Pipeline+
  birdshab4plot.summ$RailVegetatedVerge+
  birdshab4plot.summ$RoadTrailVegetated+birdshab4plot.summ$RoadVegetatedVerge+
  birdshab4plot.summ$TransmissionLine

birdshab4plot.summ$Hard.Linear<-birdshab4plot.summ$RailHardSurface+
  birdshab4plot.summ$RoadHardSurface

birdshab4plot.summ$Total.Harvest<-birdshab4plot.summ$CCMixwood0+
  birdshab4plot.summ$CCMixwood1+
  birdshab4plot.summ$CCMixwood2+
  birdshab4plot.summ$CCMixwood3+
  birdshab4plot.summ$CCMixwood4+
  birdshab4plot.summ$CCMixwoodR+
  birdshab4plot.summ$CCPine0+
  birdshab4plot.summ$CCPine1+
  birdshab4plot.summ$CCPine2+
  birdshab4plot.summ$CCPine3+
  birdshab4plot.summ$CCPine4+
  birdshab4plot.summ$CCPineR+
  birdshab4plot.summ$CCConif0+
  birdshab4plot.summ$CCConif1+
  birdshab4plot.summ$CCConif2+
  birdshab4plot.summ$CCConif3+
  birdshab4plot.summ$CCConif4+
  birdshab4plot.summ$CCConifR+
  birdshab4plot.summ$CCDecid0+
  birdshab4plot.summ$CCDecid1+
  birdshab4plot.summ$CCDecid2+
  birdshab4plot.summ$CCDecid3+
  birdshab4plot.summ$CCDecid4+
  birdshab4plot.summ$CCDecidR

birdshab4plot.summ$Nonlinear<-birdshab4plot.summ$Total.Harvest+
  birdshab4plot.summ$WellSite+birdshab4plot.summ$MineSite+
  birdshab4plot.summ$IndustrialSiteRural+
  birdshab4plot.summ$BorrowpitsDugoutsSumps+
  birdshab4plot.summ$RuralResidentialIndustrial+
  birdshab4plot.summ$Urban+
  birdshab4plot.summ$PeatMine+
  birdshab4plot.summ$CultivationCropPastureBareground+
  birdshab4plot.summ$HighDensityLivestockOperation

birdshab4plot.summ$Total.Forwet<-birdshab4plot.summ$Wetland.Decid1+
  birdshab4plot.summ$Wetland.Decid2+
  birdshab4plot.summ$Wetland.Decid3+
  birdshab4plot.summ$Wetland.Decid4+
  birdshab4plot.summ$Wetland.DecidR+
  birdshab4plot.summ$Wetland.Decid0+
  birdshab4plot.summ$Wetland.Decid5+
  birdshab4plot.summ$Wetland.Decid6+
  birdshab4plot.summ$Wetland.Decid7+
  birdshab4plot.summ$Wetland.Decid8+
  birdshab4plot.summ$Wetland.Decid9+
  birdshab4plot.summ$Wetland.Larch1+
  birdshab4plot.summ$Wetland.Larch2+
  birdshab4plot.summ$Wetland.Larch3+
  birdshab4plot.summ$Wetland.Larch4+
  birdshab4plot.summ$Wetland.LarchR+
  birdshab4plot.summ$Wetland.Larch0+
  birdshab4plot.summ$Wetland.Larch5+
  birdshab4plot.summ$Wetland.Larch6+
  birdshab4plot.summ$Wetland.Larch7+
  birdshab4plot.summ$Wetland.Larch8+
  birdshab4plot.summ$Wetland.Larch9+
  birdshab4plot.summ$Wetland.BSpr1+
  birdshab4plot.summ$Wetland.BSpr2+
  birdshab4plot.summ$Wetland.BSpr3+
  birdshab4plot.summ$Wetland.BSpr4+
  birdshab4plot.summ$Wetland.BSprR+
  birdshab4plot.summ$Wetland.BSpr0+
  birdshab4plot.summ$Wetland.BSpr5+
  birdshab4plot.summ$Wetland.BSpr6+
  birdshab4plot.summ$Wetland.BSpr7+
  birdshab4plot.summ$Wetland.BSpr8+
  birdshab4plot.summ$Wetland.BSpr9+
  birdshab4plot.summ$Swamp.Decid1+
  birdshab4plot.summ$Swamp.Decid2+
  birdshab4plot.summ$Swamp.Decid3+
  birdshab4plot.summ$Swamp.Decid4+
  birdshab4plot.summ$Swamp.DecidR+
  birdshab4plot.summ$Swamp.Decid0+
  birdshab4plot.summ$Swamp.Decid5+
  birdshab4plot.summ$Swamp.Decid6+
  birdshab4plot.summ$Swamp.Decid7+
  birdshab4plot.summ$Swamp.Decid8+
  birdshab4plot.summ$Swamp.Decid9+
  birdshab4plot.summ$Swamp.Conif1+
  birdshab4plot.summ$Swamp.Conif2+
  birdshab4plot.summ$Swamp.Conif3+
  birdshab4plot.summ$Swamp.Conif4+
  birdshab4plot.summ$Swamp.ConifR+
  birdshab4plot.summ$Swamp.Conif0+
  birdshab4plot.summ$Swamp.Conif5+
  birdshab4plot.summ$Swamp.Conif6+
  birdshab4plot.summ$Swamp.Conif7+
  birdshab4plot.summ$Swamp.Conif8+
  birdshab4plot.summ$Swamp.Conif9+
  birdshab4plot.summ$Swamp.Mixwood1+
  birdshab4plot.summ$Swamp.Mixwood2+
  birdshab4plot.summ$Swamp.Mixwood3+
  birdshab4plot.summ$Swamp.Mixwood4+
  birdshab4plot.summ$Swamp.MixwoodR+
  birdshab4plot.summ$Swamp.Mixwood0+
  birdshab4plot.summ$Swamp.Mixwood5+
  birdshab4plot.summ$Swamp.Mixwood6+
  birdshab4plot.summ$Swamp.Mixwood7+
  birdshab4plot.summ$Swamp.Mixwood8+
  birdshab4plot.summ$Swamp.Mixwood9+
  birdshab4plot.summ$Swamp.Pine1+
  birdshab4plot.summ$Swamp.Pine2+
  birdshab4plot.summ$Swamp.Pine3+
  birdshab4plot.summ$Swamp.Pine4+
  birdshab4plot.summ$Swamp.PineR+
  birdshab4plot.summ$Swamp.Pine0+
  birdshab4plot.summ$Swamp.Pine5+
  birdshab4plot.summ$Swamp.Pine6+
  birdshab4plot.summ$Swamp.Pine7+
  birdshab4plot.summ$Swamp.Pine8+
  birdshab4plot.summ$Swamp.Pine9


birdshab4plot.summ$Total.Decid<-birdshab4plot.summ$CCDecid0+
  birdshab4plot.summ$CCDecid1+
  birdshab4plot.summ$CCDecid2+
  birdshab4plot.summ$CCDecid3+
  birdshab4plot.summ$CCDecid4+
  birdshab4plot.summ$CCDecidR+
  birdshab4plot.summ$Decid1+
  birdshab4plot.summ$Decid2+
  birdshab4plot.summ$Decid3+
  birdshab4plot.summ$Decid4+
  birdshab4plot.summ$DecidR+
  birdshab4plot.summ$Decid0+
  birdshab4plot.summ$Decid5+
  birdshab4plot.summ$Decid6+
  birdshab4plot.summ$Decid7+
  birdshab4plot.summ$Decid8+
  birdshab4plot.summ$Decid9

birdshab4plot.summ$Total.Conif<-birdshab4plot.summ$CCConif0+
  birdshab4plot.summ$CCConif1+
  birdshab4plot.summ$CCConif2+
  birdshab4plot.summ$CCConif3+
  birdshab4plot.summ$CCConif4+
  birdshab4plot.summ$CCConifR+
  birdshab4plot.summ$Conif1+
  birdshab4plot.summ$Conif2+
  birdshab4plot.summ$Conif3+
  birdshab4plot.summ$Conif4+
  birdshab4plot.summ$ConifR+
  birdshab4plot.summ$Conif0+
  birdshab4plot.summ$Conif5+
  birdshab4plot.summ$Conif6+
  birdshab4plot.summ$Conif7+
  birdshab4plot.summ$Conif8+
  birdshab4plot.summ$Conif9

birdshab4plot.summ$Total.Mixwood<-birdshab4plot.summ$CCMixwood0+
  birdshab4plot.summ$CCMixwood1+
  birdshab4plot.summ$CCMixwood2+
  birdshab4plot.summ$CCMixwood3+
  birdshab4plot.summ$CCMixwood4+
  birdshab4plot.summ$CCMixwoodR+
  birdshab4plot.summ$Mixwood1+
  birdshab4plot.summ$Mixwood2+
  birdshab4plot.summ$Mixwood3+
  birdshab4plot.summ$Mixwood4+
  birdshab4plot.summ$MixwoodR+
  birdshab4plot.summ$Mixwood0+
  birdshab4plot.summ$Mixwood5+
  birdshab4plot.summ$Mixwood6+
  birdshab4plot.summ$Mixwood7+
  birdshab4plot.summ$Mixwood8+
  birdshab4plot.summ$Mixwood9

birdshab4plot.summ$Total.Pine<-birdshab4plot.summ$CCPine0+
  birdshab4plot.summ$CCPine1+
  birdshab4plot.summ$CCPine2+
  birdshab4plot.summ$CCPine3+
  birdshab4plot.summ$CCPine4+
  birdshab4plot.summ$CCPineR+
  birdshab4plot.summ$Pine1+
  birdshab4plot.summ$Pine2+
  birdshab4plot.summ$Pine3+
  birdshab4plot.summ$Pine4+
  birdshab4plot.summ$PineR+
  birdshab4plot.summ$Pine0+
  birdshab4plot.summ$Pine5+
  birdshab4plot.summ$Pine6+
  birdshab4plot.summ$Pine7+
  birdshab4plot.summ$Pine8+
  birdshab4plot.summ$Pine9

birdshab4plot.summ$Soft<-birdshab4plot.summ$SeismicLine+
  birdshab4plot.summ$Pipeline

birdshab4plot.summ$Hard<-birdshab4plot.summ$WellSite+
  birdshab4plot.summ$RailHardSurface+
  birdshab4plot.summ$RailVegetatedVerge+
  birdshab4plot.summ$RoadHardSurface+
  birdshab4plot.summ$RoadTrailVegetated+
  birdshab4plot.summ$RoadVegetatedVerge+
  birdshab4plot.summ$MineSite+
  birdshab4plot.summ$BorrowpitsDugoutsSumps+
  birdshab4plot.summ$PeatMine

#Basic Habitat Model
spp.list.max<-c("BRCR_max",
                "BTNW_max",
                "CAWA_max",
                "OVEN_max",
                "SWTH_max",
                "WTSP_max")

h4.max<-list()
for (i in spp.list.max) {
  birdshab4plot.summ$spp<-birdshab4plot.summ[,i]
  try(h4.max[[i]]<-glmer(spp~Total.Decid+Hard+Soft+(1|grid), family="poisson", data=birdshab4plot.summ))
}
summary(h4.max$CAWA_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.8239     0.5841  -6.547 5.89e-11 ***
#Total.Decid   2.6396     0.9107   2.899  0.00375 ** 
#Hard          4.3314     3.2164   1.347  0.17808    
#Soft         -3.4597     8.7304  -0.396  0.69190          
summary(h4.max$OVEN_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   0.8090     0.1508   5.364 8.14e-08 ***
#Total.Decid   1.5338     0.1755   8.738  < 2e-16 ***
#Hard         -0.1979     0.6522  -0.303   0.7616    
#Soft         -3.3900     1.7236  -1.967   0.0492 *   
summary(h4.max$WTSP_max)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.3305     0.1557   8.547  < 2e-16 ***
#Total.Decid   0.7247     0.1558   4.651  3.3e-06 ***
#Hard          0.5471     0.4462   1.226    0.220    
#Soft          0.6964     1.2954   0.538    0.591     
spp.list.bin<-c("BRCR_bin",
                "BTNW_bin",
                "CAWA_bin",
                "OVEN_bin",
                "SWTH_bin",
                "WTSP_bin")

h4.bin<-list()
for (i in spp.list.bin) {
  birdshab4plot.summ$spp<-birdshab4plot.summ[,i]
  try(h4.bin[[i]]<-glmer(spp~Total.Decid+Hard+Soft+(1|grid), family="poisson", data=birdshab4plot.summ))
}
summary(h4.bin$CAWA_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.9182     0.6117  -6.405  1.5e-10 ***
#Total.Decid   2.3808     0.9908   2.403   0.0163 *  
#Hard          4.7787     3.3096   1.444   0.1488    
#Soft         -1.6255     8.9254  -0.182   0.8555    
summary(h4.bin$OVEN_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.39477    0.13102   3.013  0.00259 ** 
#Total.Decid  1.31254    0.23436   5.601 2.14e-08 ***
#Hard        -0.05562    0.79480  -0.070  0.94421    
#Soft        -1.90290    2.10659  -0.903  0.36636    
summary(h4.bin$WTSP_bin)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   0.8280     0.1213   6.828 8.59e-12 ***
#Total.Decid   0.5294     0.2097   2.525   0.0116 *  
#Hard          0.5719     0.5941   0.963   0.3358    
#Soft          1.7944     1.6996   1.056   0.2911    

#3x3 scale
birds9plot<-birds.habitatHF[!is.na(birds.habitatHF$Grp3x3),]
birds9plot$unit<-paste0(birds9plot$Grid,"_",birds9plot$Grp3x3)
birds9plot.summ<-birds9plot%>%
  group_by(unit)%>%
  summarise_at(vars(ALFL_mean:YWAR_bin), sum)
hab9plot.summ<-birds9plot%>%
  group_by(unit)%>%
  summarise_at(vars(Conif0:CCPine4), mean)
birdshab9plot.summ<-data.frame(birds9plot.summ,hab9plot.summ)
write.csv(birdshab9plot.summ,file="birdshab9plot.summ.csv")


birdshab9plot.summ<-read.csv("birdshab9plot.summ.csv", header=TRUE)
birdshab9plot.summ$Total.Linear<-birdshab9plot.summ$SeismicLine+
  birdshab9plot.summ$Pipeline+birdshab9plot.summ$RailHardSurface+
  birdshab9plot.summ$RailVegetatedVerge+birdshab9plot.summ$RoadHardSurface+
  birdshab9plot.summ$RoadTrailVegetated+birdshab9plot.summ$RoadVegetatedVerge+
  birdshab9plot.summ$TransmissionLine

birdshab9plot.summ$Soft.Linear<-birdshab9plot.summ$SeismicLine+
  birdshab9plot.summ$Pipeline+
  birdshab9plot.summ$RailVegetatedVerge+
  birdshab9plot.summ$RoadTrailVegetated+birdshab9plot.summ$RoadVegetatedVerge+
  birdshab9plot.summ$TransmissionLine

birdshab9plot.summ$Hard.Linear<-birdshab9plot.summ$RailHardSurface+
  birdshab9plot.summ$RoadHardSurface

birdshab9plot.summ$Total.Harvest<-birdshab9plot.summ$CCMixwood0+
  birdshab9plot.summ$CCMixwood1+
  birdshab9plot.summ$CCMixwood2+
  birdshab9plot.summ$CCMixwood3+
  birdshab9plot.summ$CCMixwood4+
  birdshab9plot.summ$CCMixwoodR+
  birdshab9plot.summ$CCPine0+
  birdshab9plot.summ$CCPine1+
  birdshab9plot.summ$CCPine2+
  birdshab9plot.summ$CCPine3+
  birdshab9plot.summ$CCPine4+
  birdshab9plot.summ$CCPineR+
  birdshab9plot.summ$CCConif0+
  birdshab9plot.summ$CCConif1+
  birdshab9plot.summ$CCConif2+
  birdshab9plot.summ$CCConif3+
  birdshab9plot.summ$CCConif4+
  birdshab9plot.summ$CCConifR+
  birdshab9plot.summ$CCDecid0+
  birdshab9plot.summ$CCDecid1+
  birdshab9plot.summ$CCDecid2+
  birdshab9plot.summ$CCDecid3+
  birdshab9plot.summ$CCDecid4+
  birdshab9plot.summ$CCDecidR

birdshab9plot.summ$Nonlinear<-birdshab9plot.summ$Total.Harvest+
  birdshab9plot.summ$WellSite+birdshab9plot.summ$MineSite+
  birdshab9plot.summ$IndustrialSiteRural+
  birdshab9plot.summ$BorrowpitsDugoutsSumps+
  birdshab9plot.summ$RuralResidentialIndustrial+
  birdshab9plot.summ$Urban+
  birdshab9plot.summ$PeatMine+
  birdshab9plot.summ$CultivationCropPastureBareground+
  birdshab9plot.summ$HighDensityLivestockOperation

birdshab9plot.summ$Total.Forwet<-birdshab9plot.summ$Wetland.Decid1+
  birdshab9plot.summ$Wetland.Decid2+
  birdshab9plot.summ$Wetland.Decid3+
  birdshab9plot.summ$Wetland.Decid4+
  birdshab9plot.summ$Wetland.DecidR+
  birdshab9plot.summ$Wetland.Decid0+
  birdshab9plot.summ$Wetland.Decid5+
  birdshab9plot.summ$Wetland.Decid6+
  birdshab9plot.summ$Wetland.Decid7+
  birdshab9plot.summ$Wetland.Decid8+
  birdshab9plot.summ$Wetland.Decid9+
  birdshab9plot.summ$Wetland.Larch1+
  birdshab9plot.summ$Wetland.Larch2+
  birdshab9plot.summ$Wetland.Larch3+
  birdshab9plot.summ$Wetland.Larch4+
  birdshab9plot.summ$Wetland.LarchR+
  birdshab9plot.summ$Wetland.Larch0+
  birdshab9plot.summ$Wetland.Larch5+
  birdshab9plot.summ$Wetland.Larch6+
  birdshab9plot.summ$Wetland.Larch7+
  birdshab9plot.summ$Wetland.Larch8+
  birdshab9plot.summ$Wetland.Larch9+
  birdshab9plot.summ$Wetland.BSpr1+
  birdshab9plot.summ$Wetland.BSpr2+
  birdshab9plot.summ$Wetland.BSpr3+
  birdshab9plot.summ$Wetland.BSpr4+
  birdshab9plot.summ$Wetland.BSprR+
  birdshab9plot.summ$Wetland.BSpr0+
  birdshab9plot.summ$Wetland.BSpr5+
  birdshab9plot.summ$Wetland.BSpr6+
  birdshab9plot.summ$Wetland.BSpr7+
  birdshab9plot.summ$Wetland.BSpr8+
  birdshab9plot.summ$Wetland.BSpr9+
  birdshab9plot.summ$Swamp.Decid1+
  birdshab9plot.summ$Swamp.Decid2+
  birdshab9plot.summ$Swamp.Decid3+
  birdshab9plot.summ$Swamp.Decid4+
  birdshab9plot.summ$Swamp.DecidR+
  birdshab9plot.summ$Swamp.Decid0+
  birdshab9plot.summ$Swamp.Decid5+
  birdshab9plot.summ$Swamp.Decid6+
  birdshab9plot.summ$Swamp.Decid7+
  birdshab9plot.summ$Swamp.Decid8+
  birdshab9plot.summ$Swamp.Decid9+
  birdshab9plot.summ$Swamp.Conif1+
  birdshab9plot.summ$Swamp.Conif2+
  birdshab9plot.summ$Swamp.Conif3+
  birdshab9plot.summ$Swamp.Conif4+
  birdshab9plot.summ$Swamp.ConifR+
  birdshab9plot.summ$Swamp.Conif0+
  birdshab9plot.summ$Swamp.Conif5+
  birdshab9plot.summ$Swamp.Conif6+
  birdshab9plot.summ$Swamp.Conif7+
  birdshab9plot.summ$Swamp.Conif8+
  birdshab9plot.summ$Swamp.Conif9+
  birdshab9plot.summ$Swamp.Mixwood1+
  birdshab9plot.summ$Swamp.Mixwood2+
  birdshab9plot.summ$Swamp.Mixwood3+
  birdshab9plot.summ$Swamp.Mixwood4+
  birdshab9plot.summ$Swamp.MixwoodR+
  birdshab9plot.summ$Swamp.Mixwood0+
  birdshab9plot.summ$Swamp.Mixwood5+
  birdshab9plot.summ$Swamp.Mixwood6+
  birdshab9plot.summ$Swamp.Mixwood7+
  birdshab9plot.summ$Swamp.Mixwood8+
  birdshab9plot.summ$Swamp.Mixwood9+
  birdshab9plot.summ$Swamp.Pine1+
  birdshab9plot.summ$Swamp.Pine2+
  birdshab9plot.summ$Swamp.Pine3+
  birdshab9plot.summ$Swamp.Pine4+
  birdshab9plot.summ$Swamp.PineR+
  birdshab9plot.summ$Swamp.Pine0+
  birdshab9plot.summ$Swamp.Pine5+
  birdshab9plot.summ$Swamp.Pine6+
  birdshab9plot.summ$Swamp.Pine7+
  birdshab9plot.summ$Swamp.Pine8+
  birdshab9plot.summ$Swamp.Pine9


birdshab9plot.summ$Total.Decid<-birdshab9plot.summ$CCDecid0+
  birdshab9plot.summ$CCDecid1+
  birdshab9plot.summ$CCDecid2+
  birdshab9plot.summ$CCDecid3+
  birdshab9plot.summ$CCDecid4+
  birdshab9plot.summ$CCDecidR+
  birdshab9plot.summ$Decid1+
  birdshab9plot.summ$Decid2+
  birdshab9plot.summ$Decid3+
  birdshab9plot.summ$Decid4+
  birdshab9plot.summ$DecidR+
  birdshab9plot.summ$Decid0+
  birdshab9plot.summ$Decid5+
  birdshab9plot.summ$Decid6+
  birdshab9plot.summ$Decid7+
  birdshab9plot.summ$Decid8+
  birdshab9plot.summ$Decid9

birdshab9plot.summ$Total.Conif<-birdshab9plot.summ$CCConif0+
  birdshab9plot.summ$CCConif1+
  birdshab9plot.summ$CCConif2+
  birdshab9plot.summ$CCConif3+
  birdshab9plot.summ$CCConif4+
  birdshab9plot.summ$CCConifR+
  birdshab9plot.summ$Conif1+
  birdshab9plot.summ$Conif2+
  birdshab9plot.summ$Conif3+
  birdshab9plot.summ$Conif4+
  birdshab9plot.summ$ConifR+
  birdshab9plot.summ$Conif0+
  birdshab9plot.summ$Conif5+
  birdshab9plot.summ$Conif6+
  birdshab9plot.summ$Conif7+
  birdshab9plot.summ$Conif8+
  birdshab9plot.summ$Conif9

birdshab9plot.summ$Total.Mixwood<-birdshab9plot.summ$CCMixwood0+
  birdshab9plot.summ$CCMixwood1+
  birdshab9plot.summ$CCMixwood2+
  birdshab9plot.summ$CCMixwood3+
  birdshab9plot.summ$CCMixwood4+
  birdshab9plot.summ$CCMixwoodR+
  birdshab9plot.summ$Mixwood1+
  birdshab9plot.summ$Mixwood2+
  birdshab9plot.summ$Mixwood3+
  birdshab9plot.summ$Mixwood4+
  birdshab9plot.summ$MixwoodR+
  birdshab9plot.summ$Mixwood0+
  birdshab9plot.summ$Mixwood5+
  birdshab9plot.summ$Mixwood6+
  birdshab9plot.summ$Mixwood7+
  birdshab9plot.summ$Mixwood8+
  birdshab9plot.summ$Mixwood9

birdshab9plot.summ$Total.Pine<-birdshab9plot.summ$CCPine0+
  birdshab9plot.summ$CCPine1+
  birdshab9plot.summ$CCPine2+
  birdshab9plot.summ$CCPine3+
  birdshab9plot.summ$CCPine4+
  birdshab9plot.summ$CCPineR+
  birdshab9plot.summ$Pine1+
  birdshab9plot.summ$Pine2+
  birdshab9plot.summ$Pine3+
  birdshab9plot.summ$Pine4+
  birdshab9plot.summ$PineR+
  birdshab9plot.summ$Pine0+
  birdshab9plot.summ$Pine5+
  birdshab9plot.summ$Pine6+
  birdshab9plot.summ$Pine7+
  birdshab9plot.summ$Pine8+
  birdshab9plot.summ$Pine9

birdshab9plot.summ$Soft<-birdshab9plot.summ$SeismicLine+
  birdshab9plot.summ$Pipeline

birdshab9plot.summ$Hard<-birdshab9plot.summ$WellSite+
  birdshab9plot.summ$RailHardSurface+
  birdshab9plot.summ$RailVegetatedVerge+
  birdshab9plot.summ$RoadHardSurface+
  birdshab9plot.summ$RoadTrailVegetated+
  birdshab9plot.summ$RoadVegetatedVerge+
  birdshab9plot.summ$MineSite+
  birdshab9plot.summ$BorrowpitsDugoutsSumps+
  birdshab9plot.summ$PeatMine

#Basic Habitat Model
spp.list.max<-c("BRCR_max",
                "BTNW_max",
                "CAWA_max",
                "OVEN_max",
                "SWTH_max",
                "WTSP_max")

h9.max<-list()
for (i in spp.list.max) {
  birdshab9plot.summ$spp<-birdshab9plot.summ[,i]
  try(h9.max[[i]]<-glmer(spp~Total.Decid+Hard+Soft+(1|grid), family="poisson", data=birdshab9plot.summ))
}
summary(h9.max$CAWA_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.5367     0.6762  -5.230  1.7e-07 ***
#Total.Decid   4.1388     1.4613   2.832  0.00462 ** 
#Hard          9.7767     4.7113   2.075  0.03797 *  
#Soft         -2.9147    12.3377  -0.236  0.81325         
summary(h9.max$OVEN_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.6560     0.1616  10.249  < 2e-16 ***
#Total.Decid   1.5500     0.2739   5.659 1.53e-08 ***
#Hard         -0.5035     1.0052  -0.501    0.616    
#Soft         -3.4581     2.3785  -1.454    0.146  
summary(h9.max$WTSP_max)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.0190     0.1574  12.830  < 2e-16 ***
#Total.Decid   1.1520     0.2430   4.740 2.14e-06 ***
#Hard          0.8058     0.6798   1.185    0.236    
#Soft          1.8132     1.8034   1.005    0.315     
spp.list.bin<-c("BRCR_bin",
                "BTNW_bin",
                "CAWA_bin",
                "OVEN_bin",
                "SWTH_bin",
                "WTSP_bin")

h9.bin<-list()
for (i in spp.list.bin) {
  birdshab9plot.summ$spp<-birdshab9plot.summ[,i]
  try(h9.bin[[i]]<-glmer(spp~Total.Decid+Hard+Soft+(1|grid), family="poisson", data=birdshab9plot.summ))
}
summary(h9.bin$CAWA_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.4603     0.6835  -5.062 4.14e-07 ***
#Total.Decid   3.8149     1.5312   2.491   0.0127 *  
#Hard          8.5130     4.8306   1.762   0.0780 .  
#Soft         -1.0217    12.5965  -0.081   0.9354    
summary(h9.bin$OVEN_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.1977     0.1449   8.267  < 2e-16 ***
#Total.Decid   1.4706     0.3714   3.960 7.49e-05 ***
#Hard         -0.3882     1.1857  -0.327    0.743    
#Soft         -1.5041     2.8817  -0.522    0.602        
summary(h9.bin$WTSP_bin)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.5163     0.1298  11.683  < 2e-16 ***
#Total.Decid   1.0464     0.3056   3.424 0.000616 ***
#Hard          0.7929     0.8916   0.889 0.373873    
#Soft          1.9106     2.3638   0.808 0.418951     




#4x4 scale
birds16plot<-birds.habitatHF[!is.na(birds.habitatHF$Grp4x4),]
birds16plot$unit<-paste0(birds16plot$Grid,"_",birds16plot$Grp4x4)
birds16plot.summ<-birds16plot%>%
  group_by(unit)%>%
  summarise_at(vars(ALFL_mean:YWAR_bin), sum)
hab16plot.summ<-birds16plot%>%
  group_by(unit)%>%
  summarise_at(vars(Conif0:CCPine4), mean)
birdshab16plot.summ<-data.frame(birds16plot.summ,hab16plot.summ)
write.csv(birdshab16plot.summ,file="birdshab16plot.summ.csv")

birdshab16plot.summ<-read.csv("birdshab16plot.summ.csv", header=TRUE)
birdshab16plot.summ$Total.Linear<-birdshab16plot.summ$SeismicLine+
  birdshab16plot.summ$Pipeline+birdshab16plot.summ$RailHardSurface+
  birdshab16plot.summ$RailVegetatedVerge+birdshab16plot.summ$RoadHardSurface+
  birdshab16plot.summ$RoadTrailVegetated+birdshab16plot.summ$RoadVegetatedVerge+
  birdshab16plot.summ$TransmissionLine

birdshab16plot.summ$Soft.Linear<-birdshab16plot.summ$SeismicLine+
  birdshab16plot.summ$Pipeline+
  birdshab16plot.summ$RailVegetatedVerge+
  birdshab16plot.summ$RoadTrailVegetated+birdshab16plot.summ$RoadVegetatedVerge+
  birdshab16plot.summ$TransmissionLine

birdshab16plot.summ$Hard.Linear<-birdshab16plot.summ$RailHardSurface+
  birdshab16plot.summ$RoadHardSurface

birdshab16plot.summ$Total.Harvest<-birdshab16plot.summ$CCMixwood0+
  birdshab16plot.summ$CCMixwood1+
  birdshab16plot.summ$CCMixwood2+
  birdshab16plot.summ$CCMixwood3+
  birdshab16plot.summ$CCMixwood4+
  birdshab16plot.summ$CCMixwoodR+
  birdshab16plot.summ$CCPine0+
  birdshab16plot.summ$CCPine1+
  birdshab16plot.summ$CCPine2+
  birdshab16plot.summ$CCPine3+
  birdshab16plot.summ$CCPine4+
  birdshab16plot.summ$CCPineR+
  birdshab16plot.summ$CCConif0+
  birdshab16plot.summ$CCConif1+
  birdshab16plot.summ$CCConif2+
  birdshab16plot.summ$CCConif3+
  birdshab16plot.summ$CCConif4+
  birdshab16plot.summ$CCConifR+
  birdshab16plot.summ$CCDecid0+
  birdshab16plot.summ$CCDecid1+
  birdshab16plot.summ$CCDecid2+
  birdshab16plot.summ$CCDecid3+
  birdshab16plot.summ$CCDecid4+
  birdshab16plot.summ$CCDecidR

birdshab16plot.summ$Nonlinear<-birdshab16plot.summ$Total.Harvest+
  birdshab16plot.summ$WellSite+birdshab16plot.summ$MineSite+
  birdshab16plot.summ$IndustrialSiteRural+
  birdshab16plot.summ$BorrowpitsDugoutsSumps+
  birdshab16plot.summ$RuralResidentialIndustrial+
  birdshab16plot.summ$Urban+
  birdshab16plot.summ$PeatMine+
  birdshab16plot.summ$CultivationCropPastureBareground+
  birdshab16plot.summ$HighDensityLivestockOperation

birdshab16plot.summ$Total.Forwet<-birdshab16plot.summ$Wetland.Decid1+
  birdshab16plot.summ$Wetland.Decid2+
  birdshab16plot.summ$Wetland.Decid3+
  birdshab16plot.summ$Wetland.Decid4+
  birdshab16plot.summ$Wetland.DecidR+
  birdshab16plot.summ$Wetland.Decid0+
  birdshab16plot.summ$Wetland.Decid5+
  birdshab16plot.summ$Wetland.Decid6+
  birdshab16plot.summ$Wetland.Decid7+
  birdshab16plot.summ$Wetland.Decid8+
  birdshab16plot.summ$Wetland.Decid9+
  birdshab16plot.summ$Wetland.Larch1+
  birdshab16plot.summ$Wetland.Larch2+
  birdshab16plot.summ$Wetland.Larch3+
  birdshab16plot.summ$Wetland.Larch4+
  birdshab16plot.summ$Wetland.LarchR+
  birdshab16plot.summ$Wetland.Larch0+
  birdshab16plot.summ$Wetland.Larch5+
  birdshab16plot.summ$Wetland.Larch6+
  birdshab16plot.summ$Wetland.Larch7+
  birdshab16plot.summ$Wetland.Larch8+
  birdshab16plot.summ$Wetland.Larch9+
  birdshab16plot.summ$Wetland.BSpr1+
  birdshab16plot.summ$Wetland.BSpr2+
  birdshab16plot.summ$Wetland.BSpr3+
  birdshab16plot.summ$Wetland.BSpr4+
  birdshab16plot.summ$Wetland.BSprR+
  birdshab16plot.summ$Wetland.BSpr0+
  birdshab16plot.summ$Wetland.BSpr5+
  birdshab16plot.summ$Wetland.BSpr6+
  birdshab16plot.summ$Wetland.BSpr7+
  birdshab16plot.summ$Wetland.BSpr8+
  birdshab16plot.summ$Wetland.BSpr9+
  birdshab16plot.summ$Swamp.Decid1+
  birdshab16plot.summ$Swamp.Decid2+
  birdshab16plot.summ$Swamp.Decid3+
  birdshab16plot.summ$Swamp.Decid4+
  birdshab16plot.summ$Swamp.DecidR+
  birdshab16plot.summ$Swamp.Decid0+
  birdshab16plot.summ$Swamp.Decid5+
  birdshab16plot.summ$Swamp.Decid6+
  birdshab16plot.summ$Swamp.Decid7+
  birdshab16plot.summ$Swamp.Decid8+
  birdshab16plot.summ$Swamp.Decid9+
  birdshab16plot.summ$Swamp.Conif1+
  birdshab16plot.summ$Swamp.Conif2+
  birdshab16plot.summ$Swamp.Conif3+
  birdshab16plot.summ$Swamp.Conif4+
  birdshab16plot.summ$Swamp.ConifR+
  birdshab16plot.summ$Swamp.Conif0+
  birdshab16plot.summ$Swamp.Conif5+
  birdshab16plot.summ$Swamp.Conif6+
  birdshab16plot.summ$Swamp.Conif7+
  birdshab16plot.summ$Swamp.Conif8+
  birdshab16plot.summ$Swamp.Conif9+
  birdshab16plot.summ$Swamp.Mixwood1+
  birdshab16plot.summ$Swamp.Mixwood2+
  birdshab16plot.summ$Swamp.Mixwood3+
  birdshab16plot.summ$Swamp.Mixwood4+
  birdshab16plot.summ$Swamp.MixwoodR+
  birdshab16plot.summ$Swamp.Mixwood0+
  birdshab16plot.summ$Swamp.Mixwood5+
  birdshab16plot.summ$Swamp.Mixwood6+
  birdshab16plot.summ$Swamp.Mixwood7+
  birdshab16plot.summ$Swamp.Mixwood8+
  birdshab16plot.summ$Swamp.Mixwood9+
  birdshab16plot.summ$Swamp.Pine1+
  birdshab16plot.summ$Swamp.Pine2+
  birdshab16plot.summ$Swamp.Pine3+
  birdshab16plot.summ$Swamp.Pine4+
  birdshab16plot.summ$Swamp.PineR+
  birdshab16plot.summ$Swamp.Pine0+
  birdshab16plot.summ$Swamp.Pine5+
  birdshab16plot.summ$Swamp.Pine6+
  birdshab16plot.summ$Swamp.Pine7+
  birdshab16plot.summ$Swamp.Pine8+
  birdshab16plot.summ$Swamp.Pine9


birdshab16plot.summ$Total.Decid<-birdshab16plot.summ$CCDecid0+
  birdshab16plot.summ$CCDecid1+
  birdshab16plot.summ$CCDecid2+
  birdshab16plot.summ$CCDecid3+
  birdshab16plot.summ$CCDecid4+
  birdshab16plot.summ$CCDecidR+
  birdshab16plot.summ$Decid1+
  birdshab16plot.summ$Decid2+
  birdshab16plot.summ$Decid3+
  birdshab16plot.summ$Decid4+
  birdshab16plot.summ$DecidR+
  birdshab16plot.summ$Decid0+
  birdshab16plot.summ$Decid5+
  birdshab16plot.summ$Decid6+
  birdshab16plot.summ$Decid7+
  birdshab16plot.summ$Decid8+
  birdshab16plot.summ$Decid9

birdshab16plot.summ$Total.Conif<-birdshab16plot.summ$CCConif0+
  birdshab16plot.summ$CCConif1+
  birdshab16plot.summ$CCConif2+
  birdshab16plot.summ$CCConif3+
  birdshab16plot.summ$CCConif4+
  birdshab16plot.summ$CCConifR+
  birdshab16plot.summ$Conif1+
  birdshab16plot.summ$Conif2+
  birdshab16plot.summ$Conif3+
  birdshab16plot.summ$Conif4+
  birdshab16plot.summ$ConifR+
  birdshab16plot.summ$Conif0+
  birdshab16plot.summ$Conif5+
  birdshab16plot.summ$Conif6+
  birdshab16plot.summ$Conif7+
  birdshab16plot.summ$Conif8+
  birdshab16plot.summ$Conif9

birdshab16plot.summ$Total.Mixwood<-birdshab16plot.summ$CCMixwood0+
  birdshab16plot.summ$CCMixwood1+
  birdshab16plot.summ$CCMixwood2+
  birdshab16plot.summ$CCMixwood3+
  birdshab16plot.summ$CCMixwood4+
  birdshab16plot.summ$CCMixwoodR+
  birdshab16plot.summ$Mixwood1+
  birdshab16plot.summ$Mixwood2+
  birdshab16plot.summ$Mixwood3+
  birdshab16plot.summ$Mixwood4+
  birdshab16plot.summ$MixwoodR+
  birdshab16plot.summ$Mixwood0+
  birdshab16plot.summ$Mixwood5+
  birdshab16plot.summ$Mixwood6+
  birdshab16plot.summ$Mixwood7+
  birdshab16plot.summ$Mixwood8+
  birdshab16plot.summ$Mixwood9

birdshab16plot.summ$Total.Pine<-birdshab16plot.summ$CCPine0+
  birdshab16plot.summ$CCPine1+
  birdshab16plot.summ$CCPine2+
  birdshab16plot.summ$CCPine3+
  birdshab16plot.summ$CCPine4+
  birdshab16plot.summ$CCPineR+
  birdshab16plot.summ$Pine1+
  birdshab16plot.summ$Pine2+
  birdshab16plot.summ$Pine3+
  birdshab16plot.summ$Pine4+
  birdshab16plot.summ$PineR+
  birdshab16plot.summ$Pine0+
  birdshab16plot.summ$Pine5+
  birdshab16plot.summ$Pine6+
  birdshab16plot.summ$Pine7+
  birdshab16plot.summ$Pine8+
  birdshab16plot.summ$Pine9

birdshab16plot.summ$Soft<-birdshab16plot.summ$SeismicLine+
  birdshab16plot.summ$Pipeline

birdshab16plot.summ$Hard<-birdshab16plot.summ$WellSite+
  birdshab16plot.summ$RailHardSurface+
  birdshab16plot.summ$RailVegetatedVerge+
  birdshab16plot.summ$RoadHardSurface+
  birdshab16plot.summ$RoadTrailVegetated+
  birdshab16plot.summ$RoadVegetatedVerge+
  birdshab16plot.summ$MineSite+
  birdshab16plot.summ$BorrowpitsDugoutsSumps+
  birdshab16plot.summ$PeatMine

#Basic Habitat Model
spp.list.max<-c("BRCR_max",
                "BTNW_max",
                "CAWA_max",
                "OVEN_max",
                "SWTH_max",
                "WTSP_max")

h16.max<-list()
for (i in spp.list.max) {
  birdshab16plot.summ$spp<-birdshab16plot.summ[,i]
  try(h16.max[[i]]<-glmer(spp~Total.Decid+Hard+Soft+(1|grid), family="poisson", data=birdshab16plot.summ))
}
summary(h16.max$CAWA_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.0894     0.7253  -4.259 2.05e-05 ***
#Total.Decid   4.4902     1.6021   2.803  0.00507 ** 
#Hard          8.9459     8.0066   1.117  0.26386    
#Soft         -3.7472    17.6540  -0.212  0.83190        
summary(h16.max$OVEN_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.1429     0.1718  12.474  < 2e-16 ***
#Total.Decid   1.6964     0.3276   5.178 2.24e-07 ***
#Hard         -0.7190     1.3596  -0.529    0.597    
#Soft         -0.3686     3.0380  -0.121    0.903    
summary(h16.max$WTSP_max)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.6298     0.1654  15.895  < 2e-16 ***
#Total.Decid   0.9825     0.2916   3.370 0.000752 ***
#Hard          1.5683     0.9352   1.677 0.093554 .  
#Soft          0.3040     2.2644   0.134 0.893190        
spp.list.bin<-c("BRCR_bin",
                "BTNW_bin",
                "CAWA_bin",
                "OVEN_bin",
                "SWTH_bin",
                "WTSP_bin")

h16.bin<-list()
for (i in spp.list.bin) {
  birdshab16plot.summ$spp<-birdshab16plot.summ[,i]
  try(h16.bin[[i]]<-glmer(spp~Total.Decid+Hard+Soft+(1|grid), family="poisson", data=birdshab16plot.summ))
}
summary(h16.bin$CAWA_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -2.832      0.755  -3.751 0.000176 ***
#Total.Decid    3.082      1.843   1.672 0.094467 .  
#Hard           9.594      8.064   1.190 0.234168    
#Soft          -5.518     17.651  -0.313 0.754573    
summary(h16.bin$OVEN_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.7262     0.1587  10.874  < 2e-16 ***
#Total.Decid   1.6300     0.4289   3.801 0.000144 ***
#Hard         -0.8658     1.5853  -0.546 0.584960    
#Soft          0.1650     3.6200   0.046 0.963636    
summary(h16.bin$WTSP_bin)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.0672     0.1379  14.990  < 2e-16 ***
#Total.Decid   1.0481     0.3482   3.010  0.00261 ** 
#Hard          1.4860     1.2001   1.238  0.21565    
#Soft          1.5255     2.9139   0.524  0.60060     





#5x5 scale
birds25plot<-birds.habitatHF[!is.na(birds.habitatHF$Grp3x3),]
birds25plot$unit<-paste0(birds25plot$Grid,"_",birds25plot$Grp3x3)
birds25plot.summ<-birds25plot%>%
  group_by(unit)%>%
  summarise_at(vars(ALFL_mean:YWAR_bin), sum)
hab25plot.summ<-birds25plot%>%
  group_by(unit)%>%
  summarise_at(vars(Conif0:CCPine4), mean)
birdshab25plot.summ<-data.frame(birds25plot.summ,hab25plot.summ)
write.csv(birdshab25plot.summ,file="birdshab25plot.summ.csv")

birdshab25plot.summ<-read.csv("birdshab25plot.summ.csv", header=TRUE)
birdshab25plot.summ$Total.Linear<-birdshab25plot.summ$SeismicLine+
  birdshab25plot.summ$Pipeline+birdshab25plot.summ$RailHardSurface+
  birdshab25plot.summ$RailVegetatedVerge+birdshab25plot.summ$RoadHardSurface+
  birdshab25plot.summ$RoadTrailVegetated+birdshab25plot.summ$RoadVegetatedVerge+
  birdshab25plot.summ$TransmissionLine

birdshab25plot.summ$Soft.Linear<-birdshab25plot.summ$SeismicLine+
  birdshab25plot.summ$Pipeline+
  birdshab25plot.summ$RailVegetatedVerge+
  birdshab25plot.summ$RoadTrailVegetated+birdshab25plot.summ$RoadVegetatedVerge+
  birdshab25plot.summ$TransmissionLine

birdshab25plot.summ$Hard.Linear<-birdshab25plot.summ$RailHardSurface+
  birdshab25plot.summ$RoadHardSurface

birdshab25plot.summ$Total.Harvest<-birdshab25plot.summ$CCMixwood0+
  birdshab25plot.summ$CCMixwood1+
  birdshab25plot.summ$CCMixwood2+
  birdshab25plot.summ$CCMixwood3+
  birdshab25plot.summ$CCMixwood4+
  birdshab25plot.summ$CCMixwoodR+
  birdshab25plot.summ$CCPine0+
  birdshab25plot.summ$CCPine1+
  birdshab25plot.summ$CCPine2+
  birdshab25plot.summ$CCPine3+
  birdshab25plot.summ$CCPine4+
  birdshab25plot.summ$CCPineR+
  birdshab25plot.summ$CCConif0+
  birdshab25plot.summ$CCConif1+
  birdshab25plot.summ$CCConif2+
  birdshab25plot.summ$CCConif3+
  birdshab25plot.summ$CCConif4+
  birdshab25plot.summ$CCConifR+
  birdshab25plot.summ$CCDecid0+
  birdshab25plot.summ$CCDecid1+
  birdshab25plot.summ$CCDecid2+
  birdshab25plot.summ$CCDecid3+
  birdshab25plot.summ$CCDecid4+
  birdshab25plot.summ$CCDecidR

birdshab25plot.summ$Nonlinear<-birdshab25plot.summ$Total.Harvest+
  birdshab25plot.summ$WellSite+birdshab25plot.summ$MineSite+
  birdshab25plot.summ$IndustrialSiteRural+
  birdshab25plot.summ$BorrowpitsDugoutsSumps+
  birdshab25plot.summ$RuralResidentialIndustrial+
  birdshab25plot.summ$Urban+
  birdshab25plot.summ$PeatMine+
  birdshab25plot.summ$CultivationCropPastureBareground+
  birdshab25plot.summ$HighDensityLivestockOperation

birdshab25plot.summ$Total.Forwet<-birdshab25plot.summ$Wetland.Decid1+
  birdshab25plot.summ$Wetland.Decid2+
  birdshab25plot.summ$Wetland.Decid3+
  birdshab25plot.summ$Wetland.Decid4+
  birdshab25plot.summ$Wetland.DecidR+
  birdshab25plot.summ$Wetland.Decid0+
  birdshab25plot.summ$Wetland.Decid5+
  birdshab25plot.summ$Wetland.Decid6+
  birdshab25plot.summ$Wetland.Decid7+
  birdshab25plot.summ$Wetland.Decid8+
  birdshab25plot.summ$Wetland.Decid9+
  birdshab25plot.summ$Wetland.Larch1+
  birdshab25plot.summ$Wetland.Larch2+
  birdshab25plot.summ$Wetland.Larch3+
  birdshab25plot.summ$Wetland.Larch4+
  birdshab25plot.summ$Wetland.LarchR+
  birdshab25plot.summ$Wetland.Larch0+
  birdshab25plot.summ$Wetland.Larch5+
  birdshab25plot.summ$Wetland.Larch6+
  birdshab25plot.summ$Wetland.Larch7+
  birdshab25plot.summ$Wetland.Larch8+
  birdshab25plot.summ$Wetland.Larch9+
  birdshab25plot.summ$Wetland.BSpr1+
  birdshab25plot.summ$Wetland.BSpr2+
  birdshab25plot.summ$Wetland.BSpr3+
  birdshab25plot.summ$Wetland.BSpr4+
  birdshab25plot.summ$Wetland.BSprR+
  birdshab25plot.summ$Wetland.BSpr0+
  birdshab25plot.summ$Wetland.BSpr5+
  birdshab25plot.summ$Wetland.BSpr6+
  birdshab25plot.summ$Wetland.BSpr7+
  birdshab25plot.summ$Wetland.BSpr8+
  birdshab25plot.summ$Wetland.BSpr9+
  birdshab25plot.summ$Swamp.Decid1+
  birdshab25plot.summ$Swamp.Decid2+
  birdshab25plot.summ$Swamp.Decid3+
  birdshab25plot.summ$Swamp.Decid4+
  birdshab25plot.summ$Swamp.DecidR+
  birdshab25plot.summ$Swamp.Decid0+
  birdshab25plot.summ$Swamp.Decid5+
  birdshab25plot.summ$Swamp.Decid6+
  birdshab25plot.summ$Swamp.Decid7+
  birdshab25plot.summ$Swamp.Decid8+
  birdshab25plot.summ$Swamp.Decid9+
  birdshab25plot.summ$Swamp.Conif1+
  birdshab25plot.summ$Swamp.Conif2+
  birdshab25plot.summ$Swamp.Conif3+
  birdshab25plot.summ$Swamp.Conif4+
  birdshab25plot.summ$Swamp.ConifR+
  birdshab25plot.summ$Swamp.Conif0+
  birdshab25plot.summ$Swamp.Conif5+
  birdshab25plot.summ$Swamp.Conif6+
  birdshab25plot.summ$Swamp.Conif7+
  birdshab25plot.summ$Swamp.Conif8+
  birdshab25plot.summ$Swamp.Conif9+
  birdshab25plot.summ$Swamp.Mixwood1+
  birdshab25plot.summ$Swamp.Mixwood2+
  birdshab25plot.summ$Swamp.Mixwood3+
  birdshab25plot.summ$Swamp.Mixwood4+
  birdshab25plot.summ$Swamp.MixwoodR+
  birdshab25plot.summ$Swamp.Mixwood0+
  birdshab25plot.summ$Swamp.Mixwood5+
  birdshab25plot.summ$Swamp.Mixwood6+
  birdshab25plot.summ$Swamp.Mixwood7+
  birdshab25plot.summ$Swamp.Mixwood8+
  birdshab25plot.summ$Swamp.Mixwood9+
  birdshab25plot.summ$Swamp.Pine1+
  birdshab25plot.summ$Swamp.Pine2+
  birdshab25plot.summ$Swamp.Pine3+
  birdshab25plot.summ$Swamp.Pine4+
  birdshab25plot.summ$Swamp.PineR+
  birdshab25plot.summ$Swamp.Pine0+
  birdshab25plot.summ$Swamp.Pine5+
  birdshab25plot.summ$Swamp.Pine6+
  birdshab25plot.summ$Swamp.Pine7+
  birdshab25plot.summ$Swamp.Pine8+
  birdshab25plot.summ$Swamp.Pine9


birdshab25plot.summ$Total.Decid<-birdshab25plot.summ$CCDecid0+
  birdshab25plot.summ$CCDecid1+
  birdshab25plot.summ$CCDecid2+
  birdshab25plot.summ$CCDecid3+
  birdshab25plot.summ$CCDecid4+
  birdshab25plot.summ$CCDecidR+
  birdshab25plot.summ$Decid1+
  birdshab25plot.summ$Decid2+
  birdshab25plot.summ$Decid3+
  birdshab25plot.summ$Decid4+
  birdshab25plot.summ$DecidR+
  birdshab25plot.summ$Decid0+
  birdshab25plot.summ$Decid5+
  birdshab25plot.summ$Decid6+
  birdshab25plot.summ$Decid7+
  birdshab25plot.summ$Decid8+
  birdshab25plot.summ$Decid9

birdshab25plot.summ$Total.Conif<-birdshab25plot.summ$CCConif0+
  birdshab25plot.summ$CCConif1+
  birdshab25plot.summ$CCConif2+
  birdshab25plot.summ$CCConif3+
  birdshab25plot.summ$CCConif4+
  birdshab25plot.summ$CCConifR+
  birdshab25plot.summ$Conif1+
  birdshab25plot.summ$Conif2+
  birdshab25plot.summ$Conif3+
  birdshab25plot.summ$Conif4+
  birdshab25plot.summ$ConifR+
  birdshab25plot.summ$Conif0+
  birdshab25plot.summ$Conif5+
  birdshab25plot.summ$Conif6+
  birdshab25plot.summ$Conif7+
  birdshab25plot.summ$Conif8+
  birdshab25plot.summ$Conif9

birdshab25plot.summ$Total.Mixwood<-birdshab25plot.summ$CCMixwood0+
  birdshab25plot.summ$CCMixwood1+
  birdshab25plot.summ$CCMixwood2+
  birdshab25plot.summ$CCMixwood3+
  birdshab25plot.summ$CCMixwood4+
  birdshab25plot.summ$CCMixwoodR+
  birdshab25plot.summ$Mixwood1+
  birdshab25plot.summ$Mixwood2+
  birdshab25plot.summ$Mixwood3+
  birdshab25plot.summ$Mixwood4+
  birdshab25plot.summ$MixwoodR+
  birdshab25plot.summ$Mixwood0+
  birdshab25plot.summ$Mixwood5+
  birdshab25plot.summ$Mixwood6+
  birdshab25plot.summ$Mixwood7+
  birdshab25plot.summ$Mixwood8+
  birdshab25plot.summ$Mixwood9

birdshab25plot.summ$Total.Pine<-birdshab25plot.summ$CCPine0+
  birdshab25plot.summ$CCPine1+
  birdshab25plot.summ$CCPine2+
  birdshab25plot.summ$CCPine3+
  birdshab25plot.summ$CCPine4+
  birdshab25plot.summ$CCPineR+
  birdshab25plot.summ$Pine1+
  birdshab25plot.summ$Pine2+
  birdshab25plot.summ$Pine3+
  birdshab25plot.summ$Pine4+
  birdshab25plot.summ$PineR+
  birdshab25plot.summ$Pine0+
  birdshab25plot.summ$Pine5+
  birdshab25plot.summ$Pine6+
  birdshab25plot.summ$Pine7+
  birdshab25plot.summ$Pine8+
  birdshab25plot.summ$Pine9

birdshab25plot.summ$Soft<-birdshab25plot.summ$SeismicLine+
  birdshab25plot.summ$Pipeline

birdshab25plot.summ$Hard<-birdshab25plot.summ$WellSite+
  birdshab25plot.summ$RailHardSurface+
  birdshab25plot.summ$RailVegetatedVerge+
  birdshab25plot.summ$RoadHardSurface+
  birdshab25plot.summ$RoadTrailVegetated+
  birdshab25plot.summ$RoadVegetatedVerge+
  birdshab25plot.summ$MineSite+
  birdshab25plot.summ$BorrowpitsDugoutsSumps+
  birdshab25plot.summ$PeatMine

#Basic Habitat Model
spp.list.max<-c("BRCR_max",
                "BTNW_max",
                "CAWA_max",
                "OVEN_max",
                "SWTH_max",
                "WTSP_max")

h25.max<-list()
for (i in spp.list.max) {
  birdshab25plot.summ$spp<-birdshab25plot.summ[,i]
  try(h25.max[[i]]<-glmer(spp~Total.Decid+Hard+Soft+(1|grid), family="poisson", data=birdshab25plot.summ))
}
summary(h25.max$CAWA_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.5367     0.6762  -5.230  1.7e-07 ***
#Total.Decid   4.1388     1.4613   2.832  0.00462 ** 
#Hard          9.7767     4.7113   2.075  0.03797 *  
#Soft         -2.9147    12.3377  -0.236  0.81325      
summary(h25.max$OVEN_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.6560     0.1616  10.249  < 2e-16 ***
#Total.Decid   1.5500     0.2739   5.659 1.53e-08 ***
#Hard         -0.5035     1.0052  -0.501    0.616    
#Soft         -3.4581     2.3785  -1.454    0.146    
summary(h25.max$WTSP_max)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.0190     0.1574  12.830  < 2e-16 ***
#Total.Decid   1.1520     0.2430   4.740 2.14e-06 ***
#Hard          0.8058     0.6798   1.185    0.236    
#Soft          1.8132     1.8034   1.005    0.315    
spp.list.bin<-c("BRCR_bin",
                "BTNW_bin",
                "CAWA_bin",
                "OVEN_bin",
                "SWTH_bin",
                "WTSP_bin")

h25.bin<-list()
for (i in spp.list.bin) {
  birdshab25plot.summ$spp<-birdshab25plot.summ[,i]
  try(h25.bin[[i]]<-glmer(spp~Total.Decid+Hard+Soft+(1|grid), family="poisson", data=birdshab25plot.summ))
}
summary(h25.bin$CAWA_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.4603     0.6835  -5.062 4.14e-07 ***
#Total.Decid   3.8149     1.5312   2.491   0.0127 *  
#Hard          8.5130     4.8306   1.762   0.0780 .  
#Soft         -1.0217    12.5965  -0.081   0.9354    
summary(h25.bin$OVEN_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.1977     0.1449   8.267  < 2e-16 ***
#Total.Decid   1.4706     0.3714   3.960 7.49e-05 ***
#Hard         -0.3882     1.1857  -0.327    0.743    
#Soft         -1.5041     2.8817  -0.522    0.602    
summary(h25.bin$WTSP_bin)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.5163     0.1298  11.683  < 2e-16 ***
#Total.Decid   1.0464     0.3056   3.424 0.000616 ***
#Hard          0.7929     0.8916   0.889 0.373873    
#Soft          1.9106     2.3638   0.808 0.418951    




#Grid scale (10x10)
birds100plot.summ<-birds.habitatHF%>%
  group_by(Grid)%>%
  summarise_at(vars(ALFL_mean:YWAR_bin), sum)
hab100plot.summ<-birds.habitatHF%>%
  group_by(Grid)%>%
  summarise_at(vars(Conif0:CCPine4), mean)
birdshab100plot.summ<-data.frame(birds100plot.summ,hab100plot.summ)
write.csv(birdshab100plot.summ,file="birdshab100plot.summ.csv")

birdshab100plot.summ<-read.csv("birdshab100plot.summ.csv", header=TRUE)
birdshab100plot.summ$Total.Linear<-birdshab100plot.summ$SeismicLine+
  birdshab100plot.summ$Pipeline+birdshab100plot.summ$RailHardSurface+
  birdshab100plot.summ$RailVegetatedVerge+birdshab100plot.summ$RoadHardSurface+
  birdshab100plot.summ$RoadTrailVegetated+birdshab100plot.summ$RoadVegetatedVerge+
  birdshab100plot.summ$TransmissionLine

birdshab100plot.summ$Soft.Linear<-birdshab100plot.summ$SeismicLine+
  birdshab100plot.summ$Pipeline+
  birdshab100plot.summ$RailVegetatedVerge+
  birdshab100plot.summ$RoadTrailVegetated+birdshab100plot.summ$RoadVegetatedVerge+
  birdshab100plot.summ$TransmissionLine

birdshab100plot.summ$Hard.Linear<-birdshab100plot.summ$RailHardSurface+
  birdshab100plot.summ$RoadHardSurface

birdshab100plot.summ$Total.Harvest<-birdshab100plot.summ$CCMixwood0+
  birdshab100plot.summ$CCMixwood1+
  birdshab100plot.summ$CCMixwood2+
  birdshab100plot.summ$CCMixwood3+
  birdshab100plot.summ$CCMixwood4+
  birdshab100plot.summ$CCMixwoodR+
  birdshab100plot.summ$CCPine0+
  birdshab100plot.summ$CCPine1+
  birdshab100plot.summ$CCPine2+
  birdshab100plot.summ$CCPine3+
  birdshab100plot.summ$CCPine4+
  birdshab100plot.summ$CCPineR+
  birdshab100plot.summ$CCConif0+
  birdshab100plot.summ$CCConif1+
  birdshab100plot.summ$CCConif2+
  birdshab100plot.summ$CCConif3+
  birdshab100plot.summ$CCConif4+
  birdshab100plot.summ$CCConifR+
  birdshab100plot.summ$CCDecid0+
  birdshab100plot.summ$CCDecid1+
  birdshab100plot.summ$CCDecid2+
  birdshab100plot.summ$CCDecid3+
  birdshab100plot.summ$CCDecid4+
  birdshab100plot.summ$CCDecidR

birdshab100plot.summ$Nonlinear<-birdshab100plot.summ$Total.Harvest+
  birdshab100plot.summ$WellSite+birdshab100plot.summ$MineSite+
  birdshab100plot.summ$IndustrialSiteRural+
  birdshab100plot.summ$BorrowpitsDugoutsSumps+
  birdshab100plot.summ$RuralResidentialIndustrial+
  birdshab100plot.summ$Urban+
  birdshab100plot.summ$PeatMine+
  birdshab100plot.summ$CultivationCropPastureBareground+
  birdshab100plot.summ$HighDensityLivestockOperation

birdshab100plot.summ$Total.Forwet<-birdshab100plot.summ$Wetland.Decid1+
  birdshab100plot.summ$Wetland.Decid2+
  birdshab100plot.summ$Wetland.Decid3+
  birdshab100plot.summ$Wetland.Decid4+
  birdshab100plot.summ$Wetland.DecidR+
  birdshab100plot.summ$Wetland.Decid0+
  birdshab100plot.summ$Wetland.Decid5+
  birdshab100plot.summ$Wetland.Decid6+
  birdshab100plot.summ$Wetland.Decid7+
  birdshab100plot.summ$Wetland.Decid8+
  birdshab100plot.summ$Wetland.Decid9+
  birdshab100plot.summ$Wetland.Larch1+
  birdshab100plot.summ$Wetland.Larch2+
  birdshab100plot.summ$Wetland.Larch3+
  birdshab100plot.summ$Wetland.Larch4+
  birdshab100plot.summ$Wetland.LarchR+
  birdshab100plot.summ$Wetland.Larch0+
  birdshab100plot.summ$Wetland.Larch5+
  birdshab100plot.summ$Wetland.Larch6+
  birdshab100plot.summ$Wetland.Larch7+
  birdshab100plot.summ$Wetland.Larch8+
  birdshab100plot.summ$Wetland.Larch9+
  birdshab100plot.summ$Wetland.BSpr1+
  birdshab100plot.summ$Wetland.BSpr2+
  birdshab100plot.summ$Wetland.BSpr3+
  birdshab100plot.summ$Wetland.BSpr4+
  birdshab100plot.summ$Wetland.BSprR+
  birdshab100plot.summ$Wetland.BSpr0+
  birdshab100plot.summ$Wetland.BSpr5+
  birdshab100plot.summ$Wetland.BSpr6+
  birdshab100plot.summ$Wetland.BSpr7+
  birdshab100plot.summ$Wetland.BSpr8+
  birdshab100plot.summ$Wetland.BSpr9+
  birdshab100plot.summ$Swamp.Decid1+
  birdshab100plot.summ$Swamp.Decid2+
  birdshab100plot.summ$Swamp.Decid3+
  birdshab100plot.summ$Swamp.Decid4+
  birdshab100plot.summ$Swamp.DecidR+
  birdshab100plot.summ$Swamp.Decid0+
  birdshab100plot.summ$Swamp.Decid5+
  birdshab100plot.summ$Swamp.Decid6+
  birdshab100plot.summ$Swamp.Decid7+
  birdshab100plot.summ$Swamp.Decid8+
  birdshab100plot.summ$Swamp.Decid9+
  birdshab100plot.summ$Swamp.Conif1+
  birdshab100plot.summ$Swamp.Conif2+
  birdshab100plot.summ$Swamp.Conif3+
  birdshab100plot.summ$Swamp.Conif4+
  birdshab100plot.summ$Swamp.ConifR+
  birdshab100plot.summ$Swamp.Conif0+
  birdshab100plot.summ$Swamp.Conif5+
  birdshab100plot.summ$Swamp.Conif6+
  birdshab100plot.summ$Swamp.Conif7+
  birdshab100plot.summ$Swamp.Conif8+
  birdshab100plot.summ$Swamp.Conif9+
  birdshab100plot.summ$Swamp.Mixwood1+
  birdshab100plot.summ$Swamp.Mixwood2+
  birdshab100plot.summ$Swamp.Mixwood3+
  birdshab100plot.summ$Swamp.Mixwood4+
  birdshab100plot.summ$Swamp.MixwoodR+
  birdshab100plot.summ$Swamp.Mixwood0+
  birdshab100plot.summ$Swamp.Mixwood5+
  birdshab100plot.summ$Swamp.Mixwood6+
  birdshab100plot.summ$Swamp.Mixwood7+
  birdshab100plot.summ$Swamp.Mixwood8+
  birdshab100plot.summ$Swamp.Mixwood9+
  birdshab100plot.summ$Swamp.Pine1+
  birdshab100plot.summ$Swamp.Pine2+
  birdshab100plot.summ$Swamp.Pine3+
  birdshab100plot.summ$Swamp.Pine4+
  birdshab100plot.summ$Swamp.PineR+
  birdshab100plot.summ$Swamp.Pine0+
  birdshab100plot.summ$Swamp.Pine5+
  birdshab100plot.summ$Swamp.Pine6+
  birdshab100plot.summ$Swamp.Pine7+
  birdshab100plot.summ$Swamp.Pine8+
  birdshab100plot.summ$Swamp.Pine9


birdshab100plot.summ$Total.Decid<-birdshab100plot.summ$CCDecid0+
  birdshab100plot.summ$CCDecid1+
  birdshab100plot.summ$CCDecid2+
  birdshab100plot.summ$CCDecid3+
  birdshab100plot.summ$CCDecid4+
  birdshab100plot.summ$CCDecidR+
  birdshab100plot.summ$Decid1+
  birdshab100plot.summ$Decid2+
  birdshab100plot.summ$Decid3+
  birdshab100plot.summ$Decid4+
  birdshab100plot.summ$DecidR+
  birdshab100plot.summ$Decid0+
  birdshab100plot.summ$Decid5+
  birdshab100plot.summ$Decid6+
  birdshab100plot.summ$Decid7+
  birdshab100plot.summ$Decid8+
  birdshab100plot.summ$Decid9

birdshab100plot.summ$Total.Conif<-birdshab100plot.summ$CCConif0+
  birdshab100plot.summ$CCConif1+
  birdshab100plot.summ$CCConif2+
  birdshab100plot.summ$CCConif3+
  birdshab100plot.summ$CCConif4+
  birdshab100plot.summ$CCConifR+
  birdshab100plot.summ$Conif1+
  birdshab100plot.summ$Conif2+
  birdshab100plot.summ$Conif3+
  birdshab100plot.summ$Conif4+
  birdshab100plot.summ$ConifR+
  birdshab100plot.summ$Conif0+
  birdshab100plot.summ$Conif5+
  birdshab100plot.summ$Conif6+
  birdshab100plot.summ$Conif7+
  birdshab100plot.summ$Conif8+
  birdshab100plot.summ$Conif9

birdshab100plot.summ$Total.Mixwood<-birdshab100plot.summ$CCMixwood0+
  birdshab100plot.summ$CCMixwood1+
  birdshab100plot.summ$CCMixwood2+
  birdshab100plot.summ$CCMixwood3+
  birdshab100plot.summ$CCMixwood4+
  birdshab100plot.summ$CCMixwoodR+
  birdshab100plot.summ$Mixwood1+
  birdshab100plot.summ$Mixwood2+
  birdshab100plot.summ$Mixwood3+
  birdshab100plot.summ$Mixwood4+
  birdshab100plot.summ$MixwoodR+
  birdshab100plot.summ$Mixwood0+
  birdshab100plot.summ$Mixwood5+
  birdshab100plot.summ$Mixwood6+
  birdshab100plot.summ$Mixwood7+
  birdshab100plot.summ$Mixwood8+
  birdshab100plot.summ$Mixwood9

birdshab100plot.summ$Total.Pine<-birdshab100plot.summ$CCPine0+
  birdshab100plot.summ$CCPine1+
  birdshab100plot.summ$CCPine2+
  birdshab100plot.summ$CCPine3+
  birdshab100plot.summ$CCPine4+
  birdshab100plot.summ$CCPineR+
  birdshab100plot.summ$Pine1+
  birdshab100plot.summ$Pine2+
  birdshab100plot.summ$Pine3+
  birdshab100plot.summ$Pine4+
  birdshab100plot.summ$PineR+
  birdshab100plot.summ$Pine0+
  birdshab100plot.summ$Pine5+
  birdshab100plot.summ$Pine6+
  birdshab100plot.summ$Pine7+
  birdshab100plot.summ$Pine8+
  birdshab100plot.summ$Pine9

birdshab100plot.summ$Soft<-birdshab100plot.summ$SeismicLine+
  birdshab100plot.summ$Pipeline

birdshab100plot.summ$Hard<-birdshab100plot.summ$WellSite+
  birdshab100plot.summ$RailHardSurface+
  birdshab100plot.summ$RailVegetatedVerge+
  birdshab100plot.summ$RoadHardSurface+
  birdshab100plot.summ$RoadTrailVegetated+
  birdshab100plot.summ$RoadVegetatedVerge+
  birdshab100plot.summ$MineSite+
  birdshab100plot.summ$BorrowpitsDugoutsSumps+
  birdshab100plot.summ$PeatMine

#Basic Habitat Model
spp.list.max<-c("BRCR_max",
                "BTNW_max",
                "CAWA_max",
                "OVEN_max",
                "SWTH_max",
                "WTSP_max")

h100.max<-list()
for (i in spp.list.max) {
  birdshab100plot.summ$spp<-birdshab100plot.summ[,i]
  try(h100.max[[i]]<-glm(spp~Total.Decid+Hard+Soft, family="poisson", data=birdshab100plot.summ))
}
summary(h100.max$CAWA_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.6158     0.6648  -2.430   0.0151 *  
#Total.Decid   8.9543     1.8560   4.825  1.4e-06 ***
#Hard         -2.1278    14.7499  -0.144   0.8853    
#Soft         10.9589    32.2092   0.340   0.7337   
summary(h100.max$OVEN_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  3.43553    0.08421  40.796   <2e-16 ***
#Total.Decid  4.30899    0.26531  16.241   <2e-16 ***
#Hard        -2.00596    2.01926  -0.993    0.321    
#Soft         1.40644    4.63539   0.303    0.762
summary(h100.max$WTSP_max)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   4.20024    0.06379  65.844  < 2e-16 ***
#Total.Decid   2.03669    0.22052   9.236  < 2e-16 ***
#Hard          8.40405    1.30398   6.445 1.16e-10 ***
#Soft        -12.67484    3.14116  -4.035 5.46e-05 ***
spp.list.bin<-c("BRCR_bin",
                "BTNW_bin",
                "CAWA_bin",
                "OVEN_bin",
                "SWTH_bin",
                "WTSP_bin")

h100.bin<-list()
for (i in spp.list.bin) {
  birdshab100plot.summ$spp<-birdshab100plot.summ[,i]
  try(h100.bin[[i]]<-glm(spp~Total.Decid+Hard+Soft, family="poisson", data=birdshab100plot.summ))
}
summary(h100.bin$CAWA_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.44491    0.66252  -2.181   0.0292 *  
#Total.Decid  8.00349    1.88297   4.250 2.13e-05 ***
#Hard         0.04744   14.23027   0.003   0.9973    
#Soft         6.84917   31.37745   0.218   0.8272  
summary(h100.bin$OVEN_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   3.1862     0.1025  31.076   <2e-16 ***
#Total.Decid   3.2981     0.3347   9.855   <2e-16 ***
#Hard         -1.0033     2.4590  -0.408    0.683    
#Soft         -0.2613     5.7303  -0.046    0.964    
summary(h100.bin$WTSP_bin)
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   3.81070    0.08224  46.339  < 2e-16 ***
#Total.Decid   1.52511    0.29018   5.256 1.48e-07 ***
#Hard          7.11913    1.75093   4.066 4.78e-05 ***
#Soft        -12.55926    4.25570  -2.951  0.00317 ** 

summary(h4.bin$BRCR_bin)
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.3837     0.2500  -5.535 3.12e-08 ***
#Total.Decid   2.0732     0.4941   4.196 2.72e-05 ***
#Hard         -5.1507     2.3455  -2.196   0.0281 *  
#Soft         -2.5312     5.2209  -0.485   0.6278    

summary(h4.max$BRCR_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.3221     0.2417  -5.469 4.52e-08 ***
#Total.Decid   2.1809     0.4701   4.639 3.50e-06 ***
#Hard         -5.7935     2.3155  -2.502   0.0123 *  
#Soft         -1.6654     5.0135  -0.332   0.7398    

summary(h9.bin$BRCR_bin)
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.7298     0.2624  -2.782  0.00541 **
#Total.Decid   2.2977     0.7240   3.173  0.00151 **
#Hard         -3.2195     2.9976  -1.074  0.28282   
#Soft          1.1423     6.9011   0.166  0.86853   

summary(h9.max$BRCR_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.6335     0.2585  -2.451  0.01424 * 
#Total.Decid   2.2679     0.7067   3.209  0.00133 **
#Hard         -3.9588     2.9512  -1.341  0.17979   
#Soft          2.2783     6.7329   0.338  0.73508    

summary(h16.bin$BRCR_bin)
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.3616     0.2768  -1.306    0.191    
#Total.Decid   3.1748     0.7209   4.404 1.06e-05 ***
#Hard         -4.0100     3.9238  -1.022    0.307    
#Soft          3.6034     9.1504   0.394    0.694    
summary(h16.max$BRCR_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.2705     0.2690  -1.006    0.315    
#Total.Decid   3.2361     0.6973   4.641 3.47e-06 ***
#Hard         -4.7179     3.8096  -1.238    0.216    
#Soft          4.1708     8.9166   0.468    0.640

summary(h25.bin$BRCR_bin)
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.7298     0.2624  -2.782  0.00541 **
#Total.Decid   2.2977     0.7240   3.173  0.00151 **
#Hard         -3.2195     2.9976  -1.074  0.28282   
#Soft          1.1423     6.9011   0.166  0.86853     

summary(h25.max$BRCR_max)
#            Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  -0.6335     0.2585  -2.451  0.01424 * 
#Total.Decid   2.2679     0.7067   3.209  0.00133 **
#Hard         -3.9588     2.9512  -1.341  0.17979   
#Soft          2.2783     6.7329   0.338  0.73508  

summary(h100.bin$BRCR_bin)
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.2074     0.2752   4.387 1.15e-05 ***
#Total.Decid   4.5235     0.8524   5.307 1.11e-07 ***
#Hard          9.7186     5.4004   1.800   0.0719 .  
#Soft        -29.0856    12.8383  -2.266   0.0235 *     

summary(h100.max$BRCR_max)
# (Intercept)   1.3253     0.2609   5.080 3.78e-07 ***
#   Total.Decid   4.6140     0.8061   5.724 1.04e-08 ***
#   Hard          7.6786     5.3309   1.440   0.1498    
# Soft        -26.6455    12.6037  -2.114   0.0345 *  


summary(h4.bin$BTNW_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.4746     0.8222  -5.442 5.26e-08 ***
#Total.Decid   2.3626     1.1230   2.104   0.0354 *  
#Hard          0.1212     4.0898   0.030   0.9764    
#Soft          4.1112    10.3239   0.398   0.6905 
summary(h4.max$BTNW_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.5787     0.8327  -5.499 3.83e-08 ***
#Total.Decid   2.6494     1.0812   2.451   0.0143 *  
#Hard         -0.2573     3.9531  -0.065   0.9481    
#Soft          5.3833     9.8996   0.544   0.5866 
summary(h9.bin$BTNW_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -3.620      0.853  -4.244 2.19e-05 ***
#Total.Decid    3.154      1.696   1.859    0.063 .  
#Hard           5.791      7.771   0.745    0.456    
#Soft         -17.978     17.598  -1.022    0.307    
summary(h9.max$BTNW_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.7562     0.8664  -4.335 1.46e-05 ***
#Total.Decid   3.4305     1.6473   2.082   0.0373 *  
#Hard          6.6070     7.5241   0.878   0.3799    
#Soft        -17.4858    16.9504  -1.032   0.3023    
summary(h16.bin$BTNW_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.6603     0.9875  -3.707  0.00021 ***
#Total.Decid   4.5132     2.1374   2.112  0.03472 *  
#Hard          8.1109    10.0115   0.810  0.41785    
#Soft         -7.8388    22.5056  -0.348  0.72761   
summary(h16.max$BTNW_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -3.816      1.015  -3.760  0.00017 ***
#Total.Decid    4.657      2.124   2.192  0.02838 *  
#Hard          11.678      9.932   1.176  0.23969    
#Soft         -13.033     22.281  -0.585  0.55858 
summary(h25.bin$BTNW_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -3.620      0.853  -4.244 2.19e-05 ***
#Total.Decid    3.154      1.696   1.859    0.063 .  
#Hard           5.791      7.771   0.745    0.456    
#Soft         -17.978     17.598  -1.022    0.307   
summary(h25.max$BTNW_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -3.7562     0.8664  -4.335 1.46e-05 ***
#Total.Decid   3.4305     1.6473   2.082   0.0373 *  
#Hard          6.6070     7.5241   0.878   0.3799    
#Soft        -17.4858    16.9504  -1.032   0.3023
summary(h100.bin$BTNW_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.8249     0.7946  -2.297   0.0216 *  
#Total.Decid   8.7222     2.2310   3.910 9.25e-05 ***
#Hard          3.6820    16.0811   0.229   0.8189    
#Soft         -4.7297    35.3317  -0.134   0.8935   
summary(h100.max$BTNW_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.8249     0.7946  -2.297   0.0216 *  
#Total.Decid   8.7222     2.2310   3.910 9.25e-05 ***
#Hard          3.6820    16.0811   0.229   0.8189    
#Soft         -4.7297    35.3317  -0.134   0.8935 
summary(h4.bin$SWTH_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.90996    0.08548  10.645   <2e-16 ***
#Total.Decid  0.33164    0.21610   1.535   0.1249    
#Hard        -1.70906    0.74526  -2.293   0.0218 *  
#Soft        -1.75765    1.91895  -0.916   0.3597   
summary(h4.max$SWTH_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.2033     0.1070  11.244  < 2e-16 ***
#Total.Decid   0.2940     0.2014   1.460 0.144395    
#Hard         -2.4533     0.6966  -3.522 0.000429 ***
#Soft         -0.6927     1.7759  -0.390 0.696502    
summary(h9.bin$SWTH_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.7608     0.0984  17.894   <2e-16 ***
#Total.Decid   0.2347     0.3018   0.778   0.4368    
#Hard         -2.2042     1.0835  -2.034   0.0419 *  
#Soft         -1.0995     2.6625  -0.413   0.6796   
summary(h9.max$SWTH_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  2.09753    0.12441  16.860  < 2e-16 ***
#Total.Decid  0.04126    0.30243   0.136 0.891474    
#Hard        -3.55322    1.00489  -3.536 0.000406 ***
#Soft         0.99057    2.44297   0.405 0.685127    
summary(h16.bin$SWTH_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.3111     0.1024  22.577   <2e-16 ***
#Total.Decid   0.3273     0.3278   0.999    0.318    
#Hard         -1.7243     1.3595  -1.268    0.205    
#Soft         -2.1261     3.2480  -0.655    0.513 
summary(h16.max$SWTH_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   2.6206     0.1344  19.500  < 2e-16 ***
#Total.Decid   0.2634     0.3514   0.750 0.453476    
#Hard         -4.4145     1.2960  -3.406 0.000658 ***
#Soft          2.6556     3.0117   0.882 0.377904  
summary(h25.bin$SWTH_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.7608     0.0984  17.894   <2e-16 ***
#Total.Decid   0.2347     0.3018   0.778   0.4368    
#Hard         -2.2042     1.0835  -2.034   0.0419 *  
#Soft         -1.0995     2.6625  -0.413   0.6796    
summary(h25.max$SWTH_max)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  2.09753    0.12441  16.860  < 2e-16 ***
#Total.Decid  0.04126    0.30243   0.136 0.891474    
#Hard        -3.55322    1.00489  -3.536 0.000406 ***
#Soft         0.99057    2.44297   0.405 0.685127    
summary(h100.bin$SWTH_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   4.0226     0.0860  46.776  < 2e-16 ***
#Total.Decid   0.7977     0.3082   2.588  0.00965 ** 
#Hard          3.9421     2.0147   1.957  0.05039 .  
#Soft        -15.8479     4.9781  -3.184  0.00146 ** 
summary(h100.max$SWTH_bin)
#            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   4.0226     0.0860  46.776  < 2e-16 ***
#Total.Decid   0.7977     0.3082   2.588  0.00965 ** 
#Hard          3.9421     2.0147   1.957  0.05039 .  
#Soft        -15.8479     4.9781  -3.184  0.00146 ** 

#par(mfrow=c(2,3))
par(mfrow=c(1,1))

fits1<-exp(predict(h2.max$OVEN_max, birds.habitatHF))
plot(birds.habitatHF$OVEN_max, fits1)
fits4<-exp(predict(h4.max$OVEN_max, birdshab4plot.summ))
plot(birdshab4plot.summ$OVEN_max, fits4)
fits9<-exp(predict(h9.max$OVEN_max, birdshab9plot.summ))
plot(birdshab9plot.summ$OVEN_max, fits9)
fits16<-exp(predict(h16.max$OVEN_max, birdshab16plot.summ))
plot(birdshab16plot.summ$OVEN_max, fits16)
fits25<-exp(predict(h25.max$OVEN_max, birdshab25plot.summ))
plot(birdshab25plot.summ$OVEN_max, fits25)
fits100<-exp(predict(h100.max$OVEN_max, birdshab100plot.summ))
plot(birdshab100plot.summ$OVEN_max, fits100)