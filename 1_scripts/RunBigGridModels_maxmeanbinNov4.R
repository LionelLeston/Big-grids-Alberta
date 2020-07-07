allgrids.summ2<-read.csv("allgrids.summ.Oct15.csv", header=TRUE)
str(allgrids.summ2)
allgrids.summ2$SS<-allgrids.summ2$StationKey

BigGridhabitatHF<-read.csv("BigGridhabitatHF.csv", header=TRUE)
nrow(BigGridhabitatHF)#4231
BigGridhabitatHF.u<-unique(BigGridhabitatHF)
nrow(BigGridhabitatHF.u)#1233

birds.habitatHF<-merge(allgrids.summ2, BigGridhabitatHF.u, by=c("SS"))
nrow(birds.habitatHF)
write.csv(birds.habitatHF, file="birds.habitatHF.csv")#data check

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

#Basic Habitat Model
spp.list.max<-c("BRCR_max",
            "BTNW_max",
            "CAWA_max",
            "OVEN_max",
            "SWTH_max",
            "WTSP_max")

h1.max<-list()
for (i in spp.list.max) {
  birds.habitatHF$spp<-birds.habitatHF[,i]
  try(h1.max[[i]]<-glmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+(1|Grid), family="poisson", data=birds.habitatHF))
}

spp.list.mean<-c("BRCR_mean",
                "BTNW_mean",
                "CAWA_mean",
                "OVEN_mean",
                "SWTH_mean",
                "WTSP_mean")

h1.mean<-list()
for (i in spp.list.mean) {
  birds.habitatHF$spp<-sqrt(birds.habitatHF[,i])
  try(h1.mean[[i]]<-lmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+(1|Grid), data=birds.habitatHF))
}

spp.list.bin<-c("BRCR_bin",
                 "BTNW_bin",
                 "CAWA_bin",
                 "OVEN_bin",
                 "SWTH_bin",
                 "WTSP_bin")

h1.bin<-list()
for (i in spp.list.bin) {
  birds.habitatHF$spp<-sqrt(birds.habitatHF[,i])
  try(h1.bin[[i]]<-glmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+(1|Grid), family="binomial", data=birds.habitatHF))
}

summary(h1.bin$BRCR_bin)
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -3.0927     0.2382 -12.982  < 2e-16 ***
# Total.Decid     2.4361     0.3640   6.693 2.19e-11 ***
# Total.Conif     2.7932     0.9221   3.029  0.00245 ** 
# Total.Mixwood   0.7219     0.5866   1.231  0.21849    
# Total.Harvest  -3.7596     0.9440  -3.983 6.82e-05 ***

summary(h1.mean$BRCR_mean)
#              Estimate Std. Error t value
# (Intercept)    0.02474    0.01113   2.222
# Total.Decid    0.17057    0.02147   7.946
# Total.Conif    0.15449    0.04678   3.303
# Total.Mixwood  0.03995    0.02786   1.434
# Total.Harvest -0.17062    0.03239  -5.268
summary(h1.max$BRCR_max)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -3.0238     0.2068 -14.619  < 2e-16 ***
#Total.Decid     2.1218     0.3079   6.890 5.57e-12 ***
#Total.Conif     2.1362     0.7987   2.675  0.00748 ** 
#Total.Mixwood   0.6917     0.5229   1.323  0.18591    
#Total.Harvest  -3.4141     0.8920  -3.827  0.00013 ***
  
summary(h1.bin$BTNW_bin)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -6.3099     0.8234  -7.663 1.81e-14 ***
#Total.Decid     2.9432     0.8211   3.584 0.000338 ***
#Total.Conif     3.4792     2.0335   1.711 0.087100 .  
#Total.Mixwood   2.8897     1.0753   2.687 0.007203 ** 
#Total.Harvest  -3.4820     3.0512  -1.141 0.253786   
summary(h1.mean$BTNW_mean)
#                Estimate Std. Error t value
# (Intercept)   -0.0003222  0.0068446  -0.047
# Total.Decid    0.0509079  0.0110236   4.618
# Total.Conif    0.0441595  0.0237830   1.857
# Total.Mixwood  0.0297602  0.0142119   2.094
# Total.Harvest -0.0325649  0.0165441  -1.968

summary(h1.max$BTNW_max)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -6.1853     0.7883  -7.847 4.28e-15 ***
#Total.Decid     2.7818     0.7547   3.686 0.000228 ***
#Total.Conif     2.9689     1.8823   1.577 0.114728    
#Total.Mixwood   2.5697     1.0054   2.556 0.010590 *  
#Total.Harvest  -3.3283     3.0026  -1.108 0.267666 

summary(h1.bin$CAWA_bin)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -4.9832     0.5412  -9.209   <2e-16 ***
#Total.Decid     2.2276     0.7210   3.090    0.002 ** 
#Total.Conif    -0.2663     1.6115  -0.165    0.869    
#Total.Mixwood   1.0709     1.0844   0.988    0.323    
#Total.Harvest  -0.4608     0.8378  -0.550    0.582  
summary(h1.mean$CAWA_mean)
#               Estimate Std. Error t value
# (Intercept)    0.008784   0.007720   1.138
# Total.Decid    0.043496   0.012657   3.436
# Total.Conif   -0.030460   0.027328  -1.115
# Total.Mixwood  0.007450   0.016326   0.456
# Total.Harvest -0.008920   0.019003  -0.469
summary(h1.max$CAWA_max)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -4.9486     0.5237  -9.450  < 2e-16 ***
#Total.Decid     2.2969     0.6590   3.485 0.000491 ***
#Total.Conif    -0.2754     1.5852  -0.174 0.862084    
#Total.Mixwood   0.9765     1.0513   0.929 0.352978    
#Total.Harvest  -0.7307     0.7630  -0.958 0.338178 

summary(h1.bin$OVEN_bin)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -0.7619     0.2275  -3.349 0.000812 ***
#Total.Decid     4.3039     0.3711  11.597  < 2e-16 ***
#Total.Conif     2.6357     0.6069   4.343 1.41e-05 ***
#Total.Mixwood   0.8856     0.3401   2.604 0.009214 ** 
#Total.Harvest  -1.5992     0.4589  -3.485 0.000492 ***
summary(h1.mean$OVEN_mean)
#              Estimate Std. Error t value
# (Intercept)    0.28170    0.04522   6.230
# Total.Decid    0.92376    0.05038  18.334
# Total.Conif    0.64379    0.10774   5.975
# Total.Mixwood  0.23504    0.06456   3.640
# Total.Harvest -0.45754    0.07525  -6.080
summary(h1.max$OVEN_max)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -0.7421     0.1380  -5.379 7.51e-08 ***
#Total.Decid     1.5036     0.1114  13.500  < 2e-16 ***
#Total.Conif     1.1924     0.2578   4.625 3.75e-06 ***
#Total.Mixwood   0.5736     0.1717   3.342 0.000832 ***
#Total.Harvest  -0.8327     0.1778  -4.682 2.84e-06 ***

summary(h1.bin$SWTH_bin)
#              Estimate Std. Error z value Pr(>|z|)   
#(Intercept)     0.2014     0.1861   1.082  0.27925   
#Total.Decid     0.7192     0.2617   2.748  0.00599 **
#Total.Conif     1.0935     0.5749   1.902  0.05715 . 
#Total.Mixwood   0.8488     0.3435   2.471  0.01348 * 
#Total.Harvest  -0.1972     0.3832  -0.515  0.60673  
summary(h1.mean$SWTH_mean)
#              Estimate Std. Error t value
# (Intercept)    0.44041    0.04928   8.936
# Total.Decid    0.12568    0.04862   2.585
# Total.Conif    0.18665    0.10380   1.798
# Total.Mixwood  0.18052    0.06223   2.901
# Total.Harvest -0.11231    0.07255  -1.548
summary(h1.max$SWTH_max)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -0.3328     0.0936  -3.555 0.000378 ***
#Total.Decid     0.2069     0.1263   1.638 0.101355    
#Total.Conif     0.3487     0.2670   1.306 0.191430    
#Total.Mixwood   0.3863     0.1543   2.503 0.012310 *  
#Total.Harvest  -0.2033     0.1961  -1.037 0.299934    

summary(h1.bin$WTSP_bin)
#              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     0.7059     0.2947   2.395  0.01660 *  
#Total.Decid     2.6974     0.4224   6.386 1.71e-10 ***
#Total.Conif     1.6619     1.1054   1.503  0.13272    
#Total.Mixwood  -0.7933     0.3721  -2.132  0.03301 *  
#Total.Harvest   2.7300     0.9183   2.973  0.00295 ** 
summary(h1.mean$WTSP_mean)
#              Estimate Std. Error t value
# (Intercept)    0.63702    0.06802   9.365
# Total.Decid    0.46483    0.05120   9.079
# Total.Conif    0.24890    0.10909   2.282
# Total.Mixwood -0.14273    0.06545  -2.181
# Total.Harvest  0.30503    0.07632   3.997
summary(h1.max$WTSP_max)
#               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -0.006325   0.150260  -0.042 0.966422    
#Total.Decid    0.572730   0.097477   5.876 4.21e-09 ***
#Total.Conif    0.276274   0.190395   1.451 0.146763    
#Total.Mixwood -0.266728   0.152943  -1.744 0.081164 .  
#Total.Harvest  0.420658   0.127816   3.291 0.000998 ***

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
  try(h2.max[[i]]<-glmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+Soft+Hard+NEAR_DIST_SOFT.s+NEAR_DIST_HARD.s+(1|Grid), family="poisson", data=birds.habitatHF))
}

spp.list.mean<-c("BRCR_mean",
                 "BTNW_mean",
                 "CAWA_mean",
                 "OVEN_mean",
                 "SWTH_mean",
                 "WTSP_mean")

h2.mean<-list()
for (i in spp.list.mean) {
  birds.habitatHF$spp<-sqrt(birds.habitatHF[,i])
  try(h2.mean[[i]]<-lmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+Soft+Hard+NEAR_DIST_SOFT.s+NEAR_DIST_HARD.s+(1|Grid), data=birds.habitatHF))
}

spp.list.bin<-c("BRCR_bin",
                "BTNW_bin",
                "CAWA_bin",
                "OVEN_bin",
                "SWTH_bin",
                "WTSP_bin")

h2.bin<-list()
for (i in spp.list.bin) {
  birds.habitatHF$spp<-sqrt(birds.habitatHF[,i])
  try(h2.bin[[i]]<-glmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+Soft+Hard+NEAR_DIST_SOFT.s+NEAR_DIST_HARD.s+(1|Grid), family="binomial", data=birds.habitatHF))
}

summary(h2.bin$BRCR_bin)
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.7545970  0.2683747 -10.264  < 2e-16 ***
#Total.Decid       2.2614100  0.3683534   6.139 8.29e-10 ***
#Total.Conif       2.5699675  0.9342228   2.751  0.00594 ** 
#Total.Mixwood     0.5305905  0.5903127   0.899  0.36874    
#Total.Harvest    -3.6458292  0.9370664  -3.891 1.00e-04 ***
#Soft             -1.9525444  3.1448808  -0.621  0.53469    
#Hard             -6.9276341  2.2478329  -3.082  0.00206 ** 
#NEAR_DIST_SOFT.s -0.0385659  0.2129638  -0.181  0.85630    
#NEAR_DIST_HARD.s -0.0007467  0.1480161  -0.005  0.99597     
summary(h2.mean$BRCR_mean)
#                     Estimate Std. Error t value
#(Intercept)       0.0386489  0.0124983   3.092
# Total.Decid       0.1635502  0.0216122   7.567
# Total.Conif       0.1463966  0.0469032   3.121
# Total.Mixwood     0.0337012  0.0281547   1.197
# Total.Harvest    -0.1698062  0.0328389  -5.171
# Soft             -0.1490012  0.1529127  -0.974
# Hard             -0.1564356  0.0580816  -2.693
# NEAR_DIST_SOFT.s  0.0028575  0.0109842   0.260
# NEAR_DIST_HARD.s -0.0002949  0.0079859  -0.037

summary(h2.max$BRCR_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -2.75865    0.23069 -11.958  < 2e-16 ***
#Total.Decid       1.95902    0.30796   6.361    2e-10 ***
#Total.Conif       1.95589    0.79997   2.445 0.014487 *  
#Total.Mixwood     0.53066    0.52139   1.018 0.308789    
#Total.Harvest    -3.27694    0.87888  -3.729 0.000193 ***
#Soft             -1.60238    2.84141  -0.564 0.572795    
#Hard             -5.28884    1.94705  -2.716 0.006601 ** 
#NEAR_DIST_SOFT.s -0.06811    0.17190  -0.396 0.691960    
#NEAR_DIST_HARD.s  0.09168    0.11952   0.767 0.443067 

###Brown Creeper showed similar significant response to 
#TotalDecid, TotalConif, TotalHarvest, and HardFootprint(local)
#whether it was modeled as binary, max, or mean

summary(h2.bin$BTNW_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -6.4898     0.8895  -7.296 2.96e-13 ***
#Total.Decid        2.9678     0.8524   3.482 0.000499 ***
#Total.Conif        3.6415     2.0899   1.742 0.081438 .  
#Total.Mixwood      2.7226     1.0952   2.486 0.012921 *  
#Total.Harvest     -3.1735     3.0574  -1.038 0.299278    
#Soft               6.8498     4.5091   1.519 0.128739    
#Hard              -3.6342     3.8709  -0.939 0.347798    
#NEAR_DIST_SOFT.s   0.4696     0.5066   0.927 0.353916    
#NEAR_DIST_HARD.s   0.1863     0.2577   0.723 0.469637 
summary(h2.mean$BTNW_mean)
#                      Estimate Std. Error t value
#(Intercept)      -0.001129   0.007146  -0.158
# Total.Decid       0.049300   0.011079   4.450
# Total.Conif       0.042428   0.023869   1.778
# Total.Mixwood     0.025750   0.014318   1.799
# Total.Harvest    -0.027628   0.016721  -1.652
# Soft              0.109109   0.078038   1.398
# Hard             -0.026041   0.029636  -0.879
# NEAR_DIST_SOFT.s  0.011892   0.006358   1.870
# NEAR_DIST_HARD.s -0.003543   0.004073  -0.870
summary(h2.max$BTNW_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -6.32498    0.83562  -7.569 3.76e-14 ***
#Total.Decid       2.79600    0.79216   3.530 0.000416 ***
#Total.Conif       3.00938    1.90999   1.576 0.115118    
#Total.Mixwood     2.43819    1.03005   2.367 0.017930 *  
#Total.Harvest    -3.00682    2.98621  -1.007 0.313981    
#Soft              6.29735    3.92456   1.605 0.108582    
#Hard             -2.93903    3.39305  -0.866 0.386385    
#NEAR_DIST_SOFT.s  0.49967    0.47476   1.052 0.292579    
#NEAR_DIST_HARD.s  0.07909    0.22994   0.344 0.730871 

###Black-throated Green Warblers show similar significant and n.s. 
#reponses to habitat, soft energy footprint and hard energy footprint
#whether modelled as binary, mean or max

summary(h2.bin$CAWA_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -5.2528     0.5628  -9.333  < 2e-16 ***
#Total.Decid        2.4020     0.7644   3.142  0.00168 ** 
#Total.Conif        0.2183     1.6534   0.132  0.89495    
#Total.Mixwood      1.2952     1.1069   1.170  0.24198    
#Total.Harvest     -0.3112     0.8540  -0.364  0.71553    
#Soft               4.5246     4.7002   0.963  0.33573    
#Hard               1.2959     2.0322   0.638  0.52367    
#NEAR_DIST_SOFT.s  -0.7152     0.4617  -1.549  0.12136    
#NEAR_DIST_HARD.s   0.5039     0.2594   1.943  0.05204 . 
summary(h2.mean$CAWA_mean)
#                      Estimate Std. Error t value
#(Intercept)       0.005839   0.008286   0.705
# Total.Decid       0.045225   0.012741   3.549
# Total.Conif      -0.025014   0.027439  -0.912
# Total.Mixwood     0.009831   0.016458   0.597
# Total.Harvest    -0.008501   0.019222  -0.442
# Soft              0.060525   0.089725   0.675
# Hard              0.013769   0.034074   0.404
# NEAR_DIST_SOFT.s -0.008869   0.007378  -1.202
# NEAR_DIST_HARD.s  0.010065   0.004683   2.149
summary(h2.max$CAWA_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -5.1045     0.5360  -9.523  < 2e-16 ***
# Total.Decid        2.4564     0.7012   3.503  0.00046 ***
# Total.Conif        0.0784     1.6182   0.048  0.96136    
# Total.Mixwood      1.1823     1.0696   1.105  0.26900    
# Total.Harvest     -0.6744     0.7760  -0.869  0.38484    
# Soft               1.5823     4.5642   0.347  0.72883    
# Hard               1.3662     1.8261   0.748  0.45436    
# NEAR_DIST_SOFT.s  -0.5977     0.4381  -1.364  0.17249    
# NEAR_DIST_HARD.s   0.3449     0.2319   1.487  0.13698 

###Canada Warbler showed similar significant and n.s. responses 
#when measured as binary or mean. When measured by max, CAWA 
#increased nonsignificantly with distance from hard footprint, though
#Canada Warbler increased significantly when modelled as binary or mean
summary(h2.bin$OVEN_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -0.6791     0.2252  -3.016 0.002564 ** 
#Total.Decid        4.3285     0.3749  11.546  < 2e-16 ***
#Total.Conif        2.5647     0.6106   4.200 2.67e-05 ***
#Total.Mixwood      0.8385     0.3437   2.440 0.014703 *  
#Total.Harvest     -1.6498     0.4638  -3.557 0.000375 ***
#Soft              -2.3119     1.9464  -1.188 0.234916    
#Hard              -0.3364     0.7244  -0.464 0.642316    
#NEAR_DIST_SOFT.s   0.4598     0.2101   2.189 0.028602 *  
#NEAR_DIST_HARD.s  -0.2445     0.1118  -2.187 0.028718 * 
summary(h2.mean$OVEN_mean)
#                     Estimate Std. Error t value
#(Intercept)       0.30232    0.04654   6.496
# Total.Decid       0.91557    0.05077  18.033
# Total.Conif       0.62367    0.10823   5.762
# Total.Mixwood     0.22862    0.06487   3.524
# Total.Harvest    -0.46388    0.07587  -6.114
# Soft             -0.52985    0.35543  -1.491
# Hard             -0.09952    0.13493  -0.738
# NEAR_DIST_SOFT.s  0.05379    0.04188   1.284
# NEAR_DIST_HARD.s -0.01934    0.01853  -1.044
summary(h2.max$OVEN_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.69328    0.14150  -4.900 9.60e-07 ***
#Total.Decid       1.47769    0.11315  13.060  < 2e-16 ***
#Total.Conif       1.14331    0.25968   4.403 1.07e-05 ***
#Total.Mixwood     0.55705    0.17223   3.234  0.00122 ** 
#Total.Harvest    -0.84274    0.17885  -4.712 2.45e-06 ***
#Soft             -1.09667    0.91519  -1.198  0.23080    
#Hard             -0.28528    0.39186  -0.728  0.46660    
#NEAR_DIST_SOFT.s  0.07057    0.12006   0.588  0.55668    
#NEAR_DIST_HARD.s -0.02377    0.04324  -0.550  0.58241  

###Ovenbird increased with Conif, Decid, Mixedwood and decreased
#with Harvest when modelled in all 3 ways. Only responded signi-
#ficantly to energy footprint when modelled as binary distribution

summary(h2.bin$SWTH_bin)
#                    Estimate Std. Error z value Pr(>|z|)   
#(Intercept)       0.40679    0.15677   2.595  0.00947 ** 
#Total.Decid       0.61494    0.26218   2.345  0.01900 *  
#Total.Conif       1.05893    0.58206   1.819  0.06887 .  
#Total.Mixwood     0.73645    0.34736   2.120  0.03399 *  
#Total.Harvest    -0.08895    0.38952  -0.228  0.81936    
#Soft             -1.57211    1.72356  -0.912  0.36170    
#Hard             -2.71239    0.70899  -3.826  0.00013 ***
#NEAR_DIST_SOFT.s  0.30977    0.14718   2.105  0.03531 *  
#NEAR_DIST_HARD.s  0.08064    0.10216   0.789  0.42993  
summary(h2.mean$SWTH_mean)
#                     Estimate Std. Error t value
#(Intercept)       0.481456   0.036637  13.141
# Total.Decid       0.104746   0.048249   2.171
# Total.Conif       0.168146   0.103281   1.628
# Total.Mixwood     0.161884   0.061917   2.615
# Total.Harvest    -0.099417   0.072382  -1.374
# Soft             -0.398927   0.338565  -1.178
# Hard             -0.521610   0.128549  -4.058
# NEAR_DIST_SOFT.s  0.112694   0.032959   3.419
# NEAR_DIST_HARD.s -0.006813   0.017658  -0.386
summary(h2.max$SWTH_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.231927   0.082690  -2.805 0.005035 ** 
#Total.Decid       0.161313   0.126614   1.274 0.202642    
#Total.Conif       0.321944   0.268147   1.201 0.229897    
#Total.Mixwood     0.326353   0.152755   2.136 0.032643 *  
#Total.Harvest    -0.127643   0.197334  -0.647 0.517736    
#Soft             -0.434231   1.009423  -0.430 0.667067    
#Hard             -1.803776   0.466107  -3.870 0.000109 ***
#NEAR_DIST_SOFT.s  0.174083   0.067509   2.579 0.009918 ** 
#NEAR_DIST_HARD.s -0.007982   0.042734  -0.187 0.851836  

###Swainson's Thrush responded to distance to soft footprint and 
#amount of hard footprint in same way in all 3 model formulations

summary(h2.bin$WTSP_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       0.46993    0.29180   1.610  0.10731    
#Total.Decid       2.70521    0.42104   6.425 1.32e-10 ***
#Total.Conif       1.88094    1.12227   1.676  0.09373 .  
#Total.Mixwood    -0.80329    0.37562  -2.139  0.03247 *  
#Total.Harvest     2.80648    0.91725   3.060  0.00222 ** 
#Soft              5.53569    2.66121   2.080  0.03751 *  
#Hard              2.00476    1.08963   1.840  0.06579 .  
#NEAR_DIST_SOFT.s  0.08106    0.26285   0.308  0.75778    
#NEAR_DIST_HARD.s -0.14425    0.11336  -1.272  0.20322 
summary(h2.mean$WTSP_mean)
#                    Estimate Std. Error t value
#(Intercept)       0.607639   0.067822   8.959
# Total.Decid       0.478537   0.051505   9.291
# Total.Conif       0.257670   0.109365   2.356
# Total.Mixwood    -0.138542   0.065581  -2.113
# Total.Harvest     0.310466   0.076676   4.049
# Soft              0.442662   0.359846   1.230
# Hard              0.296757   0.136548   2.173
# NEAR_DIST_SOFT.s -0.008365   0.058781  -0.142
# NEAR_DIST_HARD.s -0.031340   0.018743  -1.672
summary(h2.max$WTSP_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      -0.041527   0.150528  -0.276  0.78264    
# Total.Decid       0.591890   0.099181   5.968 2.41e-09 ***
# Total.Conif       0.269916   0.191921   1.406  0.15961    
# Total.Mixwood    -0.272353   0.154318  -1.765  0.07758 .  
# Total.Harvest     0.435630   0.129246   3.371  0.00075 ***
# Soft              0.882284   0.679103   1.299  0.19388    
# Hard              0.164125   0.259771   0.632  0.52751    
# NEAR_DIST_SOFT.s -0.005882   0.130013  -0.045  0.96392    
# NEAR_DIST_HARD.s -0.088951   0.039757  -2.237  0.02526 * 

###White-throated Sparrow shows similar significant responses to
#deciduous, mixedwood, and harvest under all 3 formulations
#responds positively to hard and soft energy footprint but significant
#variables differ among formulations: for binary it's local hard and
#soft, for mean it's local hard, for max it's distance to nearest hard
#footprint.