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
  
#Data Exploration
plot(birds.habitatHF$Total.Harvest, birds.habitatHF$CAWA_mean)
plot(birds.habitatHF$Total.Harvest, birds.habitatHF$CAWA_max)
library(lme4)
cor(birds.habitatHF[,c("Total.Conif",
                       "Total.Decid",
                       "Total.Mixwood",
                       "Total.Harvest")])

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
birds.habitatHF$NEAR_DIST_SEISMIC.s<-scale(birds.habitatHF$NEAR_DIST_SEISMIC, scale=T, center=T)
birds.habitatHF$NEAR_DIST_ANYROAD.s<-scale(birds.habitatHF$NEAR_DIST_ANYROAD, scale=T, center=T)
spp.list.max<-c("BRCR_max",
                "BTNW_max",
                "CAWA_max",
                "OVEN_max",
                "SWTH_max",
                "WTSP_max")

h2.max<-list()
for (i in spp.list.max) {
  birds.habitatHF$spp<-birds.habitatHF[,i]
  try(h2.max[[i]]<-glmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+Total.Linear+NEAR_DIST_SEISMIC.s+NEAR_DIST_ANYROAD.s+(1|Grid), family="poisson", data=birds.habitatHF))
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
  try(h2.mean[[i]]<-lmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+Total.Linear+NEAR_DIST_SEISMIC.s+NEAR_DIST_ANYROAD.s+(1|Grid), data=birds.habitatHF))
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
  try(h2.bin[[i]]<-glmer(spp~Total.Decid+Total.Conif+Total.Mixwood+Total.Harvest+Total.Linear+NEAR_DIST_SEISMIC.s+NEAR_DIST_ANYROAD.s+(1|Grid), family="binomial", data=birds.habitatHF))
}

summary(h2.bin$BRCR_bin)
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -2.931451   0.254457 -11.520  < 2e-16 ***
#Total.Decid          2.360203   0.366042   6.448 1.13e-10 ***
#Total.Conif          2.674492   0.926345   2.887  0.00389 ** 
#Total.Mixwood        0.628717   0.590960   1.064  0.28738    
#Total.Harvest       -3.759152   0.936461  -4.014 5.96e-05 ***
#Total.Linear        -3.167643   1.835621  -1.726  0.08441 .  
#NEAR_DIST_SEISMIC.s -0.007034   0.193140  -0.036  0.97095    
#NEAR_DIST_ANYROAD.s  0.054532   0.134595   0.405  0.68536    
summary(h2.mean$BRCR_mean)
#                     Estimate Std. Error t value
# (Intercept)          0.033384   0.012036   2.774
# Total.Decid          0.166845   0.021564   7.737
# Total.Conif          0.151101   0.046890   3.222
# Total.Mixwood        0.035892   0.028095   1.278
# Total.Harvest       -0.172077   0.032674  -5.267
# Total.Linear        -0.154059   0.078528  -1.962
# NEAR_DIST_SEISMIC.s  0.003869   0.010392   0.372
# NEAR_DIST_ANYROAD.s  0.001668   0.007546   0.221

summary(h2.max$BRCR_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -2.88558    0.22133 -13.037  < 2e-16 ***
#Total.Decid          2.03965    0.30774   6.628 3.41e-11 ***
#Total.Conif          2.03073    0.79958   2.540 0.011094 *  
#Total.Mixwood        0.60121    0.52468   1.146 0.251856    
#Total.Harvest       -3.38575    0.88301  -3.834 0.000126 ***
#Total.Linear        -2.74647    1.69561  -1.620 0.105285    
#NEAR_DIST_SEISMIC.s -0.02669    0.15766  -0.169 0.865575    
#NEAR_DIST_ANYROAD.s  0.10268    0.10884   0.943 0.345508 

summary(h2.bin$BTNW_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          -6.5657     0.8713  -7.535 4.88e-14 ***
#Total.Decid           3.0731     0.8498   3.616 0.000299 ***
#Total.Conif           3.7636     2.0946   1.797 0.072363 .  
#Total.Mixwood         2.8959     1.0965   2.641 0.008267 ** 
#Total.Harvest        -3.4360     3.1268  -1.099 0.271825    
#Total.Linear          2.3153     3.0032   0.771 0.440740    
#NEAR_DIST_SEISMIC.s   0.3889     0.4843   0.803 0.422010    
#NEAR_DIST_ANYROAD.s   0.2758     0.2372   1.163 0.244996 
summary(h2.mean$BTNW_mean)
#                      Estimate Std. Error t value
# (Intercept)         -0.0004235  0.0070173  -0.060
# Total.Decid          0.0504377  0.0110606   4.560
# Total.Conif          0.0431608  0.0238577   1.809
# Total.Mixwood        0.0279285  0.0142896   1.954
# Total.Harvest       -0.0308097  0.0166430  -1.851
# Total.Linear         0.0048520  0.0401346   0.121
# NEAR_DIST_SEISMIC.s  0.0089517  0.0061749   1.450
# NEAR_DIST_ANYROAD.s -0.0007946  0.0038419  -0.207
summary(h2.max$BTNW_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          -6.3556     0.8190  -7.760 8.47e-15 ***
#Total.Decid           2.8570     0.7829   3.649 0.000263 ***
#Total.Conif           3.0775     1.9145   1.608 0.107939    
#Total.Mixwood         2.5626     1.0297   2.489 0.012819 *  
#Total.Harvest        -3.2295     3.0279  -1.067 0.286153    
#Total.Linear          1.7348     2.7486   0.631 0.527933    
#NEAR_DIST_SEISMIC.s   0.4213     0.4610   0.914 0.360771    
#NEAR_DIST_ANYROAD.s   0.1592     0.2078   0.766 0.443675 

summary(h2.bin$CAWA_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          -5.3249     0.5539  -9.613  < 2e-16 ***
# Total.Decid           2.4316     0.7618   3.192  0.00141 ** 
# Total.Conif           0.2288     1.6540   0.138  0.88999    
# Total.Mixwood         1.2650     1.1018   1.148  0.25091    
# Total.Harvest        -0.2331     0.8508  -0.274  0.78407    
# Total.Linear          4.1862     2.4427   1.714  0.08657 .  
# NEAR_DIST_SEISMIC.s  -0.7615     0.4480  -1.700  0.08915 .  
# NEAR_DIST_ANYROAD.s   0.6074     0.2579   2.355  0.01851 *  
summary(h2.mean$CAWA_mean)
#                      Estimate Std. Error t value
# (Intercept)          0.004677   0.008078   0.579
# Total.Decid          0.045955   0.012685   3.623
# Total.Conif         -0.024454   0.027357  -0.894
# Total.Mixwood        0.010464   0.016386   0.639
# Total.Harvest       -0.007673   0.019085  -0.402
# Total.Linear         0.062731   0.046027   1.363
# NEAR_DIST_SEISMIC.s -0.008874   0.007112  -1.248
# NEAR_DIST_ANYROAD.s  0.011625   0.004406   2.639
summary(h2.max$CAWA_max)
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -5.12829    0.52266  -9.812  < 2e-16 ***
# Total.Decid          2.42045    0.69463   3.485 0.000493 ***
# Total.Conif          0.08856    1.61502   0.055 0.956269    
# Total.Mixwood        1.09559    1.06046   1.033 0.301546    
# Total.Harvest       -0.57102    0.77029  -0.741 0.458504    
# Total.Linear         2.68387    2.38128   1.127 0.259714    
# NEAR_DIST_SEISMIC.s -0.61298    0.42431  -1.445 0.148562    
# NEAR_DIST_ANYROAD.s  0.42740    0.22781   1.876 0.060637 .

summary(h2.bin$OVEN_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          -0.7991     0.2105  -3.797 0.000147 ***
# Total.Decid           4.3445     0.3718  11.686  < 2e-16 ***
# Total.Conif           2.6301     0.6067   4.335 1.46e-05 ***
# Total.Mixwood         0.8552     0.3409   2.509 0.012119 *  
# Total.Harvest        -1.5516     0.4591  -3.379 0.000726 ***
# Total.Linear          0.6225     0.9915   0.628 0.530068    
# NEAR_DIST_SEISMIC.s   0.4572     0.1936   2.362 0.018180 *  
# NEAR_DIST_ANYROAD.s  -0.1428     0.1053  -1.357 0.174901 
summary(h2.mean$OVEN_mean)
#                     Estimate Std. Error t value
# (Intercept)          0.275626   0.044021   6.261
# Total.Decid          0.926950   0.050593  18.322
# Total.Conif          0.646906   0.108121   5.983
# Total.Mixwood        0.234626   0.064746   3.624
# Total.Harvest       -0.449802   0.075516  -5.956
# Total.Linear         0.087935   0.182816   0.481
# NEAR_DIST_SEISMIC.s  0.056210   0.039505   1.423
# NEAR_DIST_ANYROAD.s  0.004189   0.017426   0.240
summary(h2.max$OVEN_max)
# #                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -0.76142    0.13463  -5.656 1.55e-08 ***
# Total.Decid          1.51652    0.11289  13.434  < 2e-16 ***
# Total.Conif          1.21101    0.25915   4.673 2.97e-06 ***
# Total.Mixwood        0.58197    0.17189   3.386  0.00071 ***
# Total.Harvest       -0.82012    0.17825  -4.601 4.21e-06 ***
# Total.Linear         0.28975    0.46702   0.620  0.53498    
# NEAR_DIST_SEISMIC.s  0.08651    0.11386   0.760  0.44739    
# NEAR_DIST_ANYROAD.s  0.02405    0.03950   0.609  0.54255 

summary(h2.bin$SWTH_bin)
#                    Estimate Std. Error z value Pr(>|z|)   
# (Intercept)          0.29223    0.13968   2.092   0.0364 * 
# Total.Decid          0.68779    0.25768   2.669   0.0076 **
# Total.Conif          1.18206    0.57812   2.045   0.0409 * 
# Total.Mixwood        0.79834    0.34429   2.319   0.0204 * 
# Total.Harvest       -0.11412    0.38549  -0.296   0.7672   
# Total.Linear        -1.89134    0.88431  -2.139   0.0325 * 
# NEAR_DIST_SEISMIC.s  0.32465    0.12921   2.513   0.0120 * 
# NEAR_DIST_ANYROAD.s  0.12336    0.09585   1.287   0.1981 
summary(h2.mean$SWTH_mean)
#                     Estimate Std. Error t value
# (Intercept)          0.461429   0.033966  13.585
# Total.Decid          0.119650   0.048195   2.483
# Total.Conif          0.191454   0.103521   1.849
# Total.Mixwood        0.173777   0.061994   2.803
# Total.Harvest       -0.107900   0.072255  -1.493
# Total.Linear        -0.446192   0.174551  -2.556
# NEAR_DIST_SEISMIC.s  0.113787   0.030212   3.766
# NEAR_DIST_ANYROAD.s  0.001349   0.016676   0.081
summary(h2.max$SWTH_max)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -0.28458    0.07506  -3.791  0.00015 ***
#Total.Decid          0.20657    0.12581   1.642  0.10062    
#Total.Conif          0.37934    0.26784   1.416  0.15669    
#Total.Mixwood        0.36482    0.15218   2.397  0.01652 *  
#Total.Harvest       -0.16162    0.19601  -0.825  0.40963    
#Total.Linear        -1.14965    0.54281  -2.118  0.03418 *  
#NEAR_DIST_SEISMIC.s  0.17096    0.05965   2.866  0.00415 ** 
#NEAR_DIST_ANYROAD.s  0.01656    0.04001   0.414  0.67891 

summary(h2.bin$WTSP_bin)
#                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          0.49214    0.29012   1.696  0.08982 .  
#Total.Decid          2.69180    0.41993   6.410 1.45e-10 ***
#Total.Conif          1.79279    1.11126   1.613  0.10668    
#Total.Mixwood       -0.77394    0.37471  -2.065  0.03888 *  
#Total.Harvest        2.70304    0.90705   2.980  0.00288 ** 
#Total.Linear         4.77294    1.47923   3.227  0.00125 ** 
#NEAR_DIST_SEISMIC.s  0.00526    0.25993   0.020  0.98385    
#NEAR_DIST_ANYROAD.s -0.12899    0.10687  -1.207  0.22746
summary(h2.mean$WTSP_mean)
#                    Estimate Std. Error t value
# (Intercept)          0.60780    0.06762   8.989
# Total.Decid          0.47760    0.05123   9.322
# Total.Conif          0.25295    0.10900   2.321
# Total.Mixwood       -0.13782    0.06533  -2.110
# Total.Harvest        0.31715    0.07618   4.163
# Total.Linear         0.53876    0.18475   2.916
# NEAR_DIST_SEISMIC.s -0.01132    0.05897  -0.192
# NEAR_DIST_ANYROAD.s -0.02504    0.01758  -1.424
summary(h2.max$WTSP_max)
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -0.04671    0.14874  -0.314 0.753483    
# Total.Decid          0.60266    0.09872   6.105 1.03e-09 ***
# Total.Conif          0.27450    0.19152   1.433 0.151777    
# Total.Mixwood       -0.25699    0.15383  -1.671 0.094808 .  
# Total.Harvest        0.43495    0.12853   3.384 0.000714 ***
# Total.Linear         0.63892    0.34571   1.848 0.064586 .  
# NEAR_DIST_SEISMIC.s -0.01101    0.13003  -0.085 0.932508    
# NEAR_DIST_ANYROAD.s -0.07189    0.03700  -1.943 0.052034 .  
