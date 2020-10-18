library(dplyr)
library(tidyr)
library(stringr)
#150-m scale data from 2016
vegHF.150.2016<-read.csv("0_data/processed/10_vegHF150mscale_2016_stationgridassignment.csv", header=TRUE)
str(vegHF.150.2016)#1500 obs
nrow(vegHF.150.2016)

#Create unique "landscapes"
vegHF.150.2016$L2x2<-paste0("L-",vegHF.150.2016$Gridnum,"-",vegHF.150.2016$g2x2)
vegHF.150.2016$L3x3<-paste0("L-",vegHF.150.2016$Gridnum,"-",vegHF.150.2016$g3x3)
vegHF.150.2016$L4x4<-paste0("L-",vegHF.150.2016$Gridnum,"-",vegHF.150.2016$g4x4)
vegHF.150.2016$L5x5<-paste0("L-",vegHF.150.2016$Gridnum,"-",vegHF.150.2016$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date

Summ2x2<-vegHF.150.2016%>%
  group_by(L2x2)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ2x2<-data.frame(Summ2x2)
Summ2x2$PropConifer.150.2016<-(Summ2x2$CCPineR_sum+
  Summ2x2$CCPine1_sum+
  Summ2x2$CCPine2_sum+
  Summ2x2$CCPine3_sum+
  Summ2x2$CCPine4_sum+
  Summ2x2$CCSpruceR_sum+
  Summ2x2$CCSpruce1_sum+
  Summ2x2$CCSpruce2_sum+
  Summ2x2$CCSpruce3_sum+
  Summ2x2$CCSpruce4_sum+
  Summ2x2$TreedFenR_sum+
  Summ2x2$TreedFen1_sum+
  Summ2x2$TreedFen2_sum+
  Summ2x2$TreedFen3_sum+
  Summ2x2$TreedFen4_sum+
  Summ2x2$TreedFen5_sum+
  Summ2x2$TreedFen6_sum+
  Summ2x2$TreedFen7_sum+
  Summ2x2$TreedFen8_sum+
  Summ2x2$TreedFen9_sum+
  Summ2x2$TreedBogR_sum+
  Summ2x2$TreedBog1_sum+
  Summ2x2$TreedBog2_sum+
  Summ2x2$TreedBog3_sum+
  Summ2x2$TreedBog4_sum+
  Summ2x2$TreedBog5_sum+
  Summ2x2$TreedBog6_sum+
  Summ2x2$TreedBog7_sum+
  Summ2x2$TreedBog8_sum+
  Summ2x2$TreedBog9_sum+
  Summ2x2$SpruceR_sum+
  Summ2x2$Spruce1_sum+
  Summ2x2$Spruce2_sum+
  Summ2x2$Spruce3_sum+
  Summ2x2$Spruce4_sum+
  Summ2x2$Spruce5_sum+
  Summ2x2$Spruce6_sum+
  Summ2x2$Spruce7_sum+
  Summ2x2$Spruce8_sum+
  Summ2x2$Spruce9_sum+
  Summ2x2$PineR_sum+
  Summ2x2$Pine1_sum+
  Summ2x2$Pine2_sum+
  Summ2x2$Pine3_sum+
  Summ2x2$Pine4_sum+
  Summ2x2$Pine5_sum+
  Summ2x2$Pine6_sum+
  Summ2x2$Pine7_sum+
  Summ2x2$Pine8_sum+
  Summ2x2$Pine9_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$PropWet.150.2016<-(Summ2x2$ShrubbyBog_sum+
  Summ2x2$ShrubbyFen_sum+
  Summ2x2$ShrubbySwamp_sum+
  Summ2x2$Marsh_sum+
  Summ2x2$GraminoidFen_sum+
  Summ2x2$TreedBogR_sum+
  Summ2x2$TreedBog1_sum+
  Summ2x2$TreedBog2_sum+
  Summ2x2$TreedBog3_sum+
  Summ2x2$TreedBog4_sum+
  Summ2x2$TreedBog5_sum+
  Summ2x2$TreedBog6_sum+
  Summ2x2$TreedBog7_sum+
  Summ2x2$TreedBog8_sum+
  Summ2x2$TreedBog9_sum+
  Summ2x2$TreedFenR_sum+
  Summ2x2$TreedFen1_sum+
  Summ2x2$TreedFen2_sum+
  Summ2x2$TreedFen3_sum+
  Summ2x2$TreedFen4_sum+
  Summ2x2$TreedFen5_sum+
  Summ2x2$TreedFen6_sum+
  Summ2x2$TreedFen7_sum+
  Summ2x2$TreedFen8_sum+
  Summ2x2$TreedFen9_sum+
  Summ2x2$TreedSwampR_sum+
  Summ2x2$TreedSwamp1_sum+
  Summ2x2$TreedSwamp2_sum+
  Summ2x2$TreedSwamp3_sum+
  Summ2x2$TreedSwamp4_sum+
  Summ2x2$TreedSwamp5_sum+
  Summ2x2$TreedSwamp6_sum+
  Summ2x2$TreedSwamp7_sum+
  Summ2x2$TreedSwamp8_sum+
  Summ2x2$TreedSwamp9_sum+
  Summ2x2$Water_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points
  
Summ2x2$TotalForest<-Summ2x2$TreedBogR_sum+
  Summ2x2$TreedBog1_sum+
  Summ2x2$TreedBog2_sum+
  Summ2x2$TreedBog3_sum+
  Summ2x2$TreedBog4_sum+
  Summ2x2$TreedBog5_sum+
  Summ2x2$TreedBog6_sum+
  Summ2x2$TreedBog7_sum+
  Summ2x2$TreedBog8_sum+
  Summ2x2$TreedBog9_sum+
  Summ2x2$TreedFenR_sum+
  Summ2x2$TreedFen1_sum+
  Summ2x2$TreedFen2_sum+
  Summ2x2$TreedFen3_sum+
  Summ2x2$TreedFen4_sum+
  Summ2x2$TreedFen5_sum+
  Summ2x2$TreedFen6_sum+
  Summ2x2$TreedFen7_sum+
  Summ2x2$TreedFen8_sum+
  Summ2x2$TreedFen9_sum+
  Summ2x2$TreedSwampR_sum+
  Summ2x2$TreedSwamp1_sum+
  Summ2x2$TreedSwamp2_sum+
  Summ2x2$TreedSwamp3_sum+
  Summ2x2$TreedSwamp4_sum+
  Summ2x2$TreedSwamp5_sum+
  Summ2x2$TreedSwamp6_sum+
  Summ2x2$TreedSwamp7_sum+
  Summ2x2$TreedSwamp8_sum+
  Summ2x2$TreedSwamp9_sum+
  Summ2x2$SpruceR_sum+
  Summ2x2$Spruce1_sum+
  Summ2x2$Spruce2_sum+
  Summ2x2$Spruce3_sum+
  Summ2x2$Spruce4_sum+
  Summ2x2$Spruce5_sum+
  Summ2x2$Spruce6_sum+
  Summ2x2$Spruce7_sum+
  Summ2x2$Spruce8_sum+
  Summ2x2$Spruce9_sum+
  Summ2x2$PineR_sum+
  Summ2x2$Pine1_sum+
  Summ2x2$Pine2_sum+
  Summ2x2$Pine3_sum+
  Summ2x2$Pine4_sum+
  Summ2x2$Pine5_sum+
  Summ2x2$Pine6_sum+
  Summ2x2$Pine7_sum+
  Summ2x2$Pine8_sum+
  Summ2x2$Pine9_sum+
  Summ2x2$DecidR_sum+
  Summ2x2$Decid1_sum+
  Summ2x2$Decid2_sum+
  Summ2x2$Decid3_sum+
  Summ2x2$Decid4_sum+
  Summ2x2$Decid5_sum+
  Summ2x2$Decid6_sum+
  Summ2x2$Decid7_sum+
  Summ2x2$Decid8_sum+
  Summ2x2$Decid9_sum+
  Summ2x2$MixedwoodR_sum+
  Summ2x2$Mixedwood1_sum+
  Summ2x2$Mixedwood2_sum+
  Summ2x2$Mixedwood3_sum+
  Summ2x2$Mixedwood4_sum+
  Summ2x2$Mixedwood5_sum+
  Summ2x2$Mixedwood6_sum+
  Summ2x2$Mixedwood7_sum+
  Summ2x2$Mixedwood8_sum+
  Summ2x2$Mixedwood9_sum+
  Summ2x2$CCPineR_sum+
  Summ2x2$CCPine1_sum+
  Summ2x2$CCPine2_sum+
  Summ2x2$CCPine3_sum+
  Summ2x2$CCPine4_sum+
  Summ2x2$CCSpruceR_sum+
  Summ2x2$CCSpruce1_sum+
  Summ2x2$CCSpruce2_sum+
  Summ2x2$CCSpruce3_sum+
  Summ2x2$CCSpruce4_sum+
  Summ2x2$CCDecidR_sum+
  Summ2x2$CCDecid1_sum+
  Summ2x2$CCDecid2_sum+
  Summ2x2$CCDecid3_sum+
  Summ2x2$CCDecid4_sum+
  Summ2x2$CCMixedwoodR_sum+
  Summ2x2$CCMixedwood1_sum+
  Summ2x2$CCMixedwood2_sum+
  Summ2x2$CCMixedwood3_sum+
  Summ2x2$CCMixedwood4_sum

Summ2x2$Wt.For.Age.150.2016<-(Summ2x2$TreedBogR_sum*4.5+#0-9 years
                                Summ2x2$TreedBog1_sum*10+#10-19 years
                                Summ2x2$TreedBog2_sum*20+#20-39 years
                                Summ2x2$TreedBog3_sum*40+#40-59 years
                                Summ2x2$TreedBog4_sum*60+#60-79 years
                                Summ2x2$TreedBog5_sum*80+#80-99 years
                                Summ2x2$TreedBog6_sum*100+#100-119 years
                                Summ2x2$TreedBog7_sum*120+#120-139 years
                                Summ2x2$TreedBog8_sum*140+#140-159 years
                                Summ2x2$TreedBog9_sum*160+#160 years and older
                                Summ2x2$TreedFenR_sum*4.5+
                                Summ2x2$TreedFen1_sum*10+
                                Summ2x2$TreedFen2_sum*20+
                                Summ2x2$TreedFen3_sum*40+
                                Summ2x2$TreedFen4_sum*60+
                                Summ2x2$TreedFen5_sum*80+
                                Summ2x2$TreedFen6_sum*100+
                                Summ2x2$TreedFen7_sum*120+
                                Summ2x2$TreedFen8_sum*140+
                                Summ2x2$TreedFen9_sum*160+
                                Summ2x2$TreedSwampR_sum*4.5+
                                Summ2x2$TreedSwamp1_sum*10+
                                Summ2x2$TreedSwamp2_sum*20+
                                Summ2x2$TreedSwamp3_sum*40+
                                Summ2x2$TreedSwamp4_sum*60+
                                Summ2x2$TreedSwamp5_sum*80+
                                Summ2x2$TreedSwamp6_sum*100+
                                Summ2x2$TreedSwamp7_sum*120+
                                Summ2x2$TreedSwamp8_sum*140+
                                Summ2x2$TreedSwamp9_sum*160+
                                Summ2x2$SpruceR_sum*4.5+
                                Summ2x2$Spruce1_sum*10+
                                Summ2x2$Spruce2_sum*20+
                                Summ2x2$Spruce3_sum*40+
                                Summ2x2$Spruce4_sum*60+
                                Summ2x2$Spruce5_sum*80+
                                Summ2x2$Spruce6_sum*100+
                                Summ2x2$Spruce7_sum*120+
                                Summ2x2$Spruce8_sum*140+
                                Summ2x2$Spruce9_sum*160+
                                Summ2x2$PineR_sum*4.5+
                                Summ2x2$Pine1_sum*10+
                                Summ2x2$Pine2_sum*20+
                                Summ2x2$Pine3_sum*40+
                                Summ2x2$Pine4_sum*60+
                                Summ2x2$Pine5_sum*80+
                                Summ2x2$Pine6_sum*100+
                                Summ2x2$Pine7_sum*120+
                                Summ2x2$Pine8_sum*140+
                                Summ2x2$Pine9_sum*160+
                                Summ2x2$DecidR_sum*4.5+
                                Summ2x2$Decid1_sum*10+
                                Summ2x2$Decid2_sum*20+
                                Summ2x2$Decid3_sum*40+
                                Summ2x2$Decid4_sum*60+
                                Summ2x2$Decid5_sum*80+
                                Summ2x2$Decid6_sum*100+
                                Summ2x2$Decid7_sum*120+
                                Summ2x2$Decid8_sum*140+
                                Summ2x2$Decid9_sum*160+
                                Summ2x2$MixedwoodR_sum*4.5+
                                Summ2x2$Mixedwood1_sum*10+
                                Summ2x2$Mixedwood2_sum*20+
                                Summ2x2$Mixedwood3_sum*40+
                                Summ2x2$Mixedwood4_sum*60+
                                Summ2x2$Mixedwood5_sum*80+
                                Summ2x2$Mixedwood6_sum*100+
                                Summ2x2$Mixedwood7_sum*120+
                                Summ2x2$Mixedwood8_sum*140+
                                Summ2x2$Mixedwood9_sum*160+
                                Summ2x2$CCPineR_sum*4.5+
                                Summ2x2$CCPine1_sum*10+
                                Summ2x2$CCPine2_sum*20+
                                Summ2x2$CCPine3_sum*40+
                                Summ2x2$CCPine4_sum*60+
                                Summ2x2$CCSpruceR_sum*4.5+
                                Summ2x2$CCSpruce1_sum*10+
                                Summ2x2$CCSpruce2_sum*20+
                                Summ2x2$CCSpruce3_sum*40+
                                Summ2x2$CCSpruce4_sum*60+
                                Summ2x2$CCDecidR_sum*4.5+
                                Summ2x2$CCDecid1_sum*10+
                                Summ2x2$CCDecid2_sum*20+
                                Summ2x2$CCDecid3_sum*40+
                                Summ2x2$CCDecid4_sum*60+
                                Summ2x2$CCMixedwoodR_sum*4.5+
                                Summ2x2$CCMixedwood1_sum*10+
                                Summ2x2$CCMixedwood2_sum*20+
                                Summ2x2$CCMixedwood3_sum*40+
                                Summ2x2$CCMixedwood4_sum*60)/Summ2x2$TotalForest  

Summ2x2$PropSoftLinear.150.2016<-(Summ2x2$SeismicLineNarrow_sum+
  Summ2x2$SeismicLineWide_sum+
  Summ2x2$Pipeline_sum+
  Summ2x2$TransmissionLine_sum+
  Summ2x2$RoadTrailVegetated_sum+
  Summ2x2$RoadVegetatedVerge_sum+
  Summ2x2$RailVegetatedVerge_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$PropHardLinear.150.2016<-(Summ2x2$RoadHardSurface_sum+
                                    Summ2x2$RailHardSurface_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$PropSoftPoly.150.2016<-(Summ2x2$OtherDisturbedVegetation_sum+
                                    Summ2x2$WellSite_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$PropHardPoly.150.2016<-(Summ2x2$CultivationCrop_sum+
                                  Summ2x2$CultivationAbandoned_sum+
                                  Summ2x2$CultivationRoughPasture_sum+
                                  Summ2x2$CultivationTamePasture_sum+
                                  Summ2x2$HighDensityLivestockOperation_sum+
                                  Summ2x2$BorrowpitsDugoutsSumps_sum+
                                  Summ2x2$MunicipalWaterSewage_sum+
                                  Summ2x2$Reservoirs_sum+
                                  Summ2x2$Canals_sum+
                                  Summ2x2$UrbanIndustrial_sum+
                                  Summ2x2$UrbanResidence_sum+
                                  Summ2x2$RuralResidentialIndustrial_sum+
                                  Summ2x2$IndustrialSiteRural_sum+
                                  Summ2x2$WindGenerationFacility_sum+
                                  Summ2x2$MineSite_sum+
                                  Summ2x2$PeatMine_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ2x2$zerolndscp<-str_sub(Summ2x2$L2x2, -2, -1)#distinguishes '0' from '10', '20'
Summ2x2<-Summ2x2[!Summ2x2$zerolndscp=="-0",]
write.csv(Summ2x2, file="0_data/processed/different scales/vegHF.150.2016.2x2.csv")

Summ3x3<-vegHF.150.2016%>%
  group_by(L3x3)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ3x3<-data.frame(Summ3x3)
Summ3x3$PropConifer.150.2016<-(Summ3x3$CCPineR_sum+
                                 Summ3x3$CCPine1_sum+
                                 Summ3x3$CCPine2_sum+
                                 Summ3x3$CCPine3_sum+
                                 Summ3x3$CCPine4_sum+
                                 Summ3x3$CCSpruceR_sum+
                                 Summ3x3$CCSpruce1_sum+
                                 Summ3x3$CCSpruce2_sum+
                                 Summ3x3$CCSpruce3_sum+
                                 Summ3x3$CCSpruce4_sum+
                                 Summ3x3$TreedFenR_sum+
                                 Summ3x3$TreedFen1_sum+
                                 Summ3x3$TreedFen2_sum+
                                 Summ3x3$TreedFen3_sum+
                                 Summ3x3$TreedFen4_sum+
                                 Summ3x3$TreedFen5_sum+
                                 Summ3x3$TreedFen6_sum+
                                 Summ3x3$TreedFen7_sum+
                                 Summ3x3$TreedFen8_sum+
                                 Summ3x3$TreedFen9_sum+
                                 Summ3x3$TreedBogR_sum+
                                 Summ3x3$TreedBog1_sum+
                                 Summ3x3$TreedBog2_sum+
                                 Summ3x3$TreedBog3_sum+
                                 Summ3x3$TreedBog4_sum+
                                 Summ3x3$TreedBog5_sum+
                                 Summ3x3$TreedBog6_sum+
                                 Summ3x3$TreedBog7_sum+
                                 Summ3x3$TreedBog8_sum+
                                 Summ3x3$TreedBog9_sum+
                                 Summ3x3$SpruceR_sum+
                                 Summ3x3$Spruce1_sum+
                                 Summ3x3$Spruce2_sum+
                                 Summ3x3$Spruce3_sum+
                                 Summ3x3$Spruce4_sum+
                                 Summ3x3$Spruce5_sum+
                                 Summ3x3$Spruce6_sum+
                                 Summ3x3$Spruce7_sum+
                                 Summ3x3$Spruce8_sum+
                                 Summ3x3$Spruce9_sum+
                                 Summ3x3$PineR_sum+
                                 Summ3x3$Pine1_sum+
                                 Summ3x3$Pine2_sum+
                                 Summ3x3$Pine3_sum+
                                 Summ3x3$Pine4_sum+
                                 Summ3x3$Pine5_sum+
                                 Summ3x3$Pine6_sum+
                                 Summ3x3$Pine7_sum+
                                 Summ3x3$Pine8_sum+
                                 Summ3x3$Pine9_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$PropWet.150.2016<-(Summ3x3$ShrubbyBog_sum+
                             Summ3x3$ShrubbyFen_sum+
                             Summ3x3$ShrubbySwamp_sum+
                             Summ3x3$Marsh_sum+
                             Summ3x3$GraminoidFen_sum+
                             Summ3x3$TreedBogR_sum+
                             Summ3x3$TreedBog1_sum+
                             Summ3x3$TreedBog2_sum+
                             Summ3x3$TreedBog3_sum+
                             Summ3x3$TreedBog4_sum+
                             Summ3x3$TreedBog5_sum+
                             Summ3x3$TreedBog6_sum+
                             Summ3x3$TreedBog7_sum+
                             Summ3x3$TreedBog8_sum+
                             Summ3x3$TreedBog9_sum+
                             Summ3x3$TreedFenR_sum+
                             Summ3x3$TreedFen1_sum+
                             Summ3x3$TreedFen2_sum+
                             Summ3x3$TreedFen3_sum+
                             Summ3x3$TreedFen4_sum+
                             Summ3x3$TreedFen5_sum+
                             Summ3x3$TreedFen6_sum+
                             Summ3x3$TreedFen7_sum+
                             Summ3x3$TreedFen8_sum+
                             Summ3x3$TreedFen9_sum+
                             Summ3x3$TreedSwampR_sum+
                             Summ3x3$TreedSwamp1_sum+
                             Summ3x3$TreedSwamp2_sum+
                             Summ3x3$TreedSwamp3_sum+
                             Summ3x3$TreedSwamp4_sum+
                             Summ3x3$TreedSwamp5_sum+
                             Summ3x3$TreedSwamp6_sum+
                             Summ3x3$TreedSwamp7_sum+
                             Summ3x3$TreedSwamp8_sum+
                             Summ3x3$TreedSwamp9_sum+
                             Summ3x3$Water_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$TotalForest<-Summ3x3$TreedBogR_sum+
  Summ3x3$TreedBog1_sum+
  Summ3x3$TreedBog2_sum+
  Summ3x3$TreedBog3_sum+
  Summ3x3$TreedBog4_sum+
  Summ3x3$TreedBog5_sum+
  Summ3x3$TreedBog6_sum+
  Summ3x3$TreedBog7_sum+
  Summ3x3$TreedBog8_sum+
  Summ3x3$TreedBog9_sum+
  Summ3x3$TreedFenR_sum+
  Summ3x3$TreedFen1_sum+
  Summ3x3$TreedFen2_sum+
  Summ3x3$TreedFen3_sum+
  Summ3x3$TreedFen4_sum+
  Summ3x3$TreedFen5_sum+
  Summ3x3$TreedFen6_sum+
  Summ3x3$TreedFen7_sum+
  Summ3x3$TreedFen8_sum+
  Summ3x3$TreedFen9_sum+
  Summ3x3$TreedSwampR_sum+
  Summ3x3$TreedSwamp1_sum+
  Summ3x3$TreedSwamp2_sum+
  Summ3x3$TreedSwamp3_sum+
  Summ3x3$TreedSwamp4_sum+
  Summ3x3$TreedSwamp5_sum+
  Summ3x3$TreedSwamp6_sum+
  Summ3x3$TreedSwamp7_sum+
  Summ3x3$TreedSwamp8_sum+
  Summ3x3$TreedSwamp9_sum+
  Summ3x3$SpruceR_sum+
  Summ3x3$Spruce1_sum+
  Summ3x3$Spruce2_sum+
  Summ3x3$Spruce3_sum+
  Summ3x3$Spruce4_sum+
  Summ3x3$Spruce5_sum+
  Summ3x3$Spruce6_sum+
  Summ3x3$Spruce7_sum+
  Summ3x3$Spruce8_sum+
  Summ3x3$Spruce9_sum+
  Summ3x3$PineR_sum+
  Summ3x3$Pine1_sum+
  Summ3x3$Pine2_sum+
  Summ3x3$Pine3_sum+
  Summ3x3$Pine4_sum+
  Summ3x3$Pine5_sum+
  Summ3x3$Pine6_sum+
  Summ3x3$Pine7_sum+
  Summ3x3$Pine8_sum+
  Summ3x3$Pine9_sum+
  Summ3x3$DecidR_sum+
  Summ3x3$Decid1_sum+
  Summ3x3$Decid2_sum+
  Summ3x3$Decid3_sum+
  Summ3x3$Decid4_sum+
  Summ3x3$Decid5_sum+
  Summ3x3$Decid6_sum+
  Summ3x3$Decid7_sum+
  Summ3x3$Decid8_sum+
  Summ3x3$Decid9_sum+
  Summ3x3$MixedwoodR_sum+
  Summ3x3$Mixedwood1_sum+
  Summ3x3$Mixedwood2_sum+
  Summ3x3$Mixedwood3_sum+
  Summ3x3$Mixedwood4_sum+
  Summ3x3$Mixedwood5_sum+
  Summ3x3$Mixedwood6_sum+
  Summ3x3$Mixedwood7_sum+
  Summ3x3$Mixedwood8_sum+
  Summ3x3$Mixedwood9_sum+
  Summ3x3$CCPineR_sum+
  Summ3x3$CCPine1_sum+
  Summ3x3$CCPine2_sum+
  Summ3x3$CCPine3_sum+
  Summ3x3$CCPine4_sum+
  Summ3x3$CCSpruceR_sum+
  Summ3x3$CCSpruce1_sum+
  Summ3x3$CCSpruce2_sum+
  Summ3x3$CCSpruce3_sum+
  Summ3x3$CCSpruce4_sum+
  Summ3x3$CCDecidR_sum+
  Summ3x3$CCDecid1_sum+
  Summ3x3$CCDecid2_sum+
  Summ3x3$CCDecid3_sum+
  Summ3x3$CCDecid4_sum+
  Summ3x3$CCMixedwoodR_sum+
  Summ3x3$CCMixedwood1_sum+
  Summ3x3$CCMixedwood2_sum+
  Summ3x3$CCMixedwood3_sum+
  Summ3x3$CCMixedwood4_sum

Summ3x3$Wt.For.Age.150.2016<-(Summ3x3$TreedBogR_sum*4.5+#0-9 years
                                Summ3x3$TreedBog1_sum*10+#10-19 years
                                Summ3x3$TreedBog2_sum*20+#20-39 years
                                Summ3x3$TreedBog3_sum*40+#40-59 years
                                Summ3x3$TreedBog4_sum*60+#60-79 years
                                Summ3x3$TreedBog5_sum*80+#80-99 years
                                Summ3x3$TreedBog6_sum*100+#100-119 years
                                Summ3x3$TreedBog7_sum*120+#120-139 years
                                Summ3x3$TreedBog8_sum*140+#140-159 years
                                Summ3x3$TreedBog9_sum*160+#160 years and older
                                Summ3x3$TreedFenR_sum*4.5+
                                Summ3x3$TreedFen1_sum*10+
                                Summ3x3$TreedFen2_sum*20+
                                Summ3x3$TreedFen3_sum*40+
                                Summ3x3$TreedFen4_sum*60+
                                Summ3x3$TreedFen5_sum*80+
                                Summ3x3$TreedFen6_sum*100+
                                Summ3x3$TreedFen7_sum*120+
                                Summ3x3$TreedFen8_sum*140+
                                Summ3x3$TreedFen9_sum*160+
                                Summ3x3$TreedSwampR_sum*4.5+
                                Summ3x3$TreedSwamp1_sum*10+
                                Summ3x3$TreedSwamp2_sum*20+
                                Summ3x3$TreedSwamp3_sum*40+
                                Summ3x3$TreedSwamp4_sum*60+
                                Summ3x3$TreedSwamp5_sum*80+
                                Summ3x3$TreedSwamp6_sum*100+
                                Summ3x3$TreedSwamp7_sum*120+
                                Summ3x3$TreedSwamp8_sum*140+
                                Summ3x3$TreedSwamp9_sum*160+
                                Summ3x3$SpruceR_sum*4.5+
                                Summ3x3$Spruce1_sum*10+
                                Summ3x3$Spruce2_sum*20+
                                Summ3x3$Spruce3_sum*40+
                                Summ3x3$Spruce4_sum*60+
                                Summ3x3$Spruce5_sum*80+
                                Summ3x3$Spruce6_sum*100+
                                Summ3x3$Spruce7_sum*120+
                                Summ3x3$Spruce8_sum*140+
                                Summ3x3$Spruce9_sum*160+
                                Summ3x3$PineR_sum*4.5+
                                Summ3x3$Pine1_sum*10+
                                Summ3x3$Pine2_sum*20+
                                Summ3x3$Pine3_sum*40+
                                Summ3x3$Pine4_sum*60+
                                Summ3x3$Pine5_sum*80+
                                Summ3x3$Pine6_sum*100+
                                Summ3x3$Pine7_sum*120+
                                Summ3x3$Pine8_sum*140+
                                Summ3x3$Pine9_sum*160+
                                Summ3x3$DecidR_sum*4.5+
                                Summ3x3$Decid1_sum*10+
                                Summ3x3$Decid2_sum*20+
                                Summ3x3$Decid3_sum*40+
                                Summ3x3$Decid4_sum*60+
                                Summ3x3$Decid5_sum*80+
                                Summ3x3$Decid6_sum*100+
                                Summ3x3$Decid7_sum*120+
                                Summ3x3$Decid8_sum*140+
                                Summ3x3$Decid9_sum*160+
                                Summ3x3$MixedwoodR_sum*4.5+
                                Summ3x3$Mixedwood1_sum*10+
                                Summ3x3$Mixedwood2_sum*20+
                                Summ3x3$Mixedwood3_sum*40+
                                Summ3x3$Mixedwood4_sum*60+
                                Summ3x3$Mixedwood5_sum*80+
                                Summ3x3$Mixedwood6_sum*100+
                                Summ3x3$Mixedwood7_sum*120+
                                Summ3x3$Mixedwood8_sum*140+
                                Summ3x3$Mixedwood9_sum*160+
                                Summ3x3$CCPineR_sum*4.5+
                                Summ3x3$CCPine1_sum*10+
                                Summ3x3$CCPine2_sum*20+
                                Summ3x3$CCPine3_sum*40+
                                Summ3x3$CCPine4_sum*60+
                                Summ3x3$CCSpruceR_sum*4.5+
                                Summ3x3$CCSpruce1_sum*10+
                                Summ3x3$CCSpruce2_sum*20+
                                Summ3x3$CCSpruce3_sum*40+
                                Summ3x3$CCSpruce4_sum*60+
                                Summ3x3$CCDecidR_sum*4.5+
                                Summ3x3$CCDecid1_sum*10+
                                Summ3x3$CCDecid2_sum*20+
                                Summ3x3$CCDecid3_sum*40+
                                Summ3x3$CCDecid4_sum*60+
                                Summ3x3$CCMixedwoodR_sum*4.5+
                                Summ3x3$CCMixedwood1_sum*10+
                                Summ3x3$CCMixedwood2_sum*20+
                                Summ3x3$CCMixedwood3_sum*40+
                                Summ3x3$CCMixedwood4_sum*60)/Summ3x3$TotalForest  

Summ3x3$PropSoftLinear.150.2016<-(Summ3x3$SeismicLineNarrow_sum+
                                    Summ3x3$SeismicLineWide_sum+
                                    Summ3x3$Pipeline_sum+
                                    Summ3x3$TransmissionLine_sum+
                                    Summ3x3$RoadTrailVegetated_sum+
                                    Summ3x3$RoadVegetatedVerge_sum+
                                    Summ3x3$RailVegetatedVerge_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$PropHardLinear.150.2016<-(Summ3x3$RoadHardSurface_sum+
                                    Summ3x3$RailHardSurface_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$PropSoftPoly.150.2016<-(Summ3x3$OtherDisturbedVegetation_sum+
                                  Summ3x3$WellSite_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$PropHardPoly.150.2016<-(Summ3x3$CultivationCrop_sum+
                                  Summ3x3$CultivationAbandoned_sum+
                                  Summ3x3$CultivationRoughPasture_sum+
                                  Summ3x3$CultivationTamePasture_sum+
                                  Summ3x3$HighDensityLivestockOperation_sum+
                                  Summ3x3$BorrowpitsDugoutsSumps_sum+
                                  Summ3x3$MunicipalWaterSewage_sum+
                                  Summ3x3$Reservoirs_sum+
                                  Summ3x3$Canals_sum+
                                  Summ3x3$UrbanIndustrial_sum+
                                  Summ3x3$UrbanResidence_sum+
                                  Summ3x3$RuralResidentialIndustrial_sum+
                                  Summ3x3$IndustrialSiteRural_sum+
                                  Summ3x3$WindGenerationFacility_sum+
                                  Summ3x3$MineSite_sum+
                                  Summ3x3$PeatMine_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ3x3$zerolndscp<-str_sub(Summ3x3$L3x3, -2, -1)#distinguishes '0' from '10', '20'
Summ3x3<-Summ3x3[!Summ3x3$zerolndscp=="-0",]
write.csv(Summ3x3, file="0_data/processed/different scales/vegHF.150.2016.3x3.csv")

Summ4x4<-vegHF.150.2016%>%
  group_by(L4x4)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ4x4<-data.frame(Summ4x4)
Summ4x4$PropConifer.150.2016<-(Summ4x4$CCPineR_sum+
                                 Summ4x4$CCPine1_sum+
                                 Summ4x4$CCPine2_sum+
                                 Summ4x4$CCPine3_sum+
                                 Summ4x4$CCPine4_sum+
                                 Summ4x4$CCSpruceR_sum+
                                 Summ4x4$CCSpruce1_sum+
                                 Summ4x4$CCSpruce2_sum+
                                 Summ4x4$CCSpruce3_sum+
                                 Summ4x4$CCSpruce4_sum+
                                 Summ4x4$TreedFenR_sum+
                                 Summ4x4$TreedFen1_sum+
                                 Summ4x4$TreedFen2_sum+
                                 Summ4x4$TreedFen3_sum+
                                 Summ4x4$TreedFen4_sum+
                                 Summ4x4$TreedFen5_sum+
                                 Summ4x4$TreedFen6_sum+
                                 Summ4x4$TreedFen7_sum+
                                 Summ4x4$TreedFen8_sum+
                                 Summ4x4$TreedFen9_sum+
                                 Summ4x4$TreedBogR_sum+
                                 Summ4x4$TreedBog1_sum+
                                 Summ4x4$TreedBog2_sum+
                                 Summ4x4$TreedBog3_sum+
                                 Summ4x4$TreedBog4_sum+
                                 Summ4x4$TreedBog5_sum+
                                 Summ4x4$TreedBog6_sum+
                                 Summ4x4$TreedBog7_sum+
                                 Summ4x4$TreedBog8_sum+
                                 Summ4x4$TreedBog9_sum+
                                 Summ4x4$SpruceR_sum+
                                 Summ4x4$Spruce1_sum+
                                 Summ4x4$Spruce2_sum+
                                 Summ4x4$Spruce3_sum+
                                 Summ4x4$Spruce4_sum+
                                 Summ4x4$Spruce5_sum+
                                 Summ4x4$Spruce6_sum+
                                 Summ4x4$Spruce7_sum+
                                 Summ4x4$Spruce8_sum+
                                 Summ4x4$Spruce9_sum+
                                 Summ4x4$PineR_sum+
                                 Summ4x4$Pine1_sum+
                                 Summ4x4$Pine2_sum+
                                 Summ4x4$Pine3_sum+
                                 Summ4x4$Pine4_sum+
                                 Summ4x4$Pine5_sum+
                                 Summ4x4$Pine6_sum+
                                 Summ4x4$Pine7_sum+
                                 Summ4x4$Pine8_sum+
                                 Summ4x4$Pine9_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$PropWet.150.2016<-(Summ4x4$ShrubbyBog_sum+
                             Summ4x4$ShrubbyFen_sum+
                             Summ4x4$ShrubbySwamp_sum+
                             Summ4x4$Marsh_sum+
                             Summ4x4$GraminoidFen_sum+
                             Summ4x4$TreedBogR_sum+
                             Summ4x4$TreedBog1_sum+
                             Summ4x4$TreedBog2_sum+
                             Summ4x4$TreedBog3_sum+
                             Summ4x4$TreedBog4_sum+
                             Summ4x4$TreedBog5_sum+
                             Summ4x4$TreedBog6_sum+
                             Summ4x4$TreedBog7_sum+
                             Summ4x4$TreedBog8_sum+
                             Summ4x4$TreedBog9_sum+
                             Summ4x4$TreedFenR_sum+
                             Summ4x4$TreedFen1_sum+
                             Summ4x4$TreedFen2_sum+
                             Summ4x4$TreedFen3_sum+
                             Summ4x4$TreedFen4_sum+
                             Summ4x4$TreedFen5_sum+
                             Summ4x4$TreedFen6_sum+
                             Summ4x4$TreedFen7_sum+
                             Summ4x4$TreedFen8_sum+
                             Summ4x4$TreedFen9_sum+
                             Summ4x4$TreedSwampR_sum+
                             Summ4x4$TreedSwamp1_sum+
                             Summ4x4$TreedSwamp2_sum+
                             Summ4x4$TreedSwamp3_sum+
                             Summ4x4$TreedSwamp4_sum+
                             Summ4x4$TreedSwamp5_sum+
                             Summ4x4$TreedSwamp6_sum+
                             Summ4x4$TreedSwamp7_sum+
                             Summ4x4$TreedSwamp8_sum+
                             Summ4x4$TreedSwamp9_sum+
                             Summ4x4$Water_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$TotalForest<-Summ4x4$TreedBogR_sum+
  Summ4x4$TreedBog1_sum+
  Summ4x4$TreedBog2_sum+
  Summ4x4$TreedBog3_sum+
  Summ4x4$TreedBog4_sum+
  Summ4x4$TreedBog5_sum+
  Summ4x4$TreedBog6_sum+
  Summ4x4$TreedBog7_sum+
  Summ4x4$TreedBog8_sum+
  Summ4x4$TreedBog9_sum+
  Summ4x4$TreedFenR_sum+
  Summ4x4$TreedFen1_sum+
  Summ4x4$TreedFen2_sum+
  Summ4x4$TreedFen3_sum+
  Summ4x4$TreedFen4_sum+
  Summ4x4$TreedFen5_sum+
  Summ4x4$TreedFen6_sum+
  Summ4x4$TreedFen7_sum+
  Summ4x4$TreedFen8_sum+
  Summ4x4$TreedFen9_sum+
  Summ4x4$TreedSwampR_sum+
  Summ4x4$TreedSwamp1_sum+
  Summ4x4$TreedSwamp2_sum+
  Summ4x4$TreedSwamp3_sum+
  Summ4x4$TreedSwamp4_sum+
  Summ4x4$TreedSwamp5_sum+
  Summ4x4$TreedSwamp6_sum+
  Summ4x4$TreedSwamp7_sum+
  Summ4x4$TreedSwamp8_sum+
  Summ4x4$TreedSwamp9_sum+
  Summ4x4$SpruceR_sum+
  Summ4x4$Spruce1_sum+
  Summ4x4$Spruce2_sum+
  Summ4x4$Spruce3_sum+
  Summ4x4$Spruce4_sum+
  Summ4x4$Spruce5_sum+
  Summ4x4$Spruce6_sum+
  Summ4x4$Spruce7_sum+
  Summ4x4$Spruce8_sum+
  Summ4x4$Spruce9_sum+
  Summ4x4$PineR_sum+
  Summ4x4$Pine1_sum+
  Summ4x4$Pine2_sum+
  Summ4x4$Pine3_sum+
  Summ4x4$Pine4_sum+
  Summ4x4$Pine5_sum+
  Summ4x4$Pine6_sum+
  Summ4x4$Pine7_sum+
  Summ4x4$Pine8_sum+
  Summ4x4$Pine9_sum+
  Summ4x4$DecidR_sum+
  Summ4x4$Decid1_sum+
  Summ4x4$Decid2_sum+
  Summ4x4$Decid3_sum+
  Summ4x4$Decid4_sum+
  Summ4x4$Decid5_sum+
  Summ4x4$Decid6_sum+
  Summ4x4$Decid7_sum+
  Summ4x4$Decid8_sum+
  Summ4x4$Decid9_sum+
  Summ4x4$MixedwoodR_sum+
  Summ4x4$Mixedwood1_sum+
  Summ4x4$Mixedwood2_sum+
  Summ4x4$Mixedwood3_sum+
  Summ4x4$Mixedwood4_sum+
  Summ4x4$Mixedwood5_sum+
  Summ4x4$Mixedwood6_sum+
  Summ4x4$Mixedwood7_sum+
  Summ4x4$Mixedwood8_sum+
  Summ4x4$Mixedwood9_sum+
  Summ4x4$CCPineR_sum+
  Summ4x4$CCPine1_sum+
  Summ4x4$CCPine2_sum+
  Summ4x4$CCPine3_sum+
  Summ4x4$CCPine4_sum+
  Summ4x4$CCSpruceR_sum+
  Summ4x4$CCSpruce1_sum+
  Summ4x4$CCSpruce2_sum+
  Summ4x4$CCSpruce3_sum+
  Summ4x4$CCSpruce4_sum+
  Summ4x4$CCDecidR_sum+
  Summ4x4$CCDecid1_sum+
  Summ4x4$CCDecid2_sum+
  Summ4x4$CCDecid3_sum+
  Summ4x4$CCDecid4_sum+
  Summ4x4$CCMixedwoodR_sum+
  Summ4x4$CCMixedwood1_sum+
  Summ4x4$CCMixedwood2_sum+
  Summ4x4$CCMixedwood3_sum+
  Summ4x4$CCMixedwood4_sum

Summ4x4$Wt.For.Age.150.2016<-(Summ4x4$TreedBogR_sum*4.5+#0-9 years
                                Summ4x4$TreedBog1_sum*10+#10-19 years
                                Summ4x4$TreedBog2_sum*20+#20-39 years
                                Summ4x4$TreedBog3_sum*40+#40-59 years
                                Summ4x4$TreedBog4_sum*60+#60-79 years
                                Summ4x4$TreedBog5_sum*80+#80-99 years
                                Summ4x4$TreedBog6_sum*100+#100-119 years
                                Summ4x4$TreedBog7_sum*120+#120-139 years
                                Summ4x4$TreedBog8_sum*140+#140-159 years
                                Summ4x4$TreedBog9_sum*160+#160 years and older
                                Summ4x4$TreedFenR_sum*4.5+
                                Summ4x4$TreedFen1_sum*10+
                                Summ4x4$TreedFen2_sum*20+
                                Summ4x4$TreedFen3_sum*40+
                                Summ4x4$TreedFen4_sum*60+
                                Summ4x4$TreedFen5_sum*80+
                                Summ4x4$TreedFen6_sum*100+
                                Summ4x4$TreedFen7_sum*120+
                                Summ4x4$TreedFen8_sum*140+
                                Summ4x4$TreedFen9_sum*160+
                                Summ4x4$TreedSwampR_sum*4.5+
                                Summ4x4$TreedSwamp1_sum*10+
                                Summ4x4$TreedSwamp2_sum*20+
                                Summ4x4$TreedSwamp3_sum*40+
                                Summ4x4$TreedSwamp4_sum*60+
                                Summ4x4$TreedSwamp5_sum*80+
                                Summ4x4$TreedSwamp6_sum*100+
                                Summ4x4$TreedSwamp7_sum*120+
                                Summ4x4$TreedSwamp8_sum*140+
                                Summ4x4$TreedSwamp9_sum*160+
                                Summ4x4$SpruceR_sum*4.5+
                                Summ4x4$Spruce1_sum*10+
                                Summ4x4$Spruce2_sum*20+
                                Summ4x4$Spruce3_sum*40+
                                Summ4x4$Spruce4_sum*60+
                                Summ4x4$Spruce5_sum*80+
                                Summ4x4$Spruce6_sum*100+
                                Summ4x4$Spruce7_sum*120+
                                Summ4x4$Spruce8_sum*140+
                                Summ4x4$Spruce9_sum*160+
                                Summ4x4$PineR_sum*4.5+
                                Summ4x4$Pine1_sum*10+
                                Summ4x4$Pine2_sum*20+
                                Summ4x4$Pine3_sum*40+
                                Summ4x4$Pine4_sum*60+
                                Summ4x4$Pine5_sum*80+
                                Summ4x4$Pine6_sum*100+
                                Summ4x4$Pine7_sum*120+
                                Summ4x4$Pine8_sum*140+
                                Summ4x4$Pine9_sum*160+
                                Summ4x4$DecidR_sum*4.5+
                                Summ4x4$Decid1_sum*10+
                                Summ4x4$Decid2_sum*20+
                                Summ4x4$Decid3_sum*40+
                                Summ4x4$Decid4_sum*60+
                                Summ4x4$Decid5_sum*80+
                                Summ4x4$Decid6_sum*100+
                                Summ4x4$Decid7_sum*120+
                                Summ4x4$Decid8_sum*140+
                                Summ4x4$Decid9_sum*160+
                                Summ4x4$MixedwoodR_sum*4.5+
                                Summ4x4$Mixedwood1_sum*10+
                                Summ4x4$Mixedwood2_sum*20+
                                Summ4x4$Mixedwood3_sum*40+
                                Summ4x4$Mixedwood4_sum*60+
                                Summ4x4$Mixedwood5_sum*80+
                                Summ4x4$Mixedwood6_sum*100+
                                Summ4x4$Mixedwood7_sum*120+
                                Summ4x4$Mixedwood8_sum*140+
                                Summ4x4$Mixedwood9_sum*160+
                                Summ4x4$CCPineR_sum*4.5+
                                Summ4x4$CCPine1_sum*10+
                                Summ4x4$CCPine2_sum*20+
                                Summ4x4$CCPine3_sum*40+
                                Summ4x4$CCPine4_sum*60+
                                Summ4x4$CCSpruceR_sum*4.5+
                                Summ4x4$CCSpruce1_sum*10+
                                Summ4x4$CCSpruce2_sum*20+
                                Summ4x4$CCSpruce3_sum*40+
                                Summ4x4$CCSpruce4_sum*60+
                                Summ4x4$CCDecidR_sum*4.5+
                                Summ4x4$CCDecid1_sum*10+
                                Summ4x4$CCDecid2_sum*20+
                                Summ4x4$CCDecid3_sum*40+
                                Summ4x4$CCDecid4_sum*60+
                                Summ4x4$CCMixedwoodR_sum*4.5+
                                Summ4x4$CCMixedwood1_sum*10+
                                Summ4x4$CCMixedwood2_sum*20+
                                Summ4x4$CCMixedwood3_sum*40+
                                Summ4x4$CCMixedwood4_sum*60)/Summ4x4$TotalForest  

Summ4x4$PropSoftLinear.150.2016<-(Summ4x4$SeismicLineNarrow_sum+
                                    Summ4x4$SeismicLineWide_sum+
                                    Summ4x4$Pipeline_sum+
                                    Summ4x4$TransmissionLine_sum+
                                    Summ4x4$RoadTrailVegetated_sum+
                                    Summ4x4$RoadVegetatedVerge_sum+
                                    Summ4x4$RailVegetatedVerge_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$PropHardLinear.150.2016<-(Summ4x4$RoadHardSurface_sum+
                                    Summ4x4$RailHardSurface_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$PropSoftPoly.150.2016<-(Summ4x4$OtherDisturbedVegetation_sum+
                                  Summ4x4$WellSite_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$PropHardPoly.150.2016<-(Summ4x4$CultivationCrop_sum+
                                  Summ4x4$CultivationAbandoned_sum+
                                  Summ4x4$CultivationRoughPasture_sum+
                                  Summ4x4$CultivationTamePasture_sum+
                                  Summ4x4$HighDensityLivestockOperation_sum+
                                  Summ4x4$BorrowpitsDugoutsSumps_sum+
                                  Summ4x4$MunicipalWaterSewage_sum+
                                  Summ4x4$Reservoirs_sum+
                                  Summ4x4$Canals_sum+
                                  Summ4x4$UrbanIndustrial_sum+
                                  Summ4x4$UrbanResidence_sum+
                                  Summ4x4$RuralResidentialIndustrial_sum+
                                  Summ4x4$IndustrialSiteRural_sum+
                                  Summ4x4$WindGenerationFacility_sum+
                                  Summ4x4$MineSite_sum+
                                  Summ4x4$PeatMine_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ4x4$zerolndscp<-str_sub(Summ4x4$L4x4, -2, -1)#distinguishes '0' from '10', '20'
Summ4x4<-Summ4x4[!Summ4x4$zerolndscp=="-0",]
write.csv(Summ4x4, file="0_data/processed/different scales/vegHF.150.2016.4x4.csv")

Summ5x5<-vegHF.150.2016%>%
  group_by(L5x5)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ5x5<-data.frame(Summ5x5)
Summ5x5$PropConifer.150.2016<-(Summ5x5$CCPineR_sum+
                                 Summ5x5$CCPine1_sum+
                                 Summ5x5$CCPine2_sum+
                                 Summ5x5$CCPine3_sum+
                                 Summ5x5$CCPine4_sum+
                                 Summ5x5$CCSpruceR_sum+
                                 Summ5x5$CCSpruce1_sum+
                                 Summ5x5$CCSpruce2_sum+
                                 Summ5x5$CCSpruce3_sum+
                                 Summ5x5$CCSpruce4_sum+
                                 Summ5x5$TreedFenR_sum+
                                 Summ5x5$TreedFen1_sum+
                                 Summ5x5$TreedFen2_sum+
                                 Summ5x5$TreedFen3_sum+
                                 Summ5x5$TreedFen4_sum+
                                 Summ5x5$TreedFen5_sum+
                                 Summ5x5$TreedFen6_sum+
                                 Summ5x5$TreedFen7_sum+
                                 Summ5x5$TreedFen8_sum+
                                 Summ5x5$TreedFen9_sum+
                                 Summ5x5$TreedBogR_sum+
                                 Summ5x5$TreedBog1_sum+
                                 Summ5x5$TreedBog2_sum+
                                 Summ5x5$TreedBog3_sum+
                                 Summ5x5$TreedBog4_sum+
                                 Summ5x5$TreedBog5_sum+
                                 Summ5x5$TreedBog6_sum+
                                 Summ5x5$TreedBog7_sum+
                                 Summ5x5$TreedBog8_sum+
                                 Summ5x5$TreedBog9_sum+
                                 Summ5x5$SpruceR_sum+
                                 Summ5x5$Spruce1_sum+
                                 Summ5x5$Spruce2_sum+
                                 Summ5x5$Spruce3_sum+
                                 Summ5x5$Spruce4_sum+
                                 Summ5x5$Spruce5_sum+
                                 Summ5x5$Spruce6_sum+
                                 Summ5x5$Spruce7_sum+
                                 Summ5x5$Spruce8_sum+
                                 Summ5x5$Spruce9_sum+
                                 Summ5x5$PineR_sum+
                                 Summ5x5$Pine1_sum+
                                 Summ5x5$Pine2_sum+
                                 Summ5x5$Pine3_sum+
                                 Summ5x5$Pine4_sum+
                                 Summ5x5$Pine5_sum+
                                 Summ5x5$Pine6_sum+
                                 Summ5x5$Pine7_sum+
                                 Summ5x5$Pine8_sum+
                                 Summ5x5$Pine9_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$PropWet.150.2016<-(Summ5x5$ShrubbyBog_sum+
                             Summ5x5$ShrubbyFen_sum+
                             Summ5x5$ShrubbySwamp_sum+
                             Summ5x5$Marsh_sum+
                             Summ5x5$GraminoidFen_sum+
                             Summ5x5$TreedBogR_sum+
                             Summ5x5$TreedBog1_sum+
                             Summ5x5$TreedBog2_sum+
                             Summ5x5$TreedBog3_sum+
                             Summ5x5$TreedBog4_sum+
                             Summ5x5$TreedBog5_sum+
                             Summ5x5$TreedBog6_sum+
                             Summ5x5$TreedBog7_sum+
                             Summ5x5$TreedBog8_sum+
                             Summ5x5$TreedBog9_sum+
                             Summ5x5$TreedFenR_sum+
                             Summ5x5$TreedFen1_sum+
                             Summ5x5$TreedFen2_sum+
                             Summ5x5$TreedFen3_sum+
                             Summ5x5$TreedFen4_sum+
                             Summ5x5$TreedFen5_sum+
                             Summ5x5$TreedFen6_sum+
                             Summ5x5$TreedFen7_sum+
                             Summ5x5$TreedFen8_sum+
                             Summ5x5$TreedFen9_sum+
                             Summ5x5$TreedSwampR_sum+
                             Summ5x5$TreedSwamp1_sum+
                             Summ5x5$TreedSwamp2_sum+
                             Summ5x5$TreedSwamp3_sum+
                             Summ5x5$TreedSwamp4_sum+
                             Summ5x5$TreedSwamp5_sum+
                             Summ5x5$TreedSwamp6_sum+
                             Summ5x5$TreedSwamp7_sum+
                             Summ5x5$TreedSwamp8_sum+
                             Summ5x5$TreedSwamp9_sum+
                             Summ5x5$Water_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$TotalForest<-Summ5x5$TreedBogR_sum+
  Summ5x5$TreedBog1_sum+
  Summ5x5$TreedBog2_sum+
  Summ5x5$TreedBog3_sum+
  Summ5x5$TreedBog4_sum+
  Summ5x5$TreedBog5_sum+
  Summ5x5$TreedBog6_sum+
  Summ5x5$TreedBog7_sum+
  Summ5x5$TreedBog8_sum+
  Summ5x5$TreedBog9_sum+
  Summ5x5$TreedFenR_sum+
  Summ5x5$TreedFen1_sum+
  Summ5x5$TreedFen2_sum+
  Summ5x5$TreedFen3_sum+
  Summ5x5$TreedFen4_sum+
  Summ5x5$TreedFen5_sum+
  Summ5x5$TreedFen6_sum+
  Summ5x5$TreedFen7_sum+
  Summ5x5$TreedFen8_sum+
  Summ5x5$TreedFen9_sum+
  Summ5x5$TreedSwampR_sum+
  Summ5x5$TreedSwamp1_sum+
  Summ5x5$TreedSwamp2_sum+
  Summ5x5$TreedSwamp3_sum+
  Summ5x5$TreedSwamp4_sum+
  Summ5x5$TreedSwamp5_sum+
  Summ5x5$TreedSwamp6_sum+
  Summ5x5$TreedSwamp7_sum+
  Summ5x5$TreedSwamp8_sum+
  Summ5x5$TreedSwamp9_sum+
  Summ5x5$SpruceR_sum+
  Summ5x5$Spruce1_sum+
  Summ5x5$Spruce2_sum+
  Summ5x5$Spruce3_sum+
  Summ5x5$Spruce4_sum+
  Summ5x5$Spruce5_sum+
  Summ5x5$Spruce6_sum+
  Summ5x5$Spruce7_sum+
  Summ5x5$Spruce8_sum+
  Summ5x5$Spruce9_sum+
  Summ5x5$PineR_sum+
  Summ5x5$Pine1_sum+
  Summ5x5$Pine2_sum+
  Summ5x5$Pine3_sum+
  Summ5x5$Pine4_sum+
  Summ5x5$Pine5_sum+
  Summ5x5$Pine6_sum+
  Summ5x5$Pine7_sum+
  Summ5x5$Pine8_sum+
  Summ5x5$Pine9_sum+
  Summ5x5$DecidR_sum+
  Summ5x5$Decid1_sum+
  Summ5x5$Decid2_sum+
  Summ5x5$Decid3_sum+
  Summ5x5$Decid4_sum+
  Summ5x5$Decid5_sum+
  Summ5x5$Decid6_sum+
  Summ5x5$Decid7_sum+
  Summ5x5$Decid8_sum+
  Summ5x5$Decid9_sum+
  Summ5x5$MixedwoodR_sum+
  Summ5x5$Mixedwood1_sum+
  Summ5x5$Mixedwood2_sum+
  Summ5x5$Mixedwood3_sum+
  Summ5x5$Mixedwood4_sum+
  Summ5x5$Mixedwood5_sum+
  Summ5x5$Mixedwood6_sum+
  Summ5x5$Mixedwood7_sum+
  Summ5x5$Mixedwood8_sum+
  Summ5x5$Mixedwood9_sum+
  Summ5x5$CCPineR_sum+
  Summ5x5$CCPine1_sum+
  Summ5x5$CCPine2_sum+
  Summ5x5$CCPine3_sum+
  Summ5x5$CCPine4_sum+
  Summ5x5$CCSpruceR_sum+
  Summ5x5$CCSpruce1_sum+
  Summ5x5$CCSpruce2_sum+
  Summ5x5$CCSpruce3_sum+
  Summ5x5$CCSpruce4_sum+
  Summ5x5$CCDecidR_sum+
  Summ5x5$CCDecid1_sum+
  Summ5x5$CCDecid2_sum+
  Summ5x5$CCDecid3_sum+
  Summ5x5$CCDecid4_sum+
  Summ5x5$CCMixedwoodR_sum+
  Summ5x5$CCMixedwood1_sum+
  Summ5x5$CCMixedwood2_sum+
  Summ5x5$CCMixedwood3_sum+
  Summ5x5$CCMixedwood4_sum

Summ5x5$Wt.For.Age.150.2016<-(Summ5x5$TreedBogR_sum*4.5+#0-9 years
                                Summ5x5$TreedBog1_sum*10+#10-19 years
                                Summ5x5$TreedBog2_sum*20+#20-39 years
                                Summ5x5$TreedBog3_sum*40+#40-59 years
                                Summ5x5$TreedBog4_sum*60+#60-79 years
                                Summ5x5$TreedBog5_sum*80+#80-99 years
                                Summ5x5$TreedBog6_sum*100+#100-119 years
                                Summ5x5$TreedBog7_sum*120+#120-139 years
                                Summ5x5$TreedBog8_sum*140+#140-159 years
                                Summ5x5$TreedBog9_sum*160+#160 years and older
                                Summ5x5$TreedFenR_sum*4.5+
                                Summ5x5$TreedFen1_sum*10+
                                Summ5x5$TreedFen2_sum*20+
                                Summ5x5$TreedFen3_sum*40+
                                Summ5x5$TreedFen4_sum*60+
                                Summ5x5$TreedFen5_sum*80+
                                Summ5x5$TreedFen6_sum*100+
                                Summ5x5$TreedFen7_sum*120+
                                Summ5x5$TreedFen8_sum*140+
                                Summ5x5$TreedFen9_sum*160+
                                Summ5x5$TreedSwampR_sum*4.5+
                                Summ5x5$TreedSwamp1_sum*10+
                                Summ5x5$TreedSwamp2_sum*20+
                                Summ5x5$TreedSwamp3_sum*40+
                                Summ5x5$TreedSwamp4_sum*60+
                                Summ5x5$TreedSwamp5_sum*80+
                                Summ5x5$TreedSwamp6_sum*100+
                                Summ5x5$TreedSwamp7_sum*120+
                                Summ5x5$TreedSwamp8_sum*140+
                                Summ5x5$TreedSwamp9_sum*160+
                                Summ5x5$SpruceR_sum*4.5+
                                Summ5x5$Spruce1_sum*10+
                                Summ5x5$Spruce2_sum*20+
                                Summ5x5$Spruce3_sum*40+
                                Summ5x5$Spruce4_sum*60+
                                Summ5x5$Spruce5_sum*80+
                                Summ5x5$Spruce6_sum*100+
                                Summ5x5$Spruce7_sum*120+
                                Summ5x5$Spruce8_sum*140+
                                Summ5x5$Spruce9_sum*160+
                                Summ5x5$PineR_sum*4.5+
                                Summ5x5$Pine1_sum*10+
                                Summ5x5$Pine2_sum*20+
                                Summ5x5$Pine3_sum*40+
                                Summ5x5$Pine4_sum*60+
                                Summ5x5$Pine5_sum*80+
                                Summ5x5$Pine6_sum*100+
                                Summ5x5$Pine7_sum*120+
                                Summ5x5$Pine8_sum*140+
                                Summ5x5$Pine9_sum*160+
                                Summ5x5$DecidR_sum*4.5+
                                Summ5x5$Decid1_sum*10+
                                Summ5x5$Decid2_sum*20+
                                Summ5x5$Decid3_sum*40+
                                Summ5x5$Decid4_sum*60+
                                Summ5x5$Decid5_sum*80+
                                Summ5x5$Decid6_sum*100+
                                Summ5x5$Decid7_sum*120+
                                Summ5x5$Decid8_sum*140+
                                Summ5x5$Decid9_sum*160+
                                Summ5x5$MixedwoodR_sum*4.5+
                                Summ5x5$Mixedwood1_sum*10+
                                Summ5x5$Mixedwood2_sum*20+
                                Summ5x5$Mixedwood3_sum*40+
                                Summ5x5$Mixedwood4_sum*60+
                                Summ5x5$Mixedwood5_sum*80+
                                Summ5x5$Mixedwood6_sum*100+
                                Summ5x5$Mixedwood7_sum*120+
                                Summ5x5$Mixedwood8_sum*140+
                                Summ5x5$Mixedwood9_sum*160+
                                Summ5x5$CCPineR_sum*4.5+
                                Summ5x5$CCPine1_sum*10+
                                Summ5x5$CCPine2_sum*20+
                                Summ5x5$CCPine3_sum*40+
                                Summ5x5$CCPine4_sum*60+
                                Summ5x5$CCSpruceR_sum*4.5+
                                Summ5x5$CCSpruce1_sum*10+
                                Summ5x5$CCSpruce2_sum*20+
                                Summ5x5$CCSpruce3_sum*40+
                                Summ5x5$CCSpruce4_sum*60+
                                Summ5x5$CCDecidR_sum*4.5+
                                Summ5x5$CCDecid1_sum*10+
                                Summ5x5$CCDecid2_sum*20+
                                Summ5x5$CCDecid3_sum*40+
                                Summ5x5$CCDecid4_sum*60+
                                Summ5x5$CCMixedwoodR_sum*4.5+
                                Summ5x5$CCMixedwood1_sum*10+
                                Summ5x5$CCMixedwood2_sum*20+
                                Summ5x5$CCMixedwood3_sum*40+
                                Summ5x5$CCMixedwood4_sum*60)/Summ5x5$TotalForest  

Summ5x5$PropSoftLinear.150.2016<-(Summ5x5$SeismicLineNarrow_sum+
                                    Summ5x5$SeismicLineWide_sum+
                                    Summ5x5$Pipeline_sum+
                                    Summ5x5$TransmissionLine_sum+
                                    Summ5x5$RoadTrailVegetated_sum+
                                    Summ5x5$RoadVegetatedVerge_sum+
                                    Summ5x5$RailVegetatedVerge_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$PropHardLinear.150.2016<-(Summ5x5$RoadHardSurface_sum+
                                    Summ5x5$RailHardSurface_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$PropSoftPoly.150.2016<-(Summ5x5$OtherDisturbedVegetation_sum+
                                  Summ5x5$WellSite_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$PropHardPoly.150.2016<-(Summ5x5$CultivationCrop_sum+
                                  Summ5x5$CultivationAbandoned_sum+
                                  Summ5x5$CultivationRoughPasture_sum+
                                  Summ5x5$CultivationTamePasture_sum+
                                  Summ5x5$HighDensityLivestockOperation_sum+
                                  Summ5x5$BorrowpitsDugoutsSumps_sum+
                                  Summ5x5$MunicipalWaterSewage_sum+
                                  Summ5x5$Reservoirs_sum+
                                  Summ5x5$Canals_sum+
                                  Summ5x5$UrbanIndustrial_sum+
                                  Summ5x5$UrbanResidence_sum+
                                  Summ5x5$RuralResidentialIndustrial_sum+
                                  Summ5x5$IndustrialSiteRural_sum+
                                  Summ5x5$WindGenerationFacility_sum+
                                  Summ5x5$MineSite_sum+
                                  Summ5x5$PeatMine_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ5x5$zerolndscp<-str_sub(Summ5x5$L5x5, -2, -1)#distinguishes '0' from '10', '20'
Summ5x5<-Summ5x5[!Summ5x5$zerolndscp=="-0",]
write.csv(Summ5x5, file="0_data/processed/different scales/vegHF.150.2016.5x5.csv")

#150-m scale data from 2017
vegHF.150.2017<-read.csv("0_data/processed/10_vegHF150mscale_2017_stationgridassignment.csv", header=TRUE)
str(vegHF.150.2017)#1500 obs

#Create unique "landscapes"
vegHF.150.2017$L2x2<-paste0("L-",vegHF.150.2017$Gridnum,"-",vegHF.150.2017$g2x2)
vegHF.150.2017$L3x3<-paste0("L-",vegHF.150.2017$Gridnum,"-",vegHF.150.2017$g3x3)
vegHF.150.2017$L4x4<-paste0("L-",vegHF.150.2017$Gridnum,"-",vegHF.150.2017$g4x4)
vegHF.150.2017$L5x5<-paste0("L-",vegHF.150.2017$Gridnum,"-",vegHF.150.2017$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date


Summ2x2<-vegHF.150.2017%>%
  group_by(L2x2)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ2x2<-data.frame(Summ2x2)
Summ2x2$PropConifer.150.2017<-(Summ2x2$CCPineR_sum+
                                 Summ2x2$CCPine1_sum+
                                 Summ2x2$CCPine2_sum+
                                 Summ2x2$CCPine3_sum+
                                 Summ2x2$CCPine4_sum+
                                 Summ2x2$CCSpruceR_sum+
                                 Summ2x2$CCSpruce1_sum+
                                 Summ2x2$CCSpruce2_sum+
                                 Summ2x2$CCSpruce3_sum+
                                 Summ2x2$CCSpruce4_sum+
                                 Summ2x2$TreedFenR_sum+
                                 Summ2x2$TreedFen1_sum+
                                 Summ2x2$TreedFen2_sum+
                                 Summ2x2$TreedFen3_sum+
                                 Summ2x2$TreedFen4_sum+
                                 Summ2x2$TreedFen5_sum+
                                 Summ2x2$TreedFen6_sum+
                                 Summ2x2$TreedFen7_sum+
                                 Summ2x2$TreedFen8_sum+
                                 Summ2x2$TreedFen9_sum+
                                 Summ2x2$TreedBogR_sum+
                                 Summ2x2$TreedBog1_sum+
                                 Summ2x2$TreedBog2_sum+
                                 Summ2x2$TreedBog3_sum+
                                 Summ2x2$TreedBog4_sum+
                                 Summ2x2$TreedBog5_sum+
                                 Summ2x2$TreedBog6_sum+
                                 Summ2x2$TreedBog7_sum+
                                 Summ2x2$TreedBog8_sum+
                                 Summ2x2$TreedBog9_sum+
                                 Summ2x2$SpruceR_sum+
                                 Summ2x2$Spruce1_sum+
                                 Summ2x2$Spruce2_sum+
                                 Summ2x2$Spruce3_sum+
                                 Summ2x2$Spruce4_sum+
                                 Summ2x2$Spruce5_sum+
                                 Summ2x2$Spruce6_sum+
                                 Summ2x2$Spruce7_sum+
                                 Summ2x2$Spruce8_sum+
                                 Summ2x2$Spruce9_sum+
                                 Summ2x2$PineR_sum+
                                 Summ2x2$Pine1_sum+
                                 Summ2x2$Pine2_sum+
                                 Summ2x2$Pine3_sum+
                                 Summ2x2$Pine4_sum+
                                 Summ2x2$Pine5_sum+
                                 Summ2x2$Pine6_sum+
                                 Summ2x2$Pine7_sum+
                                 Summ2x2$Pine8_sum+
                                 Summ2x2$Pine9_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$PropWet.150.2017<-(Summ2x2$ShrubbyBog_sum+
                             Summ2x2$ShrubbyFen_sum+
                             Summ2x2$ShrubbySwamp_sum+
                             Summ2x2$Marsh_sum+
                             Summ2x2$GraminoidFen_sum+
                             Summ2x2$TreedBogR_sum+
                             Summ2x2$TreedBog1_sum+
                             Summ2x2$TreedBog2_sum+
                             Summ2x2$TreedBog3_sum+
                             Summ2x2$TreedBog4_sum+
                             Summ2x2$TreedBog5_sum+
                             Summ2x2$TreedBog6_sum+
                             Summ2x2$TreedBog7_sum+
                             Summ2x2$TreedBog8_sum+
                             Summ2x2$TreedBog9_sum+
                             Summ2x2$TreedFenR_sum+
                             Summ2x2$TreedFen1_sum+
                             Summ2x2$TreedFen2_sum+
                             Summ2x2$TreedFen3_sum+
                             Summ2x2$TreedFen4_sum+
                             Summ2x2$TreedFen5_sum+
                             Summ2x2$TreedFen6_sum+
                             Summ2x2$TreedFen7_sum+
                             Summ2x2$TreedFen8_sum+
                             Summ2x2$TreedFen9_sum+
                             Summ2x2$TreedSwampR_sum+
                             Summ2x2$TreedSwamp1_sum+
                             Summ2x2$TreedSwamp2_sum+
                             Summ2x2$TreedSwamp3_sum+
                             Summ2x2$TreedSwamp4_sum+
                             Summ2x2$TreedSwamp5_sum+
                             Summ2x2$TreedSwamp6_sum+
                             Summ2x2$TreedSwamp7_sum+
                             Summ2x2$TreedSwamp8_sum+
                             Summ2x2$TreedSwamp9_sum+
                             Summ2x2$Water_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$TotalForest<-Summ2x2$TreedBogR_sum+
  Summ2x2$TreedBog1_sum+
  Summ2x2$TreedBog2_sum+
  Summ2x2$TreedBog3_sum+
  Summ2x2$TreedBog4_sum+
  Summ2x2$TreedBog5_sum+
  Summ2x2$TreedBog6_sum+
  Summ2x2$TreedBog7_sum+
  Summ2x2$TreedBog8_sum+
  Summ2x2$TreedBog9_sum+
  Summ2x2$TreedFenR_sum+
  Summ2x2$TreedFen1_sum+
  Summ2x2$TreedFen2_sum+
  Summ2x2$TreedFen3_sum+
  Summ2x2$TreedFen4_sum+
  Summ2x2$TreedFen5_sum+
  Summ2x2$TreedFen6_sum+
  Summ2x2$TreedFen7_sum+
  Summ2x2$TreedFen8_sum+
  Summ2x2$TreedFen9_sum+
  Summ2x2$TreedSwampR_sum+
  Summ2x2$TreedSwamp1_sum+
  Summ2x2$TreedSwamp2_sum+
  Summ2x2$TreedSwamp3_sum+
  Summ2x2$TreedSwamp4_sum+
  Summ2x2$TreedSwamp5_sum+
  Summ2x2$TreedSwamp6_sum+
  Summ2x2$TreedSwamp7_sum+
  Summ2x2$TreedSwamp8_sum+
  Summ2x2$TreedSwamp9_sum+
  Summ2x2$SpruceR_sum+
  Summ2x2$Spruce1_sum+
  Summ2x2$Spruce2_sum+
  Summ2x2$Spruce3_sum+
  Summ2x2$Spruce4_sum+
  Summ2x2$Spruce5_sum+
  Summ2x2$Spruce6_sum+
  Summ2x2$Spruce7_sum+
  Summ2x2$Spruce8_sum+
  Summ2x2$Spruce9_sum+
  Summ2x2$PineR_sum+
  Summ2x2$Pine1_sum+
  Summ2x2$Pine2_sum+
  Summ2x2$Pine3_sum+
  Summ2x2$Pine4_sum+
  Summ2x2$Pine5_sum+
  Summ2x2$Pine6_sum+
  Summ2x2$Pine7_sum+
  Summ2x2$Pine8_sum+
  Summ2x2$Pine9_sum+
  Summ2x2$DecidR_sum+
  Summ2x2$Decid1_sum+
  Summ2x2$Decid2_sum+
  Summ2x2$Decid3_sum+
  Summ2x2$Decid4_sum+
  Summ2x2$Decid5_sum+
  Summ2x2$Decid6_sum+
  Summ2x2$Decid7_sum+
  Summ2x2$Decid8_sum+
  Summ2x2$Decid9_sum+
  Summ2x2$MixedwoodR_sum+
  Summ2x2$Mixedwood1_sum+
  Summ2x2$Mixedwood2_sum+
  Summ2x2$Mixedwood3_sum+
  Summ2x2$Mixedwood4_sum+
  Summ2x2$Mixedwood5_sum+
  Summ2x2$Mixedwood6_sum+
  Summ2x2$Mixedwood7_sum+
  Summ2x2$Mixedwood8_sum+
  Summ2x2$Mixedwood9_sum+
  Summ2x2$CCPineR_sum+
  Summ2x2$CCPine1_sum+
  Summ2x2$CCPine2_sum+
  Summ2x2$CCPine3_sum+
  Summ2x2$CCPine4_sum+
  Summ2x2$CCSpruceR_sum+
  Summ2x2$CCSpruce1_sum+
  Summ2x2$CCSpruce2_sum+
  Summ2x2$CCSpruce3_sum+
  Summ2x2$CCSpruce4_sum+
  Summ2x2$CCDecidR_sum+
  Summ2x2$CCDecid1_sum+
  Summ2x2$CCDecid2_sum+
  Summ2x2$CCDecid3_sum+
  Summ2x2$CCDecid4_sum+
  Summ2x2$CCMixedwoodR_sum+
  Summ2x2$CCMixedwood1_sum+
  Summ2x2$CCMixedwood2_sum+
  Summ2x2$CCMixedwood3_sum+
  Summ2x2$CCMixedwood4_sum

Summ2x2$Wt.For.Age.150.2017<-(Summ2x2$TreedBogR_sum*4.5+#0-9 years
                                Summ2x2$TreedBog1_sum*10+#10-19 years
                                Summ2x2$TreedBog2_sum*20+#20-39 years
                                Summ2x2$TreedBog3_sum*40+#40-59 years
                                Summ2x2$TreedBog4_sum*60+#60-79 years
                                Summ2x2$TreedBog5_sum*80+#80-99 years
                                Summ2x2$TreedBog6_sum*100+#100-119 years
                                Summ2x2$TreedBog7_sum*120+#120-139 years
                                Summ2x2$TreedBog8_sum*140+#140-159 years
                                Summ2x2$TreedBog9_sum*160+#160 years and older
                                Summ2x2$TreedFenR_sum*4.5+
                                Summ2x2$TreedFen1_sum*10+
                                Summ2x2$TreedFen2_sum*20+
                                Summ2x2$TreedFen3_sum*40+
                                Summ2x2$TreedFen4_sum*60+
                                Summ2x2$TreedFen5_sum*80+
                                Summ2x2$TreedFen6_sum*100+
                                Summ2x2$TreedFen7_sum*120+
                                Summ2x2$TreedFen8_sum*140+
                                Summ2x2$TreedFen9_sum*160+
                                Summ2x2$TreedSwampR_sum*4.5+
                                Summ2x2$TreedSwamp1_sum*10+
                                Summ2x2$TreedSwamp2_sum*20+
                                Summ2x2$TreedSwamp3_sum*40+
                                Summ2x2$TreedSwamp4_sum*60+
                                Summ2x2$TreedSwamp5_sum*80+
                                Summ2x2$TreedSwamp6_sum*100+
                                Summ2x2$TreedSwamp7_sum*120+
                                Summ2x2$TreedSwamp8_sum*140+
                                Summ2x2$TreedSwamp9_sum*160+
                                Summ2x2$SpruceR_sum*4.5+
                                Summ2x2$Spruce1_sum*10+
                                Summ2x2$Spruce2_sum*20+
                                Summ2x2$Spruce3_sum*40+
                                Summ2x2$Spruce4_sum*60+
                                Summ2x2$Spruce5_sum*80+
                                Summ2x2$Spruce6_sum*100+
                                Summ2x2$Spruce7_sum*120+
                                Summ2x2$Spruce8_sum*140+
                                Summ2x2$Spruce9_sum*160+
                                Summ2x2$PineR_sum*4.5+
                                Summ2x2$Pine1_sum*10+
                                Summ2x2$Pine2_sum*20+
                                Summ2x2$Pine3_sum*40+
                                Summ2x2$Pine4_sum*60+
                                Summ2x2$Pine5_sum*80+
                                Summ2x2$Pine6_sum*100+
                                Summ2x2$Pine7_sum*120+
                                Summ2x2$Pine8_sum*140+
                                Summ2x2$Pine9_sum*160+
                                Summ2x2$DecidR_sum*4.5+
                                Summ2x2$Decid1_sum*10+
                                Summ2x2$Decid2_sum*20+
                                Summ2x2$Decid3_sum*40+
                                Summ2x2$Decid4_sum*60+
                                Summ2x2$Decid5_sum*80+
                                Summ2x2$Decid6_sum*100+
                                Summ2x2$Decid7_sum*120+
                                Summ2x2$Decid8_sum*140+
                                Summ2x2$Decid9_sum*160+
                                Summ2x2$MixedwoodR_sum*4.5+
                                Summ2x2$Mixedwood1_sum*10+
                                Summ2x2$Mixedwood2_sum*20+
                                Summ2x2$Mixedwood3_sum*40+
                                Summ2x2$Mixedwood4_sum*60+
                                Summ2x2$Mixedwood5_sum*80+
                                Summ2x2$Mixedwood6_sum*100+
                                Summ2x2$Mixedwood7_sum*120+
                                Summ2x2$Mixedwood8_sum*140+
                                Summ2x2$Mixedwood9_sum*160+
                                Summ2x2$CCPineR_sum*4.5+
                                Summ2x2$CCPine1_sum*10+
                                Summ2x2$CCPine2_sum*20+
                                Summ2x2$CCPine3_sum*40+
                                Summ2x2$CCPine4_sum*60+
                                Summ2x2$CCSpruceR_sum*4.5+
                                Summ2x2$CCSpruce1_sum*10+
                                Summ2x2$CCSpruce2_sum*20+
                                Summ2x2$CCSpruce3_sum*40+
                                Summ2x2$CCSpruce4_sum*60+
                                Summ2x2$CCDecidR_sum*4.5+
                                Summ2x2$CCDecid1_sum*10+
                                Summ2x2$CCDecid2_sum*20+
                                Summ2x2$CCDecid3_sum*40+
                                Summ2x2$CCDecid4_sum*60+
                                Summ2x2$CCMixedwoodR_sum*4.5+
                                Summ2x2$CCMixedwood1_sum*10+
                                Summ2x2$CCMixedwood2_sum*20+
                                Summ2x2$CCMixedwood3_sum*40+
                                Summ2x2$CCMixedwood4_sum*60)/Summ2x2$TotalForest  

Summ2x2$PropSoftLinear.150.2017<-(Summ2x2$SeismicLineNarrow_sum+
                                    Summ2x2$SeismicLineWide_sum+
                                    Summ2x2$Pipeline_sum+
                                    Summ2x2$TransmissionLine_sum+
                                    Summ2x2$RoadTrailVegetated_sum+
                                    Summ2x2$RoadVegetatedVerge_sum+
                                    Summ2x2$RailVegetatedVerge_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$PropHardLinear.150.2017<-(Summ2x2$RoadHardSurface_sum+
                                    Summ2x2$RailHardSurface_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$PropSoftPoly.150.2017<-(Summ2x2$OtherDisturbedVegetation_sum+
                                  Summ2x2$WellSite_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

Summ2x2$PropHardPoly.150.2017<-(Summ2x2$CultivationCrop_sum+
                                  Summ2x2$CultivationAbandoned_sum+
                                  Summ2x2$CultivationRoughPasture_sum+
                                  Summ2x2$CultivationTamePasture_sum+
                                  Summ2x2$HighDensityLivestockOperation_sum+
                                  Summ2x2$BorrowpitsDugoutsSumps_sum+
                                  Summ2x2$MunicipalWaterSewage_sum+
                                  Summ2x2$Reservoirs_sum+
                                  Summ2x2$Canals_sum+
                                  Summ2x2$UrbanIndustrial_sum+
                                  Summ2x2$UrbanResidence_sum+
                                  Summ2x2$RuralResidentialIndustrial_sum+
                                  Summ2x2$IndustrialSiteRural_sum+
                                  Summ2x2$WindGenerationFacility_sum+
                                  Summ2x2$MineSite_sum+
                                  Summ2x2$PeatMine_sum)/(282741)#approximate area in sq.m of 2x2 landscape using land within 150 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ2x2$zerolndscp<-str_sub(Summ2x2$L2x2, -2, -1)#distinguishes '0' from '10', '20'
Summ2x2<-Summ2x2[!Summ2x2$zerolndscp=="-0",]
write.csv(Summ2x2, file="0_data/processed/different scales/vegHF.150.2017.2x2.csv")

Summ3x3<-vegHF.150.2017%>%
  group_by(L3x3)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ3x3<-data.frame(Summ3x3)
Summ3x3$PropConifer.150.2017<-(Summ3x3$CCPineR_sum+
                                 Summ3x3$CCPine1_sum+
                                 Summ3x3$CCPine2_sum+
                                 Summ3x3$CCPine3_sum+
                                 Summ3x3$CCPine4_sum+
                                 Summ3x3$CCSpruceR_sum+
                                 Summ3x3$CCSpruce1_sum+
                                 Summ3x3$CCSpruce2_sum+
                                 Summ3x3$CCSpruce3_sum+
                                 Summ3x3$CCSpruce4_sum+
                                 Summ3x3$TreedFenR_sum+
                                 Summ3x3$TreedFen1_sum+
                                 Summ3x3$TreedFen2_sum+
                                 Summ3x3$TreedFen3_sum+
                                 Summ3x3$TreedFen4_sum+
                                 Summ3x3$TreedFen5_sum+
                                 Summ3x3$TreedFen6_sum+
                                 Summ3x3$TreedFen7_sum+
                                 Summ3x3$TreedFen8_sum+
                                 Summ3x3$TreedFen9_sum+
                                 Summ3x3$TreedBogR_sum+
                                 Summ3x3$TreedBog1_sum+
                                 Summ3x3$TreedBog2_sum+
                                 Summ3x3$TreedBog3_sum+
                                 Summ3x3$TreedBog4_sum+
                                 Summ3x3$TreedBog5_sum+
                                 Summ3x3$TreedBog6_sum+
                                 Summ3x3$TreedBog7_sum+
                                 Summ3x3$TreedBog8_sum+
                                 Summ3x3$TreedBog9_sum+
                                 Summ3x3$SpruceR_sum+
                                 Summ3x3$Spruce1_sum+
                                 Summ3x3$Spruce2_sum+
                                 Summ3x3$Spruce3_sum+
                                 Summ3x3$Spruce4_sum+
                                 Summ3x3$Spruce5_sum+
                                 Summ3x3$Spruce6_sum+
                                 Summ3x3$Spruce7_sum+
                                 Summ3x3$Spruce8_sum+
                                 Summ3x3$Spruce9_sum+
                                 Summ3x3$PineR_sum+
                                 Summ3x3$Pine1_sum+
                                 Summ3x3$Pine2_sum+
                                 Summ3x3$Pine3_sum+
                                 Summ3x3$Pine4_sum+
                                 Summ3x3$Pine5_sum+
                                 Summ3x3$Pine6_sum+
                                 Summ3x3$Pine7_sum+
                                 Summ3x3$Pine8_sum+
                                 Summ3x3$Pine9_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$PropWet.150.2017<-(Summ3x3$ShrubbyBog_sum+
                             Summ3x3$ShrubbyFen_sum+
                             Summ3x3$ShrubbySwamp_sum+
                             Summ3x3$Marsh_sum+
                             Summ3x3$GraminoidFen_sum+
                             Summ3x3$TreedBogR_sum+
                             Summ3x3$TreedBog1_sum+
                             Summ3x3$TreedBog2_sum+
                             Summ3x3$TreedBog3_sum+
                             Summ3x3$TreedBog4_sum+
                             Summ3x3$TreedBog5_sum+
                             Summ3x3$TreedBog6_sum+
                             Summ3x3$TreedBog7_sum+
                             Summ3x3$TreedBog8_sum+
                             Summ3x3$TreedBog9_sum+
                             Summ3x3$TreedFenR_sum+
                             Summ3x3$TreedFen1_sum+
                             Summ3x3$TreedFen2_sum+
                             Summ3x3$TreedFen3_sum+
                             Summ3x3$TreedFen4_sum+
                             Summ3x3$TreedFen5_sum+
                             Summ3x3$TreedFen6_sum+
                             Summ3x3$TreedFen7_sum+
                             Summ3x3$TreedFen8_sum+
                             Summ3x3$TreedFen9_sum+
                             Summ3x3$TreedSwampR_sum+
                             Summ3x3$TreedSwamp1_sum+
                             Summ3x3$TreedSwamp2_sum+
                             Summ3x3$TreedSwamp3_sum+
                             Summ3x3$TreedSwamp4_sum+
                             Summ3x3$TreedSwamp5_sum+
                             Summ3x3$TreedSwamp6_sum+
                             Summ3x3$TreedSwamp7_sum+
                             Summ3x3$TreedSwamp8_sum+
                             Summ3x3$TreedSwamp9_sum+
                             Summ3x3$Water_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$TotalForest<-Summ3x3$TreedBogR_sum+
  Summ3x3$TreedBog1_sum+
  Summ3x3$TreedBog2_sum+
  Summ3x3$TreedBog3_sum+
  Summ3x3$TreedBog4_sum+
  Summ3x3$TreedBog5_sum+
  Summ3x3$TreedBog6_sum+
  Summ3x3$TreedBog7_sum+
  Summ3x3$TreedBog8_sum+
  Summ3x3$TreedBog9_sum+
  Summ3x3$TreedFenR_sum+
  Summ3x3$TreedFen1_sum+
  Summ3x3$TreedFen2_sum+
  Summ3x3$TreedFen3_sum+
  Summ3x3$TreedFen4_sum+
  Summ3x3$TreedFen5_sum+
  Summ3x3$TreedFen6_sum+
  Summ3x3$TreedFen7_sum+
  Summ3x3$TreedFen8_sum+
  Summ3x3$TreedFen9_sum+
  Summ3x3$TreedSwampR_sum+
  Summ3x3$TreedSwamp1_sum+
  Summ3x3$TreedSwamp2_sum+
  Summ3x3$TreedSwamp3_sum+
  Summ3x3$TreedSwamp4_sum+
  Summ3x3$TreedSwamp5_sum+
  Summ3x3$TreedSwamp6_sum+
  Summ3x3$TreedSwamp7_sum+
  Summ3x3$TreedSwamp8_sum+
  Summ3x3$TreedSwamp9_sum+
  Summ3x3$SpruceR_sum+
  Summ3x3$Spruce1_sum+
  Summ3x3$Spruce2_sum+
  Summ3x3$Spruce3_sum+
  Summ3x3$Spruce4_sum+
  Summ3x3$Spruce5_sum+
  Summ3x3$Spruce6_sum+
  Summ3x3$Spruce7_sum+
  Summ3x3$Spruce8_sum+
  Summ3x3$Spruce9_sum+
  Summ3x3$PineR_sum+
  Summ3x3$Pine1_sum+
  Summ3x3$Pine2_sum+
  Summ3x3$Pine3_sum+
  Summ3x3$Pine4_sum+
  Summ3x3$Pine5_sum+
  Summ3x3$Pine6_sum+
  Summ3x3$Pine7_sum+
  Summ3x3$Pine8_sum+
  Summ3x3$Pine9_sum+
  Summ3x3$DecidR_sum+
  Summ3x3$Decid1_sum+
  Summ3x3$Decid2_sum+
  Summ3x3$Decid3_sum+
  Summ3x3$Decid4_sum+
  Summ3x3$Decid5_sum+
  Summ3x3$Decid6_sum+
  Summ3x3$Decid7_sum+
  Summ3x3$Decid8_sum+
  Summ3x3$Decid9_sum+
  Summ3x3$MixedwoodR_sum+
  Summ3x3$Mixedwood1_sum+
  Summ3x3$Mixedwood2_sum+
  Summ3x3$Mixedwood3_sum+
  Summ3x3$Mixedwood4_sum+
  Summ3x3$Mixedwood5_sum+
  Summ3x3$Mixedwood6_sum+
  Summ3x3$Mixedwood7_sum+
  Summ3x3$Mixedwood8_sum+
  Summ3x3$Mixedwood9_sum+
  Summ3x3$CCPineR_sum+
  Summ3x3$CCPine1_sum+
  Summ3x3$CCPine2_sum+
  Summ3x3$CCPine3_sum+
  Summ3x3$CCPine4_sum+
  Summ3x3$CCSpruceR_sum+
  Summ3x3$CCSpruce1_sum+
  Summ3x3$CCSpruce2_sum+
  Summ3x3$CCSpruce3_sum+
  Summ3x3$CCSpruce4_sum+
  Summ3x3$CCDecidR_sum+
  Summ3x3$CCDecid1_sum+
  Summ3x3$CCDecid2_sum+
  Summ3x3$CCDecid3_sum+
  Summ3x3$CCDecid4_sum+
  Summ3x3$CCMixedwoodR_sum+
  Summ3x3$CCMixedwood1_sum+
  Summ3x3$CCMixedwood2_sum+
  Summ3x3$CCMixedwood3_sum+
  Summ3x3$CCMixedwood4_sum

Summ3x3$Wt.For.Age.150.2017<-(Summ3x3$TreedBogR_sum*4.5+#0-9 years
                                Summ3x3$TreedBog1_sum*10+#10-19 years
                                Summ3x3$TreedBog2_sum*20+#20-39 years
                                Summ3x3$TreedBog3_sum*40+#40-59 years
                                Summ3x3$TreedBog4_sum*60+#60-79 years
                                Summ3x3$TreedBog5_sum*80+#80-99 years
                                Summ3x3$TreedBog6_sum*100+#100-119 years
                                Summ3x3$TreedBog7_sum*120+#120-139 years
                                Summ3x3$TreedBog8_sum*140+#140-159 years
                                Summ3x3$TreedBog9_sum*160+#160 years and older
                                Summ3x3$TreedFenR_sum*4.5+
                                Summ3x3$TreedFen1_sum*10+
                                Summ3x3$TreedFen2_sum*20+
                                Summ3x3$TreedFen3_sum*40+
                                Summ3x3$TreedFen4_sum*60+
                                Summ3x3$TreedFen5_sum*80+
                                Summ3x3$TreedFen6_sum*100+
                                Summ3x3$TreedFen7_sum*120+
                                Summ3x3$TreedFen8_sum*140+
                                Summ3x3$TreedFen9_sum*160+
                                Summ3x3$TreedSwampR_sum*4.5+
                                Summ3x3$TreedSwamp1_sum*10+
                                Summ3x3$TreedSwamp2_sum*20+
                                Summ3x3$TreedSwamp3_sum*40+
                                Summ3x3$TreedSwamp4_sum*60+
                                Summ3x3$TreedSwamp5_sum*80+
                                Summ3x3$TreedSwamp6_sum*100+
                                Summ3x3$TreedSwamp7_sum*120+
                                Summ3x3$TreedSwamp8_sum*140+
                                Summ3x3$TreedSwamp9_sum*160+
                                Summ3x3$SpruceR_sum*4.5+
                                Summ3x3$Spruce1_sum*10+
                                Summ3x3$Spruce2_sum*20+
                                Summ3x3$Spruce3_sum*40+
                                Summ3x3$Spruce4_sum*60+
                                Summ3x3$Spruce5_sum*80+
                                Summ3x3$Spruce6_sum*100+
                                Summ3x3$Spruce7_sum*120+
                                Summ3x3$Spruce8_sum*140+
                                Summ3x3$Spruce9_sum*160+
                                Summ3x3$PineR_sum*4.5+
                                Summ3x3$Pine1_sum*10+
                                Summ3x3$Pine2_sum*20+
                                Summ3x3$Pine3_sum*40+
                                Summ3x3$Pine4_sum*60+
                                Summ3x3$Pine5_sum*80+
                                Summ3x3$Pine6_sum*100+
                                Summ3x3$Pine7_sum*120+
                                Summ3x3$Pine8_sum*140+
                                Summ3x3$Pine9_sum*160+
                                Summ3x3$DecidR_sum*4.5+
                                Summ3x3$Decid1_sum*10+
                                Summ3x3$Decid2_sum*20+
                                Summ3x3$Decid3_sum*40+
                                Summ3x3$Decid4_sum*60+
                                Summ3x3$Decid5_sum*80+
                                Summ3x3$Decid6_sum*100+
                                Summ3x3$Decid7_sum*120+
                                Summ3x3$Decid8_sum*140+
                                Summ3x3$Decid9_sum*160+
                                Summ3x3$MixedwoodR_sum*4.5+
                                Summ3x3$Mixedwood1_sum*10+
                                Summ3x3$Mixedwood2_sum*20+
                                Summ3x3$Mixedwood3_sum*40+
                                Summ3x3$Mixedwood4_sum*60+
                                Summ3x3$Mixedwood5_sum*80+
                                Summ3x3$Mixedwood6_sum*100+
                                Summ3x3$Mixedwood7_sum*120+
                                Summ3x3$Mixedwood8_sum*140+
                                Summ3x3$Mixedwood9_sum*160+
                                Summ3x3$CCPineR_sum*4.5+
                                Summ3x3$CCPine1_sum*10+
                                Summ3x3$CCPine2_sum*20+
                                Summ3x3$CCPine3_sum*40+
                                Summ3x3$CCPine4_sum*60+
                                Summ3x3$CCSpruceR_sum*4.5+
                                Summ3x3$CCSpruce1_sum*10+
                                Summ3x3$CCSpruce2_sum*20+
                                Summ3x3$CCSpruce3_sum*40+
                                Summ3x3$CCSpruce4_sum*60+
                                Summ3x3$CCDecidR_sum*4.5+
                                Summ3x3$CCDecid1_sum*10+
                                Summ3x3$CCDecid2_sum*20+
                                Summ3x3$CCDecid3_sum*40+
                                Summ3x3$CCDecid4_sum*60+
                                Summ3x3$CCMixedwoodR_sum*4.5+
                                Summ3x3$CCMixedwood1_sum*10+
                                Summ3x3$CCMixedwood2_sum*20+
                                Summ3x3$CCMixedwood3_sum*40+
                                Summ3x3$CCMixedwood4_sum*60)/Summ3x3$TotalForest  

Summ3x3$PropSoftLinear.150.2017<-(Summ3x3$SeismicLineNarrow_sum+
                                    Summ3x3$SeismicLineWide_sum+
                                    Summ3x3$Pipeline_sum+
                                    Summ3x3$TransmissionLine_sum+
                                    Summ3x3$RoadTrailVegetated_sum+
                                    Summ3x3$RoadVegetatedVerge_sum+
                                    Summ3x3$RailVegetatedVerge_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$PropHardLinear.150.2017<-(Summ3x3$RoadHardSurface_sum+
                                    Summ3x3$RailHardSurface_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$PropSoftPoly.150.2017<-(Summ3x3$OtherDisturbedVegetation_sum+
                                  Summ3x3$WellSite_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

Summ3x3$PropHardPoly.150.2017<-(Summ3x3$CultivationCrop_sum+
                                  Summ3x3$CultivationAbandoned_sum+
                                  Summ3x3$CultivationRoughPasture_sum+
                                  Summ3x3$CultivationTamePasture_sum+
                                  Summ3x3$HighDensityLivestockOperation_sum+
                                  Summ3x3$BorrowpitsDugoutsSumps_sum+
                                  Summ3x3$MunicipalWaterSewage_sum+
                                  Summ3x3$Reservoirs_sum+
                                  Summ3x3$Canals_sum+
                                  Summ3x3$UrbanIndustrial_sum+
                                  Summ3x3$UrbanResidence_sum+
                                  Summ3x3$RuralResidentialIndustrial_sum+
                                  Summ3x3$IndustrialSiteRural_sum+
                                  Summ3x3$WindGenerationFacility_sum+
                                  Summ3x3$MineSite_sum+
                                  Summ3x3$PeatMine_sum)/(636167)#approximate area in sq.m of 3x3 landscape using land within 150 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ3x3$zerolndscp<-str_sub(Summ3x3$L3x3, -2, -1)#distinguishes '0' from '10', '20'
Summ3x3<-Summ3x3[!Summ3x3$zerolndscp=="-0",]
write.csv(Summ3x3, file="0_data/processed/different scales/vegHF.150.2017.3x3.csv")

Summ4x4<-vegHF.150.2017%>%
  group_by(L4x4)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ4x4<-data.frame(Summ4x4)
Summ4x4$PropConifer.150.2017<-(Summ4x4$CCPineR_sum+
                                 Summ4x4$CCPine1_sum+
                                 Summ4x4$CCPine2_sum+
                                 Summ4x4$CCPine3_sum+
                                 Summ4x4$CCPine4_sum+
                                 Summ4x4$CCSpruceR_sum+
                                 Summ4x4$CCSpruce1_sum+
                                 Summ4x4$CCSpruce2_sum+
                                 Summ4x4$CCSpruce3_sum+
                                 Summ4x4$CCSpruce4_sum+
                                 Summ4x4$TreedFenR_sum+
                                 Summ4x4$TreedFen1_sum+
                                 Summ4x4$TreedFen2_sum+
                                 Summ4x4$TreedFen3_sum+
                                 Summ4x4$TreedFen4_sum+
                                 Summ4x4$TreedFen5_sum+
                                 Summ4x4$TreedFen6_sum+
                                 Summ4x4$TreedFen7_sum+
                                 Summ4x4$TreedFen8_sum+
                                 Summ4x4$TreedFen9_sum+
                                 Summ4x4$TreedBogR_sum+
                                 Summ4x4$TreedBog1_sum+
                                 Summ4x4$TreedBog2_sum+
                                 Summ4x4$TreedBog3_sum+
                                 Summ4x4$TreedBog4_sum+
                                 Summ4x4$TreedBog5_sum+
                                 Summ4x4$TreedBog6_sum+
                                 Summ4x4$TreedBog7_sum+
                                 Summ4x4$TreedBog8_sum+
                                 Summ4x4$TreedBog9_sum+
                                 Summ4x4$SpruceR_sum+
                                 Summ4x4$Spruce1_sum+
                                 Summ4x4$Spruce2_sum+
                                 Summ4x4$Spruce3_sum+
                                 Summ4x4$Spruce4_sum+
                                 Summ4x4$Spruce5_sum+
                                 Summ4x4$Spruce6_sum+
                                 Summ4x4$Spruce7_sum+
                                 Summ4x4$Spruce8_sum+
                                 Summ4x4$Spruce9_sum+
                                 Summ4x4$PineR_sum+
                                 Summ4x4$Pine1_sum+
                                 Summ4x4$Pine2_sum+
                                 Summ4x4$Pine3_sum+
                                 Summ4x4$Pine4_sum+
                                 Summ4x4$Pine5_sum+
                                 Summ4x4$Pine6_sum+
                                 Summ4x4$Pine7_sum+
                                 Summ4x4$Pine8_sum+
                                 Summ4x4$Pine9_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$PropWet.150.2017<-(Summ4x4$ShrubbyBog_sum+
                             Summ4x4$ShrubbyFen_sum+
                             Summ4x4$ShrubbySwamp_sum+
                             Summ4x4$Marsh_sum+
                             Summ4x4$GraminoidFen_sum+
                             Summ4x4$TreedBogR_sum+
                             Summ4x4$TreedBog1_sum+
                             Summ4x4$TreedBog2_sum+
                             Summ4x4$TreedBog3_sum+
                             Summ4x4$TreedBog4_sum+
                             Summ4x4$TreedBog5_sum+
                             Summ4x4$TreedBog6_sum+
                             Summ4x4$TreedBog7_sum+
                             Summ4x4$TreedBog8_sum+
                             Summ4x4$TreedBog9_sum+
                             Summ4x4$TreedFenR_sum+
                             Summ4x4$TreedFen1_sum+
                             Summ4x4$TreedFen2_sum+
                             Summ4x4$TreedFen3_sum+
                             Summ4x4$TreedFen4_sum+
                             Summ4x4$TreedFen5_sum+
                             Summ4x4$TreedFen6_sum+
                             Summ4x4$TreedFen7_sum+
                             Summ4x4$TreedFen8_sum+
                             Summ4x4$TreedFen9_sum+
                             Summ4x4$TreedSwampR_sum+
                             Summ4x4$TreedSwamp1_sum+
                             Summ4x4$TreedSwamp2_sum+
                             Summ4x4$TreedSwamp3_sum+
                             Summ4x4$TreedSwamp4_sum+
                             Summ4x4$TreedSwamp5_sum+
                             Summ4x4$TreedSwamp6_sum+
                             Summ4x4$TreedSwamp7_sum+
                             Summ4x4$TreedSwamp8_sum+
                             Summ4x4$TreedSwamp9_sum+
                             Summ4x4$Water_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$TotalForest<-Summ4x4$TreedBogR_sum+
  Summ4x4$TreedBog1_sum+
  Summ4x4$TreedBog2_sum+
  Summ4x4$TreedBog3_sum+
  Summ4x4$TreedBog4_sum+
  Summ4x4$TreedBog5_sum+
  Summ4x4$TreedBog6_sum+
  Summ4x4$TreedBog7_sum+
  Summ4x4$TreedBog8_sum+
  Summ4x4$TreedBog9_sum+
  Summ4x4$TreedFenR_sum+
  Summ4x4$TreedFen1_sum+
  Summ4x4$TreedFen2_sum+
  Summ4x4$TreedFen3_sum+
  Summ4x4$TreedFen4_sum+
  Summ4x4$TreedFen5_sum+
  Summ4x4$TreedFen6_sum+
  Summ4x4$TreedFen7_sum+
  Summ4x4$TreedFen8_sum+
  Summ4x4$TreedFen9_sum+
  Summ4x4$TreedSwampR_sum+
  Summ4x4$TreedSwamp1_sum+
  Summ4x4$TreedSwamp2_sum+
  Summ4x4$TreedSwamp3_sum+
  Summ4x4$TreedSwamp4_sum+
  Summ4x4$TreedSwamp5_sum+
  Summ4x4$TreedSwamp6_sum+
  Summ4x4$TreedSwamp7_sum+
  Summ4x4$TreedSwamp8_sum+
  Summ4x4$TreedSwamp9_sum+
  Summ4x4$SpruceR_sum+
  Summ4x4$Spruce1_sum+
  Summ4x4$Spruce2_sum+
  Summ4x4$Spruce3_sum+
  Summ4x4$Spruce4_sum+
  Summ4x4$Spruce5_sum+
  Summ4x4$Spruce6_sum+
  Summ4x4$Spruce7_sum+
  Summ4x4$Spruce8_sum+
  Summ4x4$Spruce9_sum+
  Summ4x4$PineR_sum+
  Summ4x4$Pine1_sum+
  Summ4x4$Pine2_sum+
  Summ4x4$Pine3_sum+
  Summ4x4$Pine4_sum+
  Summ4x4$Pine5_sum+
  Summ4x4$Pine6_sum+
  Summ4x4$Pine7_sum+
  Summ4x4$Pine8_sum+
  Summ4x4$Pine9_sum+
  Summ4x4$DecidR_sum+
  Summ4x4$Decid1_sum+
  Summ4x4$Decid2_sum+
  Summ4x4$Decid3_sum+
  Summ4x4$Decid4_sum+
  Summ4x4$Decid5_sum+
  Summ4x4$Decid6_sum+
  Summ4x4$Decid7_sum+
  Summ4x4$Decid8_sum+
  Summ4x4$Decid9_sum+
  Summ4x4$MixedwoodR_sum+
  Summ4x4$Mixedwood1_sum+
  Summ4x4$Mixedwood2_sum+
  Summ4x4$Mixedwood3_sum+
  Summ4x4$Mixedwood4_sum+
  Summ4x4$Mixedwood5_sum+
  Summ4x4$Mixedwood6_sum+
  Summ4x4$Mixedwood7_sum+
  Summ4x4$Mixedwood8_sum+
  Summ4x4$Mixedwood9_sum+
  Summ4x4$CCPineR_sum+
  Summ4x4$CCPine1_sum+
  Summ4x4$CCPine2_sum+
  Summ4x4$CCPine3_sum+
  Summ4x4$CCPine4_sum+
  Summ4x4$CCSpruceR_sum+
  Summ4x4$CCSpruce1_sum+
  Summ4x4$CCSpruce2_sum+
  Summ4x4$CCSpruce3_sum+
  Summ4x4$CCSpruce4_sum+
  Summ4x4$CCDecidR_sum+
  Summ4x4$CCDecid1_sum+
  Summ4x4$CCDecid2_sum+
  Summ4x4$CCDecid3_sum+
  Summ4x4$CCDecid4_sum+
  Summ4x4$CCMixedwoodR_sum+
  Summ4x4$CCMixedwood1_sum+
  Summ4x4$CCMixedwood2_sum+
  Summ4x4$CCMixedwood3_sum+
  Summ4x4$CCMixedwood4_sum

Summ4x4$Wt.For.Age.150.2017<-(Summ4x4$TreedBogR_sum*4.5+#0-9 years
                                Summ4x4$TreedBog1_sum*10+#10-19 years
                                Summ4x4$TreedBog2_sum*20+#20-39 years
                                Summ4x4$TreedBog3_sum*40+#40-59 years
                                Summ4x4$TreedBog4_sum*60+#60-79 years
                                Summ4x4$TreedBog5_sum*80+#80-99 years
                                Summ4x4$TreedBog6_sum*100+#100-119 years
                                Summ4x4$TreedBog7_sum*120+#120-139 years
                                Summ4x4$TreedBog8_sum*140+#140-159 years
                                Summ4x4$TreedBog9_sum*160+#160 years and older
                                Summ4x4$TreedFenR_sum*4.5+
                                Summ4x4$TreedFen1_sum*10+
                                Summ4x4$TreedFen2_sum*20+
                                Summ4x4$TreedFen3_sum*40+
                                Summ4x4$TreedFen4_sum*60+
                                Summ4x4$TreedFen5_sum*80+
                                Summ4x4$TreedFen6_sum*100+
                                Summ4x4$TreedFen7_sum*120+
                                Summ4x4$TreedFen8_sum*140+
                                Summ4x4$TreedFen9_sum*160+
                                Summ4x4$TreedSwampR_sum*4.5+
                                Summ4x4$TreedSwamp1_sum*10+
                                Summ4x4$TreedSwamp2_sum*20+
                                Summ4x4$TreedSwamp3_sum*40+
                                Summ4x4$TreedSwamp4_sum*60+
                                Summ4x4$TreedSwamp5_sum*80+
                                Summ4x4$TreedSwamp6_sum*100+
                                Summ4x4$TreedSwamp7_sum*120+
                                Summ4x4$TreedSwamp8_sum*140+
                                Summ4x4$TreedSwamp9_sum*160+
                                Summ4x4$SpruceR_sum*4.5+
                                Summ4x4$Spruce1_sum*10+
                                Summ4x4$Spruce2_sum*20+
                                Summ4x4$Spruce3_sum*40+
                                Summ4x4$Spruce4_sum*60+
                                Summ4x4$Spruce5_sum*80+
                                Summ4x4$Spruce6_sum*100+
                                Summ4x4$Spruce7_sum*120+
                                Summ4x4$Spruce8_sum*140+
                                Summ4x4$Spruce9_sum*160+
                                Summ4x4$PineR_sum*4.5+
                                Summ4x4$Pine1_sum*10+
                                Summ4x4$Pine2_sum*20+
                                Summ4x4$Pine3_sum*40+
                                Summ4x4$Pine4_sum*60+
                                Summ4x4$Pine5_sum*80+
                                Summ4x4$Pine6_sum*100+
                                Summ4x4$Pine7_sum*120+
                                Summ4x4$Pine8_sum*140+
                                Summ4x4$Pine9_sum*160+
                                Summ4x4$DecidR_sum*4.5+
                                Summ4x4$Decid1_sum*10+
                                Summ4x4$Decid2_sum*20+
                                Summ4x4$Decid3_sum*40+
                                Summ4x4$Decid4_sum*60+
                                Summ4x4$Decid5_sum*80+
                                Summ4x4$Decid6_sum*100+
                                Summ4x4$Decid7_sum*120+
                                Summ4x4$Decid8_sum*140+
                                Summ4x4$Decid9_sum*160+
                                Summ4x4$MixedwoodR_sum*4.5+
                                Summ4x4$Mixedwood1_sum*10+
                                Summ4x4$Mixedwood2_sum*20+
                                Summ4x4$Mixedwood3_sum*40+
                                Summ4x4$Mixedwood4_sum*60+
                                Summ4x4$Mixedwood5_sum*80+
                                Summ4x4$Mixedwood6_sum*100+
                                Summ4x4$Mixedwood7_sum*120+
                                Summ4x4$Mixedwood8_sum*140+
                                Summ4x4$Mixedwood9_sum*160+
                                Summ4x4$CCPineR_sum*4.5+
                                Summ4x4$CCPine1_sum*10+
                                Summ4x4$CCPine2_sum*20+
                                Summ4x4$CCPine3_sum*40+
                                Summ4x4$CCPine4_sum*60+
                                Summ4x4$CCSpruceR_sum*4.5+
                                Summ4x4$CCSpruce1_sum*10+
                                Summ4x4$CCSpruce2_sum*20+
                                Summ4x4$CCSpruce3_sum*40+
                                Summ4x4$CCSpruce4_sum*60+
                                Summ4x4$CCDecidR_sum*4.5+
                                Summ4x4$CCDecid1_sum*10+
                                Summ4x4$CCDecid2_sum*20+
                                Summ4x4$CCDecid3_sum*40+
                                Summ4x4$CCDecid4_sum*60+
                                Summ4x4$CCMixedwoodR_sum*4.5+
                                Summ4x4$CCMixedwood1_sum*10+
                                Summ4x4$CCMixedwood2_sum*20+
                                Summ4x4$CCMixedwood3_sum*40+
                                Summ4x4$CCMixedwood4_sum*60)/Summ4x4$TotalForest  

Summ4x4$PropSoftLinear.150.2017<-(Summ4x4$SeismicLineNarrow_sum+
                                    Summ4x4$SeismicLineWide_sum+
                                    Summ4x4$Pipeline_sum+
                                    Summ4x4$TransmissionLine_sum+
                                    Summ4x4$RoadTrailVegetated_sum+
                                    Summ4x4$RoadVegetatedVerge_sum+
                                    Summ4x4$RailVegetatedVerge_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$PropHardLinear.150.2017<-(Summ4x4$RoadHardSurface_sum+
                                    Summ4x4$RailHardSurface_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$PropSoftPoly.150.2017<-(Summ4x4$OtherDisturbedVegetation_sum+
                                  Summ4x4$WellSite_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

Summ4x4$PropHardPoly.150.2017<-(Summ4x4$CultivationCrop_sum+
                                  Summ4x4$CultivationAbandoned_sum+
                                  Summ4x4$CultivationRoughPasture_sum+
                                  Summ4x4$CultivationTamePasture_sum+
                                  Summ4x4$HighDensityLivestockOperation_sum+
                                  Summ4x4$BorrowpitsDugoutsSumps_sum+
                                  Summ4x4$MunicipalWaterSewage_sum+
                                  Summ4x4$Reservoirs_sum+
                                  Summ4x4$Canals_sum+
                                  Summ4x4$UrbanIndustrial_sum+
                                  Summ4x4$UrbanResidence_sum+
                                  Summ4x4$RuralResidentialIndustrial_sum+
                                  Summ4x4$IndustrialSiteRural_sum+
                                  Summ4x4$WindGenerationFacility_sum+
                                  Summ4x4$MineSite_sum+
                                  Summ4x4$PeatMine_sum)/(1130966)#approximate area in sq.m of 4x4 landscape using land within 150 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ4x4$zerolndscp<-str_sub(Summ4x4$L4x4, -2, -1)#distinguishes '0' from '10', '20'
Summ4x4<-Summ4x4[!Summ4x4$zerolndscp=="-0",]
write.csv(Summ4x4, file="0_data/processed/different scales/vegHF.150.2017.4x4.csv")

Summ5x5<-vegHF.150.2017%>%
  group_by(L5x5)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ5x5<-data.frame(Summ5x5)
Summ5x5$PropConifer.150.2017<-(Summ5x5$CCPineR_sum+
                                 Summ5x5$CCPine1_sum+
                                 Summ5x5$CCPine2_sum+
                                 Summ5x5$CCPine3_sum+
                                 Summ5x5$CCPine4_sum+
                                 Summ5x5$CCSpruceR_sum+
                                 Summ5x5$CCSpruce1_sum+
                                 Summ5x5$CCSpruce2_sum+
                                 Summ5x5$CCSpruce3_sum+
                                 Summ5x5$CCSpruce4_sum+
                                 Summ5x5$TreedFenR_sum+
                                 Summ5x5$TreedFen1_sum+
                                 Summ5x5$TreedFen2_sum+
                                 Summ5x5$TreedFen3_sum+
                                 Summ5x5$TreedFen4_sum+
                                 Summ5x5$TreedFen5_sum+
                                 Summ5x5$TreedFen6_sum+
                                 Summ5x5$TreedFen7_sum+
                                 Summ5x5$TreedFen8_sum+
                                 Summ5x5$TreedFen9_sum+
                                 Summ5x5$TreedBogR_sum+
                                 Summ5x5$TreedBog1_sum+
                                 Summ5x5$TreedBog2_sum+
                                 Summ5x5$TreedBog3_sum+
                                 Summ5x5$TreedBog4_sum+
                                 Summ5x5$TreedBog5_sum+
                                 Summ5x5$TreedBog6_sum+
                                 Summ5x5$TreedBog7_sum+
                                 Summ5x5$TreedBog8_sum+
                                 Summ5x5$TreedBog9_sum+
                                 Summ5x5$SpruceR_sum+
                                 Summ5x5$Spruce1_sum+
                                 Summ5x5$Spruce2_sum+
                                 Summ5x5$Spruce3_sum+
                                 Summ5x5$Spruce4_sum+
                                 Summ5x5$Spruce5_sum+
                                 Summ5x5$Spruce6_sum+
                                 Summ5x5$Spruce7_sum+
                                 Summ5x5$Spruce8_sum+
                                 Summ5x5$Spruce9_sum+
                                 Summ5x5$PineR_sum+
                                 Summ5x5$Pine1_sum+
                                 Summ5x5$Pine2_sum+
                                 Summ5x5$Pine3_sum+
                                 Summ5x5$Pine4_sum+
                                 Summ5x5$Pine5_sum+
                                 Summ5x5$Pine6_sum+
                                 Summ5x5$Pine7_sum+
                                 Summ5x5$Pine8_sum+
                                 Summ5x5$Pine9_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$PropWet.150.2017<-(Summ5x5$ShrubbyBog_sum+
                             Summ5x5$ShrubbyFen_sum+
                             Summ5x5$ShrubbySwamp_sum+
                             Summ5x5$Marsh_sum+
                             Summ5x5$GraminoidFen_sum+
                             Summ5x5$TreedBogR_sum+
                             Summ5x5$TreedBog1_sum+
                             Summ5x5$TreedBog2_sum+
                             Summ5x5$TreedBog3_sum+
                             Summ5x5$TreedBog4_sum+
                             Summ5x5$TreedBog5_sum+
                             Summ5x5$TreedBog6_sum+
                             Summ5x5$TreedBog7_sum+
                             Summ5x5$TreedBog8_sum+
                             Summ5x5$TreedBog9_sum+
                             Summ5x5$TreedFenR_sum+
                             Summ5x5$TreedFen1_sum+
                             Summ5x5$TreedFen2_sum+
                             Summ5x5$TreedFen3_sum+
                             Summ5x5$TreedFen4_sum+
                             Summ5x5$TreedFen5_sum+
                             Summ5x5$TreedFen6_sum+
                             Summ5x5$TreedFen7_sum+
                             Summ5x5$TreedFen8_sum+
                             Summ5x5$TreedFen9_sum+
                             Summ5x5$TreedSwampR_sum+
                             Summ5x5$TreedSwamp1_sum+
                             Summ5x5$TreedSwamp2_sum+
                             Summ5x5$TreedSwamp3_sum+
                             Summ5x5$TreedSwamp4_sum+
                             Summ5x5$TreedSwamp5_sum+
                             Summ5x5$TreedSwamp6_sum+
                             Summ5x5$TreedSwamp7_sum+
                             Summ5x5$TreedSwamp8_sum+
                             Summ5x5$TreedSwamp9_sum+
                             Summ5x5$Water_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$TotalForest<-Summ5x5$TreedBogR_sum+
  Summ5x5$TreedBog1_sum+
  Summ5x5$TreedBog2_sum+
  Summ5x5$TreedBog3_sum+
  Summ5x5$TreedBog4_sum+
  Summ5x5$TreedBog5_sum+
  Summ5x5$TreedBog6_sum+
  Summ5x5$TreedBog7_sum+
  Summ5x5$TreedBog8_sum+
  Summ5x5$TreedBog9_sum+
  Summ5x5$TreedFenR_sum+
  Summ5x5$TreedFen1_sum+
  Summ5x5$TreedFen2_sum+
  Summ5x5$TreedFen3_sum+
  Summ5x5$TreedFen4_sum+
  Summ5x5$TreedFen5_sum+
  Summ5x5$TreedFen6_sum+
  Summ5x5$TreedFen7_sum+
  Summ5x5$TreedFen8_sum+
  Summ5x5$TreedFen9_sum+
  Summ5x5$TreedSwampR_sum+
  Summ5x5$TreedSwamp1_sum+
  Summ5x5$TreedSwamp2_sum+
  Summ5x5$TreedSwamp3_sum+
  Summ5x5$TreedSwamp4_sum+
  Summ5x5$TreedSwamp5_sum+
  Summ5x5$TreedSwamp6_sum+
  Summ5x5$TreedSwamp7_sum+
  Summ5x5$TreedSwamp8_sum+
  Summ5x5$TreedSwamp9_sum+
  Summ5x5$SpruceR_sum+
  Summ5x5$Spruce1_sum+
  Summ5x5$Spruce2_sum+
  Summ5x5$Spruce3_sum+
  Summ5x5$Spruce4_sum+
  Summ5x5$Spruce5_sum+
  Summ5x5$Spruce6_sum+
  Summ5x5$Spruce7_sum+
  Summ5x5$Spruce8_sum+
  Summ5x5$Spruce9_sum+
  Summ5x5$PineR_sum+
  Summ5x5$Pine1_sum+
  Summ5x5$Pine2_sum+
  Summ5x5$Pine3_sum+
  Summ5x5$Pine4_sum+
  Summ5x5$Pine5_sum+
  Summ5x5$Pine6_sum+
  Summ5x5$Pine7_sum+
  Summ5x5$Pine8_sum+
  Summ5x5$Pine9_sum+
  Summ5x5$DecidR_sum+
  Summ5x5$Decid1_sum+
  Summ5x5$Decid2_sum+
  Summ5x5$Decid3_sum+
  Summ5x5$Decid4_sum+
  Summ5x5$Decid5_sum+
  Summ5x5$Decid6_sum+
  Summ5x5$Decid7_sum+
  Summ5x5$Decid8_sum+
  Summ5x5$Decid9_sum+
  Summ5x5$MixedwoodR_sum+
  Summ5x5$Mixedwood1_sum+
  Summ5x5$Mixedwood2_sum+
  Summ5x5$Mixedwood3_sum+
  Summ5x5$Mixedwood4_sum+
  Summ5x5$Mixedwood5_sum+
  Summ5x5$Mixedwood6_sum+
  Summ5x5$Mixedwood7_sum+
  Summ5x5$Mixedwood8_sum+
  Summ5x5$Mixedwood9_sum+
  Summ5x5$CCPineR_sum+
  Summ5x5$CCPine1_sum+
  Summ5x5$CCPine2_sum+
  Summ5x5$CCPine3_sum+
  Summ5x5$CCPine4_sum+
  Summ5x5$CCSpruceR_sum+
  Summ5x5$CCSpruce1_sum+
  Summ5x5$CCSpruce2_sum+
  Summ5x5$CCSpruce3_sum+
  Summ5x5$CCSpruce4_sum+
  Summ5x5$CCDecidR_sum+
  Summ5x5$CCDecid1_sum+
  Summ5x5$CCDecid2_sum+
  Summ5x5$CCDecid3_sum+
  Summ5x5$CCDecid4_sum+
  Summ5x5$CCMixedwoodR_sum+
  Summ5x5$CCMixedwood1_sum+
  Summ5x5$CCMixedwood2_sum+
  Summ5x5$CCMixedwood3_sum+
  Summ5x5$CCMixedwood4_sum

Summ5x5$Wt.For.Age.150.2017<-(Summ5x5$TreedBogR_sum*4.5+#0-9 years
                                Summ5x5$TreedBog1_sum*10+#10-19 years
                                Summ5x5$TreedBog2_sum*20+#20-39 years
                                Summ5x5$TreedBog3_sum*40+#40-59 years
                                Summ5x5$TreedBog4_sum*60+#60-79 years
                                Summ5x5$TreedBog5_sum*80+#80-99 years
                                Summ5x5$TreedBog6_sum*100+#100-119 years
                                Summ5x5$TreedBog7_sum*120+#120-139 years
                                Summ5x5$TreedBog8_sum*140+#140-159 years
                                Summ5x5$TreedBog9_sum*160+#160 years and older
                                Summ5x5$TreedFenR_sum*4.5+
                                Summ5x5$TreedFen1_sum*10+
                                Summ5x5$TreedFen2_sum*20+
                                Summ5x5$TreedFen3_sum*40+
                                Summ5x5$TreedFen4_sum*60+
                                Summ5x5$TreedFen5_sum*80+
                                Summ5x5$TreedFen6_sum*100+
                                Summ5x5$TreedFen7_sum*120+
                                Summ5x5$TreedFen8_sum*140+
                                Summ5x5$TreedFen9_sum*160+
                                Summ5x5$TreedSwampR_sum*4.5+
                                Summ5x5$TreedSwamp1_sum*10+
                                Summ5x5$TreedSwamp2_sum*20+
                                Summ5x5$TreedSwamp3_sum*40+
                                Summ5x5$TreedSwamp4_sum*60+
                                Summ5x5$TreedSwamp5_sum*80+
                                Summ5x5$TreedSwamp6_sum*100+
                                Summ5x5$TreedSwamp7_sum*120+
                                Summ5x5$TreedSwamp8_sum*140+
                                Summ5x5$TreedSwamp9_sum*160+
                                Summ5x5$SpruceR_sum*4.5+
                                Summ5x5$Spruce1_sum*10+
                                Summ5x5$Spruce2_sum*20+
                                Summ5x5$Spruce3_sum*40+
                                Summ5x5$Spruce4_sum*60+
                                Summ5x5$Spruce5_sum*80+
                                Summ5x5$Spruce6_sum*100+
                                Summ5x5$Spruce7_sum*120+
                                Summ5x5$Spruce8_sum*140+
                                Summ5x5$Spruce9_sum*160+
                                Summ5x5$PineR_sum*4.5+
                                Summ5x5$Pine1_sum*10+
                                Summ5x5$Pine2_sum*20+
                                Summ5x5$Pine3_sum*40+
                                Summ5x5$Pine4_sum*60+
                                Summ5x5$Pine5_sum*80+
                                Summ5x5$Pine6_sum*100+
                                Summ5x5$Pine7_sum*120+
                                Summ5x5$Pine8_sum*140+
                                Summ5x5$Pine9_sum*160+
                                Summ5x5$DecidR_sum*4.5+
                                Summ5x5$Decid1_sum*10+
                                Summ5x5$Decid2_sum*20+
                                Summ5x5$Decid3_sum*40+
                                Summ5x5$Decid4_sum*60+
                                Summ5x5$Decid5_sum*80+
                                Summ5x5$Decid6_sum*100+
                                Summ5x5$Decid7_sum*120+
                                Summ5x5$Decid8_sum*140+
                                Summ5x5$Decid9_sum*160+
                                Summ5x5$MixedwoodR_sum*4.5+
                                Summ5x5$Mixedwood1_sum*10+
                                Summ5x5$Mixedwood2_sum*20+
                                Summ5x5$Mixedwood3_sum*40+
                                Summ5x5$Mixedwood4_sum*60+
                                Summ5x5$Mixedwood5_sum*80+
                                Summ5x5$Mixedwood6_sum*100+
                                Summ5x5$Mixedwood7_sum*120+
                                Summ5x5$Mixedwood8_sum*140+
                                Summ5x5$Mixedwood9_sum*160+
                                Summ5x5$CCPineR_sum*4.5+
                                Summ5x5$CCPine1_sum*10+
                                Summ5x5$CCPine2_sum*20+
                                Summ5x5$CCPine3_sum*40+
                                Summ5x5$CCPine4_sum*60+
                                Summ5x5$CCSpruceR_sum*4.5+
                                Summ5x5$CCSpruce1_sum*10+
                                Summ5x5$CCSpruce2_sum*20+
                                Summ5x5$CCSpruce3_sum*40+
                                Summ5x5$CCSpruce4_sum*60+
                                Summ5x5$CCDecidR_sum*4.5+
                                Summ5x5$CCDecid1_sum*10+
                                Summ5x5$CCDecid2_sum*20+
                                Summ5x5$CCDecid3_sum*40+
                                Summ5x5$CCDecid4_sum*60+
                                Summ5x5$CCMixedwoodR_sum*4.5+
                                Summ5x5$CCMixedwood1_sum*10+
                                Summ5x5$CCMixedwood2_sum*20+
                                Summ5x5$CCMixedwood3_sum*40+
                                Summ5x5$CCMixedwood4_sum*60)/Summ5x5$TotalForest  

Summ5x5$PropSoftLinear.150.2017<-(Summ5x5$SeismicLineNarrow_sum+
                                    Summ5x5$SeismicLineWide_sum+
                                    Summ5x5$Pipeline_sum+
                                    Summ5x5$TransmissionLine_sum+
                                    Summ5x5$RoadTrailVegetated_sum+
                                    Summ5x5$RoadVegetatedVerge_sum+
                                    Summ5x5$RailVegetatedVerge_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$PropHardLinear.150.2017<-(Summ5x5$RoadHardSurface_sum+
                                    Summ5x5$RailHardSurface_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$PropSoftPoly.150.2017<-(Summ5x5$OtherDisturbedVegetation_sum+
                                  Summ5x5$WellSite_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

Summ5x5$PropHardPoly.150.2017<-(Summ5x5$CultivationCrop_sum+
                                  Summ5x5$CultivationAbandoned_sum+
                                  Summ5x5$CultivationRoughPasture_sum+
                                  Summ5x5$CultivationTamePasture_sum+
                                  Summ5x5$HighDensityLivestockOperation_sum+
                                  Summ5x5$BorrowpitsDugoutsSumps_sum+
                                  Summ5x5$MunicipalWaterSewage_sum+
                                  Summ5x5$Reservoirs_sum+
                                  Summ5x5$Canals_sum+
                                  Summ5x5$UrbanIndustrial_sum+
                                  Summ5x5$UrbanResidence_sum+
                                  Summ5x5$RuralResidentialIndustrial_sum+
                                  Summ5x5$IndustrialSiteRural_sum+
                                  Summ5x5$WindGenerationFacility_sum+
                                  Summ5x5$MineSite_sum+
                                  Summ5x5$PeatMine_sum)/(1767133)#approximate area in sq.m of 5x5 landscape using land within 150 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ5x5$zerolndscp<-str_sub(Summ5x5$L5x5, -2, -1)#distinguishes '0' from '10', '20'
Summ5x5<-Summ5x5[!Summ5x5$zerolndscp=="-0",]
write.csv(Summ5x5, file="0_data/processed/different scales/vegHF.150.2017.5x5.csv")

#600-m square data from 2016
vegHF.600.2016<-read.csv("0_data/processed/10_vegHF36000m2scale_2016_stationgridassignment.csv", header=TRUE)
str(vegHF.600.2016)#1500 obs

#Create unique "landscapes"
vegHF.600.2016$L2x2<-paste0("L-",vegHF.600.2016$Gridnum,"-",vegHF.600.2016$g2x2)
vegHF.600.2016$L3x3<-paste0("L-",vegHF.600.2016$Gridnum,"-",vegHF.600.2016$g3x3)
vegHF.600.2016$L4x4<-paste0("L-",vegHF.600.2016$Gridnum,"-",vegHF.600.2016$g4x4)
vegHF.600.2016$L5x5<-paste0("L-",vegHF.600.2016$Gridnum,"-",vegHF.600.2016$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date

Summ2x2<-vegHF.600.2016%>%
  group_by(L2x2)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ2x2<-data.frame(Summ2x2)
Summ2x2$PropConifer.600.2016<-(Summ2x2$CCPineR_sum+
                                 Summ2x2$CCPine1_sum+
                                 Summ2x2$CCPine2_sum+
                                 Summ2x2$CCPine3_sum+
                                 Summ2x2$CCPine4_sum+
                                 Summ2x2$CCSpruceR_sum+
                                 Summ2x2$CCSpruce1_sum+
                                 Summ2x2$CCSpruce2_sum+
                                 Summ2x2$CCSpruce3_sum+
                                 Summ2x2$CCSpruce4_sum+
                                 Summ2x2$TreedFenR_sum+
                                 Summ2x2$TreedFen1_sum+
                                 Summ2x2$TreedFen2_sum+
                                 Summ2x2$TreedFen3_sum+
                                 Summ2x2$TreedFen4_sum+
                                 Summ2x2$TreedFen5_sum+
                                 Summ2x2$TreedFen6_sum+
                                 Summ2x2$TreedFen7_sum+
                                 Summ2x2$TreedFen8_sum+
                                 Summ2x2$TreedFen9_sum+
                                 Summ2x2$TreedBogR_sum+
                                 Summ2x2$TreedBog1_sum+
                                 Summ2x2$TreedBog2_sum+
                                 Summ2x2$TreedBog3_sum+
                                 Summ2x2$TreedBog4_sum+
                                 Summ2x2$TreedBog5_sum+
                                 Summ2x2$TreedBog6_sum+
                                 Summ2x2$TreedBog7_sum+
                                 Summ2x2$TreedBog8_sum+
                                 Summ2x2$TreedBog9_sum+
                                 Summ2x2$SpruceR_sum+
                                 Summ2x2$Spruce1_sum+
                                 Summ2x2$Spruce2_sum+
                                 Summ2x2$Spruce3_sum+
                                 Summ2x2$Spruce4_sum+
                                 Summ2x2$Spruce5_sum+
                                 Summ2x2$Spruce6_sum+
                                 Summ2x2$Spruce7_sum+
                                 Summ2x2$Spruce8_sum+
                                 Summ2x2$Spruce9_sum+
                                 Summ2x2$PineR_sum+
                                 Summ2x2$Pine1_sum+
                                 Summ2x2$Pine2_sum+
                                 Summ2x2$Pine3_sum+
                                 Summ2x2$Pine4_sum+
                                 Summ2x2$Pine5_sum+
                                 Summ2x2$Pine6_sum+
                                 Summ2x2$Pine7_sum+
                                 Summ2x2$Pine8_sum+
                                 Summ2x2$Pine9_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$PropWet.600.2016<-(Summ2x2$ShrubbyBog_sum+
                             Summ2x2$ShrubbyFen_sum+
                             Summ2x2$ShrubbySwamp_sum+
                             Summ2x2$Marsh_sum+
                             Summ2x2$GraminoidFen_sum+
                             Summ2x2$TreedBogR_sum+
                             Summ2x2$TreedBog1_sum+
                             Summ2x2$TreedBog2_sum+
                             Summ2x2$TreedBog3_sum+
                             Summ2x2$TreedBog4_sum+
                             Summ2x2$TreedBog5_sum+
                             Summ2x2$TreedBog6_sum+
                             Summ2x2$TreedBog7_sum+
                             Summ2x2$TreedBog8_sum+
                             Summ2x2$TreedBog9_sum+
                             Summ2x2$TreedFenR_sum+
                             Summ2x2$TreedFen1_sum+
                             Summ2x2$TreedFen2_sum+
                             Summ2x2$TreedFen3_sum+
                             Summ2x2$TreedFen4_sum+
                             Summ2x2$TreedFen5_sum+
                             Summ2x2$TreedFen6_sum+
                             Summ2x2$TreedFen7_sum+
                             Summ2x2$TreedFen8_sum+
                             Summ2x2$TreedFen9_sum+
                             Summ2x2$TreedSwampR_sum+
                             Summ2x2$TreedSwamp1_sum+
                             Summ2x2$TreedSwamp2_sum+
                             Summ2x2$TreedSwamp3_sum+
                             Summ2x2$TreedSwamp4_sum+
                             Summ2x2$TreedSwamp5_sum+
                             Summ2x2$TreedSwamp6_sum+
                             Summ2x2$TreedSwamp7_sum+
                             Summ2x2$TreedSwamp8_sum+
                             Summ2x2$TreedSwamp9_sum+
                             Summ2x2$Water_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$TotalForest<-Summ2x2$TreedBogR_sum+
  Summ2x2$TreedBog1_sum+
  Summ2x2$TreedBog2_sum+
  Summ2x2$TreedBog3_sum+
  Summ2x2$TreedBog4_sum+
  Summ2x2$TreedBog5_sum+
  Summ2x2$TreedBog6_sum+
  Summ2x2$TreedBog7_sum+
  Summ2x2$TreedBog8_sum+
  Summ2x2$TreedBog9_sum+
  Summ2x2$TreedFenR_sum+
  Summ2x2$TreedFen1_sum+
  Summ2x2$TreedFen2_sum+
  Summ2x2$TreedFen3_sum+
  Summ2x2$TreedFen4_sum+
  Summ2x2$TreedFen5_sum+
  Summ2x2$TreedFen6_sum+
  Summ2x2$TreedFen7_sum+
  Summ2x2$TreedFen8_sum+
  Summ2x2$TreedFen9_sum+
  Summ2x2$TreedSwampR_sum+
  Summ2x2$TreedSwamp1_sum+
  Summ2x2$TreedSwamp2_sum+
  Summ2x2$TreedSwamp3_sum+
  Summ2x2$TreedSwamp4_sum+
  Summ2x2$TreedSwamp5_sum+
  Summ2x2$TreedSwamp6_sum+
  Summ2x2$TreedSwamp7_sum+
  Summ2x2$TreedSwamp8_sum+
  Summ2x2$TreedSwamp9_sum+
  Summ2x2$SpruceR_sum+
  Summ2x2$Spruce1_sum+
  Summ2x2$Spruce2_sum+
  Summ2x2$Spruce3_sum+
  Summ2x2$Spruce4_sum+
  Summ2x2$Spruce5_sum+
  Summ2x2$Spruce6_sum+
  Summ2x2$Spruce7_sum+
  Summ2x2$Spruce8_sum+
  Summ2x2$Spruce9_sum+
  Summ2x2$PineR_sum+
  Summ2x2$Pine1_sum+
  Summ2x2$Pine2_sum+
  Summ2x2$Pine3_sum+
  Summ2x2$Pine4_sum+
  Summ2x2$Pine5_sum+
  Summ2x2$Pine6_sum+
  Summ2x2$Pine7_sum+
  Summ2x2$Pine8_sum+
  Summ2x2$Pine9_sum+
  Summ2x2$DecidR_sum+
  Summ2x2$Decid1_sum+
  Summ2x2$Decid2_sum+
  Summ2x2$Decid3_sum+
  Summ2x2$Decid4_sum+
  Summ2x2$Decid5_sum+
  Summ2x2$Decid6_sum+
  Summ2x2$Decid7_sum+
  Summ2x2$Decid8_sum+
  Summ2x2$Decid9_sum+
  Summ2x2$MixedwoodR_sum+
  Summ2x2$Mixedwood1_sum+
  Summ2x2$Mixedwood2_sum+
  Summ2x2$Mixedwood3_sum+
  Summ2x2$Mixedwood4_sum+
  Summ2x2$Mixedwood5_sum+
  Summ2x2$Mixedwood6_sum+
  Summ2x2$Mixedwood7_sum+
  Summ2x2$Mixedwood8_sum+
  Summ2x2$Mixedwood9_sum+
  Summ2x2$CCPineR_sum+
  Summ2x2$CCPine1_sum+
  Summ2x2$CCPine2_sum+
  Summ2x2$CCPine3_sum+
  Summ2x2$CCPine4_sum+
  Summ2x2$CCSpruceR_sum+
  Summ2x2$CCSpruce1_sum+
  Summ2x2$CCSpruce2_sum+
  Summ2x2$CCSpruce3_sum+
  Summ2x2$CCSpruce4_sum+
  Summ2x2$CCDecidR_sum+
  Summ2x2$CCDecid1_sum+
  Summ2x2$CCDecid2_sum+
  Summ2x2$CCDecid3_sum+
  Summ2x2$CCDecid4_sum+
  Summ2x2$CCMixedwoodR_sum+
  Summ2x2$CCMixedwood1_sum+
  Summ2x2$CCMixedwood2_sum+
  Summ2x2$CCMixedwood3_sum+
  Summ2x2$CCMixedwood4_sum

Summ2x2$Wt.For.Age.600.2016<-(Summ2x2$TreedBogR_sum*4.5+#0-9 years
                                Summ2x2$TreedBog1_sum*10+#10-19 years
                                Summ2x2$TreedBog2_sum*20+#20-39 years
                                Summ2x2$TreedBog3_sum*40+#40-59 years
                                Summ2x2$TreedBog4_sum*60+#60-79 years
                                Summ2x2$TreedBog5_sum*80+#80-99 years
                                Summ2x2$TreedBog6_sum*100+#100-119 years
                                Summ2x2$TreedBog7_sum*120+#120-139 years
                                Summ2x2$TreedBog8_sum*140+#140-159 years
                                Summ2x2$TreedBog9_sum*160+#160 years and older
                                Summ2x2$TreedFenR_sum*4.5+
                                Summ2x2$TreedFen1_sum*10+
                                Summ2x2$TreedFen2_sum*20+
                                Summ2x2$TreedFen3_sum*40+
                                Summ2x2$TreedFen4_sum*60+
                                Summ2x2$TreedFen5_sum*80+
                                Summ2x2$TreedFen6_sum*100+
                                Summ2x2$TreedFen7_sum*120+
                                Summ2x2$TreedFen8_sum*140+
                                Summ2x2$TreedFen9_sum*160+
                                Summ2x2$TreedSwampR_sum*4.5+
                                Summ2x2$TreedSwamp1_sum*10+
                                Summ2x2$TreedSwamp2_sum*20+
                                Summ2x2$TreedSwamp3_sum*40+
                                Summ2x2$TreedSwamp4_sum*60+
                                Summ2x2$TreedSwamp5_sum*80+
                                Summ2x2$TreedSwamp6_sum*100+
                                Summ2x2$TreedSwamp7_sum*120+
                                Summ2x2$TreedSwamp8_sum*140+
                                Summ2x2$TreedSwamp9_sum*160+
                                Summ2x2$SpruceR_sum*4.5+
                                Summ2x2$Spruce1_sum*10+
                                Summ2x2$Spruce2_sum*20+
                                Summ2x2$Spruce3_sum*40+
                                Summ2x2$Spruce4_sum*60+
                                Summ2x2$Spruce5_sum*80+
                                Summ2x2$Spruce6_sum*100+
                                Summ2x2$Spruce7_sum*120+
                                Summ2x2$Spruce8_sum*140+
                                Summ2x2$Spruce9_sum*160+
                                Summ2x2$PineR_sum*4.5+
                                Summ2x2$Pine1_sum*10+
                                Summ2x2$Pine2_sum*20+
                                Summ2x2$Pine3_sum*40+
                                Summ2x2$Pine4_sum*60+
                                Summ2x2$Pine5_sum*80+
                                Summ2x2$Pine6_sum*100+
                                Summ2x2$Pine7_sum*120+
                                Summ2x2$Pine8_sum*140+
                                Summ2x2$Pine9_sum*160+
                                Summ2x2$DecidR_sum*4.5+
                                Summ2x2$Decid1_sum*10+
                                Summ2x2$Decid2_sum*20+
                                Summ2x2$Decid3_sum*40+
                                Summ2x2$Decid4_sum*60+
                                Summ2x2$Decid5_sum*80+
                                Summ2x2$Decid6_sum*100+
                                Summ2x2$Decid7_sum*120+
                                Summ2x2$Decid8_sum*140+
                                Summ2x2$Decid9_sum*160+
                                Summ2x2$MixedwoodR_sum*4.5+
                                Summ2x2$Mixedwood1_sum*10+
                                Summ2x2$Mixedwood2_sum*20+
                                Summ2x2$Mixedwood3_sum*40+
                                Summ2x2$Mixedwood4_sum*60+
                                Summ2x2$Mixedwood5_sum*80+
                                Summ2x2$Mixedwood6_sum*100+
                                Summ2x2$Mixedwood7_sum*120+
                                Summ2x2$Mixedwood8_sum*140+
                                Summ2x2$Mixedwood9_sum*160+
                                Summ2x2$CCPineR_sum*4.5+
                                Summ2x2$CCPine1_sum*10+
                                Summ2x2$CCPine2_sum*20+
                                Summ2x2$CCPine3_sum*40+
                                Summ2x2$CCPine4_sum*60+
                                Summ2x2$CCSpruceR_sum*4.5+
                                Summ2x2$CCSpruce1_sum*10+
                                Summ2x2$CCSpruce2_sum*20+
                                Summ2x2$CCSpruce3_sum*40+
                                Summ2x2$CCSpruce4_sum*60+
                                Summ2x2$CCDecidR_sum*4.5+
                                Summ2x2$CCDecid1_sum*10+
                                Summ2x2$CCDecid2_sum*20+
                                Summ2x2$CCDecid3_sum*40+
                                Summ2x2$CCDecid4_sum*60+
                                Summ2x2$CCMixedwoodR_sum*4.5+
                                Summ2x2$CCMixedwood1_sum*10+
                                Summ2x2$CCMixedwood2_sum*20+
                                Summ2x2$CCMixedwood3_sum*40+
                                Summ2x2$CCMixedwood4_sum*60)/Summ2x2$TotalForest  

Summ2x2$PropSoftLinear.600.2016<-(Summ2x2$SeismicLineNarrow_sum+
                                    Summ2x2$SeismicLineWide_sum+
                                    Summ2x2$Pipeline_sum+
                                    Summ2x2$TransmissionLine_sum+
                                    Summ2x2$RoadTrailVegetated_sum+
                                    Summ2x2$RoadVegetatedVerge_sum+
                                    Summ2x2$RailVegetatedVerge_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$PropHardLinear.600.2016<-(Summ2x2$RoadHardSurface_sum+
                                    Summ2x2$RailHardSurface_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$PropSoftPoly.600.2016<-(Summ2x2$OtherDisturbedVegetation_sum+
                                  Summ2x2$WellSite_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$PropHardPoly.600.2016<-(Summ2x2$CultivationCrop_sum+
                                  Summ2x2$CultivationAbandoned_sum+
                                  Summ2x2$CultivationRoughPasture_sum+
                                  Summ2x2$CultivationTamePasture_sum+
                                  Summ2x2$HighDensityLivestockOperation_sum+
                                  Summ2x2$BorrowpitsDugoutsSumps_sum+
                                  Summ2x2$MunicipalWaterSewage_sum+
                                  Summ2x2$Reservoirs_sum+
                                  Summ2x2$Canals_sum+
                                  Summ2x2$UrbanIndustrial_sum+
                                  Summ2x2$UrbanResidence_sum+
                                  Summ2x2$RuralResidentialIndustrial_sum+
                                  Summ2x2$IndustrialSiteRural_sum+
                                  Summ2x2$WindGenerationFacility_sum+
                                  Summ2x2$MineSite_sum+
                                  Summ2x2$PeatMine_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ2x2$zerolndscp<-str_sub(Summ2x2$L2x2, -2, -1)#distinguishes '0' from '10', '20'
Summ2x2<-Summ2x2[!Summ2x2$zerolndscp=="-0",]
write.csv(Summ2x2, file="0_data/processed/different scales/vegHF.600.2016.2x2.csv")

Summ3x3<-vegHF.600.2016%>%
  group_by(L3x3)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ3x3<-data.frame(Summ3x3)
Summ3x3$PropConifer.600.2016<-(Summ3x3$CCPineR_sum+
                                 Summ3x3$CCPine1_sum+
                                 Summ3x3$CCPine2_sum+
                                 Summ3x3$CCPine3_sum+
                                 Summ3x3$CCPine4_sum+
                                 Summ3x3$CCSpruceR_sum+
                                 Summ3x3$CCSpruce1_sum+
                                 Summ3x3$CCSpruce2_sum+
                                 Summ3x3$CCSpruce3_sum+
                                 Summ3x3$CCSpruce4_sum+
                                 Summ3x3$TreedFenR_sum+
                                 Summ3x3$TreedFen1_sum+
                                 Summ3x3$TreedFen2_sum+
                                 Summ3x3$TreedFen3_sum+
                                 Summ3x3$TreedFen4_sum+
                                 Summ3x3$TreedFen5_sum+
                                 Summ3x3$TreedFen6_sum+
                                 Summ3x3$TreedFen7_sum+
                                 Summ3x3$TreedFen8_sum+
                                 Summ3x3$TreedFen9_sum+
                                 Summ3x3$TreedBogR_sum+
                                 Summ3x3$TreedBog1_sum+
                                 Summ3x3$TreedBog2_sum+
                                 Summ3x3$TreedBog3_sum+
                                 Summ3x3$TreedBog4_sum+
                                 Summ3x3$TreedBog5_sum+
                                 Summ3x3$TreedBog6_sum+
                                 Summ3x3$TreedBog7_sum+
                                 Summ3x3$TreedBog8_sum+
                                 Summ3x3$TreedBog9_sum+
                                 Summ3x3$SpruceR_sum+
                                 Summ3x3$Spruce1_sum+
                                 Summ3x3$Spruce2_sum+
                                 Summ3x3$Spruce3_sum+
                                 Summ3x3$Spruce4_sum+
                                 Summ3x3$Spruce5_sum+
                                 Summ3x3$Spruce6_sum+
                                 Summ3x3$Spruce7_sum+
                                 Summ3x3$Spruce8_sum+
                                 Summ3x3$Spruce9_sum+
                                 Summ3x3$PineR_sum+
                                 Summ3x3$Pine1_sum+
                                 Summ3x3$Pine2_sum+
                                 Summ3x3$Pine3_sum+
                                 Summ3x3$Pine4_sum+
                                 Summ3x3$Pine5_sum+
                                 Summ3x3$Pine6_sum+
                                 Summ3x3$Pine7_sum+
                                 Summ3x3$Pine8_sum+
                                 Summ3x3$Pine9_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$PropWet.600.2016<-(Summ3x3$ShrubbyBog_sum+
                             Summ3x3$ShrubbyFen_sum+
                             Summ3x3$ShrubbySwamp_sum+
                             Summ3x3$Marsh_sum+
                             Summ3x3$GraminoidFen_sum+
                             Summ3x3$TreedBogR_sum+
                             Summ3x3$TreedBog1_sum+
                             Summ3x3$TreedBog2_sum+
                             Summ3x3$TreedBog3_sum+
                             Summ3x3$TreedBog4_sum+
                             Summ3x3$TreedBog5_sum+
                             Summ3x3$TreedBog6_sum+
                             Summ3x3$TreedBog7_sum+
                             Summ3x3$TreedBog8_sum+
                             Summ3x3$TreedBog9_sum+
                             Summ3x3$TreedFenR_sum+
                             Summ3x3$TreedFen1_sum+
                             Summ3x3$TreedFen2_sum+
                             Summ3x3$TreedFen3_sum+
                             Summ3x3$TreedFen4_sum+
                             Summ3x3$TreedFen5_sum+
                             Summ3x3$TreedFen6_sum+
                             Summ3x3$TreedFen7_sum+
                             Summ3x3$TreedFen8_sum+
                             Summ3x3$TreedFen9_sum+
                             Summ3x3$TreedSwampR_sum+
                             Summ3x3$TreedSwamp1_sum+
                             Summ3x3$TreedSwamp2_sum+
                             Summ3x3$TreedSwamp3_sum+
                             Summ3x3$TreedSwamp4_sum+
                             Summ3x3$TreedSwamp5_sum+
                             Summ3x3$TreedSwamp6_sum+
                             Summ3x3$TreedSwamp7_sum+
                             Summ3x3$TreedSwamp8_sum+
                             Summ3x3$TreedSwamp9_sum+
                             Summ3x3$Water_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$TotalForest<-Summ3x3$TreedBogR_sum+
  Summ3x3$TreedBog1_sum+
  Summ3x3$TreedBog2_sum+
  Summ3x3$TreedBog3_sum+
  Summ3x3$TreedBog4_sum+
  Summ3x3$TreedBog5_sum+
  Summ3x3$TreedBog6_sum+
  Summ3x3$TreedBog7_sum+
  Summ3x3$TreedBog8_sum+
  Summ3x3$TreedBog9_sum+
  Summ3x3$TreedFenR_sum+
  Summ3x3$TreedFen1_sum+
  Summ3x3$TreedFen2_sum+
  Summ3x3$TreedFen3_sum+
  Summ3x3$TreedFen4_sum+
  Summ3x3$TreedFen5_sum+
  Summ3x3$TreedFen6_sum+
  Summ3x3$TreedFen7_sum+
  Summ3x3$TreedFen8_sum+
  Summ3x3$TreedFen9_sum+
  Summ3x3$TreedSwampR_sum+
  Summ3x3$TreedSwamp1_sum+
  Summ3x3$TreedSwamp2_sum+
  Summ3x3$TreedSwamp3_sum+
  Summ3x3$TreedSwamp4_sum+
  Summ3x3$TreedSwamp5_sum+
  Summ3x3$TreedSwamp6_sum+
  Summ3x3$TreedSwamp7_sum+
  Summ3x3$TreedSwamp8_sum+
  Summ3x3$TreedSwamp9_sum+
  Summ3x3$SpruceR_sum+
  Summ3x3$Spruce1_sum+
  Summ3x3$Spruce2_sum+
  Summ3x3$Spruce3_sum+
  Summ3x3$Spruce4_sum+
  Summ3x3$Spruce5_sum+
  Summ3x3$Spruce6_sum+
  Summ3x3$Spruce7_sum+
  Summ3x3$Spruce8_sum+
  Summ3x3$Spruce9_sum+
  Summ3x3$PineR_sum+
  Summ3x3$Pine1_sum+
  Summ3x3$Pine2_sum+
  Summ3x3$Pine3_sum+
  Summ3x3$Pine4_sum+
  Summ3x3$Pine5_sum+
  Summ3x3$Pine6_sum+
  Summ3x3$Pine7_sum+
  Summ3x3$Pine8_sum+
  Summ3x3$Pine9_sum+
  Summ3x3$DecidR_sum+
  Summ3x3$Decid1_sum+
  Summ3x3$Decid2_sum+
  Summ3x3$Decid3_sum+
  Summ3x3$Decid4_sum+
  Summ3x3$Decid5_sum+
  Summ3x3$Decid6_sum+
  Summ3x3$Decid7_sum+
  Summ3x3$Decid8_sum+
  Summ3x3$Decid9_sum+
  Summ3x3$MixedwoodR_sum+
  Summ3x3$Mixedwood1_sum+
  Summ3x3$Mixedwood2_sum+
  Summ3x3$Mixedwood3_sum+
  Summ3x3$Mixedwood4_sum+
  Summ3x3$Mixedwood5_sum+
  Summ3x3$Mixedwood6_sum+
  Summ3x3$Mixedwood7_sum+
  Summ3x3$Mixedwood8_sum+
  Summ3x3$Mixedwood9_sum+
  Summ3x3$CCPineR_sum+
  Summ3x3$CCPine1_sum+
  Summ3x3$CCPine2_sum+
  Summ3x3$CCPine3_sum+
  Summ3x3$CCPine4_sum+
  Summ3x3$CCSpruceR_sum+
  Summ3x3$CCSpruce1_sum+
  Summ3x3$CCSpruce2_sum+
  Summ3x3$CCSpruce3_sum+
  Summ3x3$CCSpruce4_sum+
  Summ3x3$CCDecidR_sum+
  Summ3x3$CCDecid1_sum+
  Summ3x3$CCDecid2_sum+
  Summ3x3$CCDecid3_sum+
  Summ3x3$CCDecid4_sum+
  Summ3x3$CCMixedwoodR_sum+
  Summ3x3$CCMixedwood1_sum+
  Summ3x3$CCMixedwood2_sum+
  Summ3x3$CCMixedwood3_sum+
  Summ3x3$CCMixedwood4_sum

Summ3x3$Wt.For.Age.600.2016<-(Summ3x3$TreedBogR_sum*4.5+#0-9 years
                                Summ3x3$TreedBog1_sum*10+#10-19 years
                                Summ3x3$TreedBog2_sum*20+#20-39 years
                                Summ3x3$TreedBog3_sum*40+#40-59 years
                                Summ3x3$TreedBog4_sum*60+#60-79 years
                                Summ3x3$TreedBog5_sum*80+#80-99 years
                                Summ3x3$TreedBog6_sum*100+#100-119 years
                                Summ3x3$TreedBog7_sum*120+#120-139 years
                                Summ3x3$TreedBog8_sum*140+#140-159 years
                                Summ3x3$TreedBog9_sum*160+#160 years and older
                                Summ3x3$TreedFenR_sum*4.5+
                                Summ3x3$TreedFen1_sum*10+
                                Summ3x3$TreedFen2_sum*20+
                                Summ3x3$TreedFen3_sum*40+
                                Summ3x3$TreedFen4_sum*60+
                                Summ3x3$TreedFen5_sum*80+
                                Summ3x3$TreedFen6_sum*100+
                                Summ3x3$TreedFen7_sum*120+
                                Summ3x3$TreedFen8_sum*140+
                                Summ3x3$TreedFen9_sum*160+
                                Summ3x3$TreedSwampR_sum*4.5+
                                Summ3x3$TreedSwamp1_sum*10+
                                Summ3x3$TreedSwamp2_sum*20+
                                Summ3x3$TreedSwamp3_sum*40+
                                Summ3x3$TreedSwamp4_sum*60+
                                Summ3x3$TreedSwamp5_sum*80+
                                Summ3x3$TreedSwamp6_sum*100+
                                Summ3x3$TreedSwamp7_sum*120+
                                Summ3x3$TreedSwamp8_sum*140+
                                Summ3x3$TreedSwamp9_sum*160+
                                Summ3x3$SpruceR_sum*4.5+
                                Summ3x3$Spruce1_sum*10+
                                Summ3x3$Spruce2_sum*20+
                                Summ3x3$Spruce3_sum*40+
                                Summ3x3$Spruce4_sum*60+
                                Summ3x3$Spruce5_sum*80+
                                Summ3x3$Spruce6_sum*100+
                                Summ3x3$Spruce7_sum*120+
                                Summ3x3$Spruce8_sum*140+
                                Summ3x3$Spruce9_sum*160+
                                Summ3x3$PineR_sum*4.5+
                                Summ3x3$Pine1_sum*10+
                                Summ3x3$Pine2_sum*20+
                                Summ3x3$Pine3_sum*40+
                                Summ3x3$Pine4_sum*60+
                                Summ3x3$Pine5_sum*80+
                                Summ3x3$Pine6_sum*100+
                                Summ3x3$Pine7_sum*120+
                                Summ3x3$Pine8_sum*140+
                                Summ3x3$Pine9_sum*160+
                                Summ3x3$DecidR_sum*4.5+
                                Summ3x3$Decid1_sum*10+
                                Summ3x3$Decid2_sum*20+
                                Summ3x3$Decid3_sum*40+
                                Summ3x3$Decid4_sum*60+
                                Summ3x3$Decid5_sum*80+
                                Summ3x3$Decid6_sum*100+
                                Summ3x3$Decid7_sum*120+
                                Summ3x3$Decid8_sum*140+
                                Summ3x3$Decid9_sum*160+
                                Summ3x3$MixedwoodR_sum*4.5+
                                Summ3x3$Mixedwood1_sum*10+
                                Summ3x3$Mixedwood2_sum*20+
                                Summ3x3$Mixedwood3_sum*40+
                                Summ3x3$Mixedwood4_sum*60+
                                Summ3x3$Mixedwood5_sum*80+
                                Summ3x3$Mixedwood6_sum*100+
                                Summ3x3$Mixedwood7_sum*120+
                                Summ3x3$Mixedwood8_sum*140+
                                Summ3x3$Mixedwood9_sum*160+
                                Summ3x3$CCPineR_sum*4.5+
                                Summ3x3$CCPine1_sum*10+
                                Summ3x3$CCPine2_sum*20+
                                Summ3x3$CCPine3_sum*40+
                                Summ3x3$CCPine4_sum*60+
                                Summ3x3$CCSpruceR_sum*4.5+
                                Summ3x3$CCSpruce1_sum*10+
                                Summ3x3$CCSpruce2_sum*20+
                                Summ3x3$CCSpruce3_sum*40+
                                Summ3x3$CCSpruce4_sum*60+
                                Summ3x3$CCDecidR_sum*4.5+
                                Summ3x3$CCDecid1_sum*10+
                                Summ3x3$CCDecid2_sum*20+
                                Summ3x3$CCDecid3_sum*40+
                                Summ3x3$CCDecid4_sum*60+
                                Summ3x3$CCMixedwoodR_sum*4.5+
                                Summ3x3$CCMixedwood1_sum*10+
                                Summ3x3$CCMixedwood2_sum*20+
                                Summ3x3$CCMixedwood3_sum*40+
                                Summ3x3$CCMixedwood4_sum*60)/Summ3x3$TotalForest  

Summ3x3$PropSoftLinear.600.2016<-(Summ3x3$SeismicLineNarrow_sum+
                                    Summ3x3$SeismicLineWide_sum+
                                    Summ3x3$Pipeline_sum+
                                    Summ3x3$TransmissionLine_sum+
                                    Summ3x3$RoadTrailVegetated_sum+
                                    Summ3x3$RoadVegetatedVerge_sum+
                                    Summ3x3$RailVegetatedVerge_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$PropHardLinear.600.2016<-(Summ3x3$RoadHardSurface_sum+
                                    Summ3x3$RailHardSurface_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$PropSoftPoly.600.2016<-(Summ3x3$OtherDisturbedVegetation_sum+
                                  Summ3x3$WellSite_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$PropHardPoly.600.2016<-(Summ3x3$CultivationCrop_sum+
                                  Summ3x3$CultivationAbandoned_sum+
                                  Summ3x3$CultivationRoughPasture_sum+
                                  Summ3x3$CultivationTamePasture_sum+
                                  Summ3x3$HighDensityLivestockOperation_sum+
                                  Summ3x3$BorrowpitsDugoutsSumps_sum+
                                  Summ3x3$MunicipalWaterSewage_sum+
                                  Summ3x3$Reservoirs_sum+
                                  Summ3x3$Canals_sum+
                                  Summ3x3$UrbanIndustrial_sum+
                                  Summ3x3$UrbanResidence_sum+
                                  Summ3x3$RuralResidentialIndustrial_sum+
                                  Summ3x3$IndustrialSiteRural_sum+
                                  Summ3x3$WindGenerationFacility_sum+
                                  Summ3x3$MineSite_sum+
                                  Summ3x3$PeatMine_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ3x3$zerolndscp<-str_sub(Summ3x3$L3x3, -2, -1)#distinguishes '0' from '10', '20'
Summ3x3<-Summ3x3[!Summ3x3$zerolndscp=="-0",]
write.csv(Summ3x3, file="0_data/processed/different scales/vegHF.600.2016.3x3.csv")

Summ4x4<-vegHF.600.2016%>%
  group_by(L4x4)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ4x4<-data.frame(Summ4x4)
Summ4x4$PropConifer.600.2016<-(Summ4x4$CCPineR_sum+
                                 Summ4x4$CCPine1_sum+
                                 Summ4x4$CCPine2_sum+
                                 Summ4x4$CCPine3_sum+
                                 Summ4x4$CCPine4_sum+
                                 Summ4x4$CCSpruceR_sum+
                                 Summ4x4$CCSpruce1_sum+
                                 Summ4x4$CCSpruce2_sum+
                                 Summ4x4$CCSpruce3_sum+
                                 Summ4x4$CCSpruce4_sum+
                                 Summ4x4$TreedFenR_sum+
                                 Summ4x4$TreedFen1_sum+
                                 Summ4x4$TreedFen2_sum+
                                 Summ4x4$TreedFen3_sum+
                                 Summ4x4$TreedFen4_sum+
                                 Summ4x4$TreedFen5_sum+
                                 Summ4x4$TreedFen6_sum+
                                 Summ4x4$TreedFen7_sum+
                                 Summ4x4$TreedFen8_sum+
                                 Summ4x4$TreedFen9_sum+
                                 Summ4x4$TreedBogR_sum+
                                 Summ4x4$TreedBog1_sum+
                                 Summ4x4$TreedBog2_sum+
                                 Summ4x4$TreedBog3_sum+
                                 Summ4x4$TreedBog4_sum+
                                 Summ4x4$TreedBog5_sum+
                                 Summ4x4$TreedBog6_sum+
                                 Summ4x4$TreedBog7_sum+
                                 Summ4x4$TreedBog8_sum+
                                 Summ4x4$TreedBog9_sum+
                                 Summ4x4$SpruceR_sum+
                                 Summ4x4$Spruce1_sum+
                                 Summ4x4$Spruce2_sum+
                                 Summ4x4$Spruce3_sum+
                                 Summ4x4$Spruce4_sum+
                                 Summ4x4$Spruce5_sum+
                                 Summ4x4$Spruce6_sum+
                                 Summ4x4$Spruce7_sum+
                                 Summ4x4$Spruce8_sum+
                                 Summ4x4$Spruce9_sum+
                                 Summ4x4$PineR_sum+
                                 Summ4x4$Pine1_sum+
                                 Summ4x4$Pine2_sum+
                                 Summ4x4$Pine3_sum+
                                 Summ4x4$Pine4_sum+
                                 Summ4x4$Pine5_sum+
                                 Summ4x4$Pine6_sum+
                                 Summ4x4$Pine7_sum+
                                 Summ4x4$Pine8_sum+
                                 Summ4x4$Pine9_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$PropWet.600.2016<-(Summ4x4$ShrubbyBog_sum+
                             Summ4x4$ShrubbyFen_sum+
                             Summ4x4$ShrubbySwamp_sum+
                             Summ4x4$Marsh_sum+
                             Summ4x4$GraminoidFen_sum+
                             Summ4x4$TreedBogR_sum+
                             Summ4x4$TreedBog1_sum+
                             Summ4x4$TreedBog2_sum+
                             Summ4x4$TreedBog3_sum+
                             Summ4x4$TreedBog4_sum+
                             Summ4x4$TreedBog5_sum+
                             Summ4x4$TreedBog6_sum+
                             Summ4x4$TreedBog7_sum+
                             Summ4x4$TreedBog8_sum+
                             Summ4x4$TreedBog9_sum+
                             Summ4x4$TreedFenR_sum+
                             Summ4x4$TreedFen1_sum+
                             Summ4x4$TreedFen2_sum+
                             Summ4x4$TreedFen3_sum+
                             Summ4x4$TreedFen4_sum+
                             Summ4x4$TreedFen5_sum+
                             Summ4x4$TreedFen6_sum+
                             Summ4x4$TreedFen7_sum+
                             Summ4x4$TreedFen8_sum+
                             Summ4x4$TreedFen9_sum+
                             Summ4x4$TreedSwampR_sum+
                             Summ4x4$TreedSwamp1_sum+
                             Summ4x4$TreedSwamp2_sum+
                             Summ4x4$TreedSwamp3_sum+
                             Summ4x4$TreedSwamp4_sum+
                             Summ4x4$TreedSwamp5_sum+
                             Summ4x4$TreedSwamp6_sum+
                             Summ4x4$TreedSwamp7_sum+
                             Summ4x4$TreedSwamp8_sum+
                             Summ4x4$TreedSwamp9_sum+
                             Summ4x4$Water_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$TotalForest<-Summ4x4$TreedBogR_sum+
  Summ4x4$TreedBog1_sum+
  Summ4x4$TreedBog2_sum+
  Summ4x4$TreedBog3_sum+
  Summ4x4$TreedBog4_sum+
  Summ4x4$TreedBog5_sum+
  Summ4x4$TreedBog6_sum+
  Summ4x4$TreedBog7_sum+
  Summ4x4$TreedBog8_sum+
  Summ4x4$TreedBog9_sum+
  Summ4x4$TreedFenR_sum+
  Summ4x4$TreedFen1_sum+
  Summ4x4$TreedFen2_sum+
  Summ4x4$TreedFen3_sum+
  Summ4x4$TreedFen4_sum+
  Summ4x4$TreedFen5_sum+
  Summ4x4$TreedFen6_sum+
  Summ4x4$TreedFen7_sum+
  Summ4x4$TreedFen8_sum+
  Summ4x4$TreedFen9_sum+
  Summ4x4$TreedSwampR_sum+
  Summ4x4$TreedSwamp1_sum+
  Summ4x4$TreedSwamp2_sum+
  Summ4x4$TreedSwamp3_sum+
  Summ4x4$TreedSwamp4_sum+
  Summ4x4$TreedSwamp5_sum+
  Summ4x4$TreedSwamp6_sum+
  Summ4x4$TreedSwamp7_sum+
  Summ4x4$TreedSwamp8_sum+
  Summ4x4$TreedSwamp9_sum+
  Summ4x4$SpruceR_sum+
  Summ4x4$Spruce1_sum+
  Summ4x4$Spruce2_sum+
  Summ4x4$Spruce3_sum+
  Summ4x4$Spruce4_sum+
  Summ4x4$Spruce5_sum+
  Summ4x4$Spruce6_sum+
  Summ4x4$Spruce7_sum+
  Summ4x4$Spruce8_sum+
  Summ4x4$Spruce9_sum+
  Summ4x4$PineR_sum+
  Summ4x4$Pine1_sum+
  Summ4x4$Pine2_sum+
  Summ4x4$Pine3_sum+
  Summ4x4$Pine4_sum+
  Summ4x4$Pine5_sum+
  Summ4x4$Pine6_sum+
  Summ4x4$Pine7_sum+
  Summ4x4$Pine8_sum+
  Summ4x4$Pine9_sum+
  Summ4x4$DecidR_sum+
  Summ4x4$Decid1_sum+
  Summ4x4$Decid2_sum+
  Summ4x4$Decid3_sum+
  Summ4x4$Decid4_sum+
  Summ4x4$Decid5_sum+
  Summ4x4$Decid6_sum+
  Summ4x4$Decid7_sum+
  Summ4x4$Decid8_sum+
  Summ4x4$Decid9_sum+
  Summ4x4$MixedwoodR_sum+
  Summ4x4$Mixedwood1_sum+
  Summ4x4$Mixedwood2_sum+
  Summ4x4$Mixedwood3_sum+
  Summ4x4$Mixedwood4_sum+
  Summ4x4$Mixedwood5_sum+
  Summ4x4$Mixedwood6_sum+
  Summ4x4$Mixedwood7_sum+
  Summ4x4$Mixedwood8_sum+
  Summ4x4$Mixedwood9_sum+
  Summ4x4$CCPineR_sum+
  Summ4x4$CCPine1_sum+
  Summ4x4$CCPine2_sum+
  Summ4x4$CCPine3_sum+
  Summ4x4$CCPine4_sum+
  Summ4x4$CCSpruceR_sum+
  Summ4x4$CCSpruce1_sum+
  Summ4x4$CCSpruce2_sum+
  Summ4x4$CCSpruce3_sum+
  Summ4x4$CCSpruce4_sum+
  Summ4x4$CCDecidR_sum+
  Summ4x4$CCDecid1_sum+
  Summ4x4$CCDecid2_sum+
  Summ4x4$CCDecid3_sum+
  Summ4x4$CCDecid4_sum+
  Summ4x4$CCMixedwoodR_sum+
  Summ4x4$CCMixedwood1_sum+
  Summ4x4$CCMixedwood2_sum+
  Summ4x4$CCMixedwood3_sum+
  Summ4x4$CCMixedwood4_sum

Summ4x4$Wt.For.Age.600.2016<-(Summ4x4$TreedBogR_sum*4.5+#0-9 years
                                Summ4x4$TreedBog1_sum*10+#10-19 years
                                Summ4x4$TreedBog2_sum*20+#20-39 years
                                Summ4x4$TreedBog3_sum*40+#40-59 years
                                Summ4x4$TreedBog4_sum*60+#60-79 years
                                Summ4x4$TreedBog5_sum*80+#80-99 years
                                Summ4x4$TreedBog6_sum*100+#100-119 years
                                Summ4x4$TreedBog7_sum*120+#120-139 years
                                Summ4x4$TreedBog8_sum*140+#140-159 years
                                Summ4x4$TreedBog9_sum*160+#160 years and older
                                Summ4x4$TreedFenR_sum*4.5+
                                Summ4x4$TreedFen1_sum*10+
                                Summ4x4$TreedFen2_sum*20+
                                Summ4x4$TreedFen3_sum*40+
                                Summ4x4$TreedFen4_sum*60+
                                Summ4x4$TreedFen5_sum*80+
                                Summ4x4$TreedFen6_sum*100+
                                Summ4x4$TreedFen7_sum*120+
                                Summ4x4$TreedFen8_sum*140+
                                Summ4x4$TreedFen9_sum*160+
                                Summ4x4$TreedSwampR_sum*4.5+
                                Summ4x4$TreedSwamp1_sum*10+
                                Summ4x4$TreedSwamp2_sum*20+
                                Summ4x4$TreedSwamp3_sum*40+
                                Summ4x4$TreedSwamp4_sum*60+
                                Summ4x4$TreedSwamp5_sum*80+
                                Summ4x4$TreedSwamp6_sum*100+
                                Summ4x4$TreedSwamp7_sum*120+
                                Summ4x4$TreedSwamp8_sum*140+
                                Summ4x4$TreedSwamp9_sum*160+
                                Summ4x4$SpruceR_sum*4.5+
                                Summ4x4$Spruce1_sum*10+
                                Summ4x4$Spruce2_sum*20+
                                Summ4x4$Spruce3_sum*40+
                                Summ4x4$Spruce4_sum*60+
                                Summ4x4$Spruce5_sum*80+
                                Summ4x4$Spruce6_sum*100+
                                Summ4x4$Spruce7_sum*120+
                                Summ4x4$Spruce8_sum*140+
                                Summ4x4$Spruce9_sum*160+
                                Summ4x4$PineR_sum*4.5+
                                Summ4x4$Pine1_sum*10+
                                Summ4x4$Pine2_sum*20+
                                Summ4x4$Pine3_sum*40+
                                Summ4x4$Pine4_sum*60+
                                Summ4x4$Pine5_sum*80+
                                Summ4x4$Pine6_sum*100+
                                Summ4x4$Pine7_sum*120+
                                Summ4x4$Pine8_sum*140+
                                Summ4x4$Pine9_sum*160+
                                Summ4x4$DecidR_sum*4.5+
                                Summ4x4$Decid1_sum*10+
                                Summ4x4$Decid2_sum*20+
                                Summ4x4$Decid3_sum*40+
                                Summ4x4$Decid4_sum*60+
                                Summ4x4$Decid5_sum*80+
                                Summ4x4$Decid6_sum*100+
                                Summ4x4$Decid7_sum*120+
                                Summ4x4$Decid8_sum*140+
                                Summ4x4$Decid9_sum*160+
                                Summ4x4$MixedwoodR_sum*4.5+
                                Summ4x4$Mixedwood1_sum*10+
                                Summ4x4$Mixedwood2_sum*20+
                                Summ4x4$Mixedwood3_sum*40+
                                Summ4x4$Mixedwood4_sum*60+
                                Summ4x4$Mixedwood5_sum*80+
                                Summ4x4$Mixedwood6_sum*100+
                                Summ4x4$Mixedwood7_sum*120+
                                Summ4x4$Mixedwood8_sum*140+
                                Summ4x4$Mixedwood9_sum*160+
                                Summ4x4$CCPineR_sum*4.5+
                                Summ4x4$CCPine1_sum*10+
                                Summ4x4$CCPine2_sum*20+
                                Summ4x4$CCPine3_sum*40+
                                Summ4x4$CCPine4_sum*60+
                                Summ4x4$CCSpruceR_sum*4.5+
                                Summ4x4$CCSpruce1_sum*10+
                                Summ4x4$CCSpruce2_sum*20+
                                Summ4x4$CCSpruce3_sum*40+
                                Summ4x4$CCSpruce4_sum*60+
                                Summ4x4$CCDecidR_sum*4.5+
                                Summ4x4$CCDecid1_sum*10+
                                Summ4x4$CCDecid2_sum*20+
                                Summ4x4$CCDecid3_sum*40+
                                Summ4x4$CCDecid4_sum*60+
                                Summ4x4$CCMixedwoodR_sum*4.5+
                                Summ4x4$CCMixedwood1_sum*10+
                                Summ4x4$CCMixedwood2_sum*20+
                                Summ4x4$CCMixedwood3_sum*40+
                                Summ4x4$CCMixedwood4_sum*60)/Summ4x4$TotalForest  

Summ4x4$PropSoftLinear.600.2016<-(Summ4x4$SeismicLineNarrow_sum+
                                    Summ4x4$SeismicLineWide_sum+
                                    Summ4x4$Pipeline_sum+
                                    Summ4x4$TransmissionLine_sum+
                                    Summ4x4$RoadTrailVegetated_sum+
                                    Summ4x4$RoadVegetatedVerge_sum+
                                    Summ4x4$RailVegetatedVerge_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$PropHardLinear.600.2016<-(Summ4x4$RoadHardSurface_sum+
                                    Summ4x4$RailHardSurface_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$PropSoftPoly.600.2016<-(Summ4x4$OtherDisturbedVegetation_sum+
                                  Summ4x4$WellSite_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$PropHardPoly.600.2016<-(Summ4x4$CultivationCrop_sum+
                                  Summ4x4$CultivationAbandoned_sum+
                                  Summ4x4$CultivationRoughPasture_sum+
                                  Summ4x4$CultivationTamePasture_sum+
                                  Summ4x4$HighDensityLivestockOperation_sum+
                                  Summ4x4$BorrowpitsDugoutsSumps_sum+
                                  Summ4x4$MunicipalWaterSewage_sum+
                                  Summ4x4$Reservoirs_sum+
                                  Summ4x4$Canals_sum+
                                  Summ4x4$UrbanIndustrial_sum+
                                  Summ4x4$UrbanResidence_sum+
                                  Summ4x4$RuralResidentialIndustrial_sum+
                                  Summ4x4$IndustrialSiteRural_sum+
                                  Summ4x4$WindGenerationFacility_sum+
                                  Summ4x4$MineSite_sum+
                                  Summ4x4$PeatMine_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ4x4$zerolndscp<-str_sub(Summ4x4$L4x4, -2, -1)#distinguishes '0' from '10', '20'
Summ4x4<-Summ4x4[!Summ4x4$zerolndscp=="-0",]
write.csv(Summ4x4, file="0_data/processed/different scales/vegHF.600.2016.4x4.csv")

Summ5x5<-vegHF.600.2016%>%
  group_by(L5x5)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ5x5<-data.frame(Summ5x5)
Summ5x5$PropConifer.600.2016<-(Summ5x5$CCPineR_sum+
                                 Summ5x5$CCPine1_sum+
                                 Summ5x5$CCPine2_sum+
                                 Summ5x5$CCPine3_sum+
                                 Summ5x5$CCPine4_sum+
                                 Summ5x5$CCSpruceR_sum+
                                 Summ5x5$CCSpruce1_sum+
                                 Summ5x5$CCSpruce2_sum+
                                 Summ5x5$CCSpruce3_sum+
                                 Summ5x5$CCSpruce4_sum+
                                 Summ5x5$TreedFenR_sum+
                                 Summ5x5$TreedFen1_sum+
                                 Summ5x5$TreedFen2_sum+
                                 Summ5x5$TreedFen3_sum+
                                 Summ5x5$TreedFen4_sum+
                                 Summ5x5$TreedFen5_sum+
                                 Summ5x5$TreedFen6_sum+
                                 Summ5x5$TreedFen7_sum+
                                 Summ5x5$TreedFen8_sum+
                                 Summ5x5$TreedFen9_sum+
                                 Summ5x5$TreedBogR_sum+
                                 Summ5x5$TreedBog1_sum+
                                 Summ5x5$TreedBog2_sum+
                                 Summ5x5$TreedBog3_sum+
                                 Summ5x5$TreedBog4_sum+
                                 Summ5x5$TreedBog5_sum+
                                 Summ5x5$TreedBog6_sum+
                                 Summ5x5$TreedBog7_sum+
                                 Summ5x5$TreedBog8_sum+
                                 Summ5x5$TreedBog9_sum+
                                 Summ5x5$SpruceR_sum+
                                 Summ5x5$Spruce1_sum+
                                 Summ5x5$Spruce2_sum+
                                 Summ5x5$Spruce3_sum+
                                 Summ5x5$Spruce4_sum+
                                 Summ5x5$Spruce5_sum+
                                 Summ5x5$Spruce6_sum+
                                 Summ5x5$Spruce7_sum+
                                 Summ5x5$Spruce8_sum+
                                 Summ5x5$Spruce9_sum+
                                 Summ5x5$PineR_sum+
                                 Summ5x5$Pine1_sum+
                                 Summ5x5$Pine2_sum+
                                 Summ5x5$Pine3_sum+
                                 Summ5x5$Pine4_sum+
                                 Summ5x5$Pine5_sum+
                                 Summ5x5$Pine6_sum+
                                 Summ5x5$Pine7_sum+
                                 Summ5x5$Pine8_sum+
                                 Summ5x5$Pine9_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$PropWet.600.2016<-(Summ5x5$ShrubbyBog_sum+
                             Summ5x5$ShrubbyFen_sum+
                             Summ5x5$ShrubbySwamp_sum+
                             Summ5x5$Marsh_sum+
                             Summ5x5$GraminoidFen_sum+
                             Summ5x5$TreedBogR_sum+
                             Summ5x5$TreedBog1_sum+
                             Summ5x5$TreedBog2_sum+
                             Summ5x5$TreedBog3_sum+
                             Summ5x5$TreedBog4_sum+
                             Summ5x5$TreedBog5_sum+
                             Summ5x5$TreedBog6_sum+
                             Summ5x5$TreedBog7_sum+
                             Summ5x5$TreedBog8_sum+
                             Summ5x5$TreedBog9_sum+
                             Summ5x5$TreedFenR_sum+
                             Summ5x5$TreedFen1_sum+
                             Summ5x5$TreedFen2_sum+
                             Summ5x5$TreedFen3_sum+
                             Summ5x5$TreedFen4_sum+
                             Summ5x5$TreedFen5_sum+
                             Summ5x5$TreedFen6_sum+
                             Summ5x5$TreedFen7_sum+
                             Summ5x5$TreedFen8_sum+
                             Summ5x5$TreedFen9_sum+
                             Summ5x5$TreedSwampR_sum+
                             Summ5x5$TreedSwamp1_sum+
                             Summ5x5$TreedSwamp2_sum+
                             Summ5x5$TreedSwamp3_sum+
                             Summ5x5$TreedSwamp4_sum+
                             Summ5x5$TreedSwamp5_sum+
                             Summ5x5$TreedSwamp6_sum+
                             Summ5x5$TreedSwamp7_sum+
                             Summ5x5$TreedSwamp8_sum+
                             Summ5x5$TreedSwamp9_sum+
                             Summ5x5$Water_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$TotalForest<-Summ5x5$TreedBogR_sum+
  Summ5x5$TreedBog1_sum+
  Summ5x5$TreedBog2_sum+
  Summ5x5$TreedBog3_sum+
  Summ5x5$TreedBog4_sum+
  Summ5x5$TreedBog5_sum+
  Summ5x5$TreedBog6_sum+
  Summ5x5$TreedBog7_sum+
  Summ5x5$TreedBog8_sum+
  Summ5x5$TreedBog9_sum+
  Summ5x5$TreedFenR_sum+
  Summ5x5$TreedFen1_sum+
  Summ5x5$TreedFen2_sum+
  Summ5x5$TreedFen3_sum+
  Summ5x5$TreedFen4_sum+
  Summ5x5$TreedFen5_sum+
  Summ5x5$TreedFen6_sum+
  Summ5x5$TreedFen7_sum+
  Summ5x5$TreedFen8_sum+
  Summ5x5$TreedFen9_sum+
  Summ5x5$TreedSwampR_sum+
  Summ5x5$TreedSwamp1_sum+
  Summ5x5$TreedSwamp2_sum+
  Summ5x5$TreedSwamp3_sum+
  Summ5x5$TreedSwamp4_sum+
  Summ5x5$TreedSwamp5_sum+
  Summ5x5$TreedSwamp6_sum+
  Summ5x5$TreedSwamp7_sum+
  Summ5x5$TreedSwamp8_sum+
  Summ5x5$TreedSwamp9_sum+
  Summ5x5$SpruceR_sum+
  Summ5x5$Spruce1_sum+
  Summ5x5$Spruce2_sum+
  Summ5x5$Spruce3_sum+
  Summ5x5$Spruce4_sum+
  Summ5x5$Spruce5_sum+
  Summ5x5$Spruce6_sum+
  Summ5x5$Spruce7_sum+
  Summ5x5$Spruce8_sum+
  Summ5x5$Spruce9_sum+
  Summ5x5$PineR_sum+
  Summ5x5$Pine1_sum+
  Summ5x5$Pine2_sum+
  Summ5x5$Pine3_sum+
  Summ5x5$Pine4_sum+
  Summ5x5$Pine5_sum+
  Summ5x5$Pine6_sum+
  Summ5x5$Pine7_sum+
  Summ5x5$Pine8_sum+
  Summ5x5$Pine9_sum+
  Summ5x5$DecidR_sum+
  Summ5x5$Decid1_sum+
  Summ5x5$Decid2_sum+
  Summ5x5$Decid3_sum+
  Summ5x5$Decid4_sum+
  Summ5x5$Decid5_sum+
  Summ5x5$Decid6_sum+
  Summ5x5$Decid7_sum+
  Summ5x5$Decid8_sum+
  Summ5x5$Decid9_sum+
  Summ5x5$MixedwoodR_sum+
  Summ5x5$Mixedwood1_sum+
  Summ5x5$Mixedwood2_sum+
  Summ5x5$Mixedwood3_sum+
  Summ5x5$Mixedwood4_sum+
  Summ5x5$Mixedwood5_sum+
  Summ5x5$Mixedwood6_sum+
  Summ5x5$Mixedwood7_sum+
  Summ5x5$Mixedwood8_sum+
  Summ5x5$Mixedwood9_sum+
  Summ5x5$CCPineR_sum+
  Summ5x5$CCPine1_sum+
  Summ5x5$CCPine2_sum+
  Summ5x5$CCPine3_sum+
  Summ5x5$CCPine4_sum+
  Summ5x5$CCSpruceR_sum+
  Summ5x5$CCSpruce1_sum+
  Summ5x5$CCSpruce2_sum+
  Summ5x5$CCSpruce3_sum+
  Summ5x5$CCSpruce4_sum+
  Summ5x5$CCDecidR_sum+
  Summ5x5$CCDecid1_sum+
  Summ5x5$CCDecid2_sum+
  Summ5x5$CCDecid3_sum+
  Summ5x5$CCDecid4_sum+
  Summ5x5$CCMixedwoodR_sum+
  Summ5x5$CCMixedwood1_sum+
  Summ5x5$CCMixedwood2_sum+
  Summ5x5$CCMixedwood3_sum+
  Summ5x5$CCMixedwood4_sum

Summ5x5$Wt.For.Age.600.2016<-(Summ5x5$TreedBogR_sum*4.5+#0-9 years
                                Summ5x5$TreedBog1_sum*10+#10-19 years
                                Summ5x5$TreedBog2_sum*20+#20-39 years
                                Summ5x5$TreedBog3_sum*40+#40-59 years
                                Summ5x5$TreedBog4_sum*60+#60-79 years
                                Summ5x5$TreedBog5_sum*80+#80-99 years
                                Summ5x5$TreedBog6_sum*100+#100-119 years
                                Summ5x5$TreedBog7_sum*120+#120-139 years
                                Summ5x5$TreedBog8_sum*140+#140-159 years
                                Summ5x5$TreedBog9_sum*160+#160 years and older
                                Summ5x5$TreedFenR_sum*4.5+
                                Summ5x5$TreedFen1_sum*10+
                                Summ5x5$TreedFen2_sum*20+
                                Summ5x5$TreedFen3_sum*40+
                                Summ5x5$TreedFen4_sum*60+
                                Summ5x5$TreedFen5_sum*80+
                                Summ5x5$TreedFen6_sum*100+
                                Summ5x5$TreedFen7_sum*120+
                                Summ5x5$TreedFen8_sum*140+
                                Summ5x5$TreedFen9_sum*160+
                                Summ5x5$TreedSwampR_sum*4.5+
                                Summ5x5$TreedSwamp1_sum*10+
                                Summ5x5$TreedSwamp2_sum*20+
                                Summ5x5$TreedSwamp3_sum*40+
                                Summ5x5$TreedSwamp4_sum*60+
                                Summ5x5$TreedSwamp5_sum*80+
                                Summ5x5$TreedSwamp6_sum*100+
                                Summ5x5$TreedSwamp7_sum*120+
                                Summ5x5$TreedSwamp8_sum*140+
                                Summ5x5$TreedSwamp9_sum*160+
                                Summ5x5$SpruceR_sum*4.5+
                                Summ5x5$Spruce1_sum*10+
                                Summ5x5$Spruce2_sum*20+
                                Summ5x5$Spruce3_sum*40+
                                Summ5x5$Spruce4_sum*60+
                                Summ5x5$Spruce5_sum*80+
                                Summ5x5$Spruce6_sum*100+
                                Summ5x5$Spruce7_sum*120+
                                Summ5x5$Spruce8_sum*140+
                                Summ5x5$Spruce9_sum*160+
                                Summ5x5$PineR_sum*4.5+
                                Summ5x5$Pine1_sum*10+
                                Summ5x5$Pine2_sum*20+
                                Summ5x5$Pine3_sum*40+
                                Summ5x5$Pine4_sum*60+
                                Summ5x5$Pine5_sum*80+
                                Summ5x5$Pine6_sum*100+
                                Summ5x5$Pine7_sum*120+
                                Summ5x5$Pine8_sum*140+
                                Summ5x5$Pine9_sum*160+
                                Summ5x5$DecidR_sum*4.5+
                                Summ5x5$Decid1_sum*10+
                                Summ5x5$Decid2_sum*20+
                                Summ5x5$Decid3_sum*40+
                                Summ5x5$Decid4_sum*60+
                                Summ5x5$Decid5_sum*80+
                                Summ5x5$Decid6_sum*100+
                                Summ5x5$Decid7_sum*120+
                                Summ5x5$Decid8_sum*140+
                                Summ5x5$Decid9_sum*160+
                                Summ5x5$MixedwoodR_sum*4.5+
                                Summ5x5$Mixedwood1_sum*10+
                                Summ5x5$Mixedwood2_sum*20+
                                Summ5x5$Mixedwood3_sum*40+
                                Summ5x5$Mixedwood4_sum*60+
                                Summ5x5$Mixedwood5_sum*80+
                                Summ5x5$Mixedwood6_sum*100+
                                Summ5x5$Mixedwood7_sum*120+
                                Summ5x5$Mixedwood8_sum*140+
                                Summ5x5$Mixedwood9_sum*160+
                                Summ5x5$CCPineR_sum*4.5+
                                Summ5x5$CCPine1_sum*10+
                                Summ5x5$CCPine2_sum*20+
                                Summ5x5$CCPine3_sum*40+
                                Summ5x5$CCPine4_sum*60+
                                Summ5x5$CCSpruceR_sum*4.5+
                                Summ5x5$CCSpruce1_sum*10+
                                Summ5x5$CCSpruce2_sum*20+
                                Summ5x5$CCSpruce3_sum*40+
                                Summ5x5$CCSpruce4_sum*60+
                                Summ5x5$CCDecidR_sum*4.5+
                                Summ5x5$CCDecid1_sum*10+
                                Summ5x5$CCDecid2_sum*20+
                                Summ5x5$CCDecid3_sum*40+
                                Summ5x5$CCDecid4_sum*60+
                                Summ5x5$CCMixedwoodR_sum*4.5+
                                Summ5x5$CCMixedwood1_sum*10+
                                Summ5x5$CCMixedwood2_sum*20+
                                Summ5x5$CCMixedwood3_sum*40+
                                Summ5x5$CCMixedwood4_sum*60)/Summ5x5$TotalForest  

Summ5x5$PropSoftLinear.600.2016<-(Summ5x5$SeismicLineNarrow_sum+
                                    Summ5x5$SeismicLineWide_sum+
                                    Summ5x5$Pipeline_sum+
                                    Summ5x5$TransmissionLine_sum+
                                    Summ5x5$RoadTrailVegetated_sum+
                                    Summ5x5$RoadVegetatedVerge_sum+
                                    Summ5x5$RailVegetatedVerge_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$PropHardLinear.600.2016<-(Summ5x5$RoadHardSurface_sum+
                                    Summ5x5$RailHardSurface_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$PropSoftPoly.600.2016<-(Summ5x5$OtherDisturbedVegetation_sum+
                                  Summ5x5$WellSite_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$PropHardPoly.600.2016<-(Summ5x5$CultivationCrop_sum+
                                  Summ5x5$CultivationAbandoned_sum+
                                  Summ5x5$CultivationRoughPasture_sum+
                                  Summ5x5$CultivationTamePasture_sum+
                                  Summ5x5$HighDensityLivestockOperation_sum+
                                  Summ5x5$BorrowpitsDugoutsSumps_sum+
                                  Summ5x5$MunicipalWaterSewage_sum+
                                  Summ5x5$Reservoirs_sum+
                                  Summ5x5$Canals_sum+
                                  Summ5x5$UrbanIndustrial_sum+
                                  Summ5x5$UrbanResidence_sum+
                                  Summ5x5$RuralResidentialIndustrial_sum+
                                  Summ5x5$IndustrialSiteRural_sum+
                                  Summ5x5$WindGenerationFacility_sum+
                                  Summ5x5$MineSite_sum+
                                  Summ5x5$PeatMine_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ5x5$zerolndscp<-str_sub(Summ5x5$L5x5, -2, -1)#distinguishes '0' from '10', '20'
Summ5x5<-Summ5x5[!Summ5x5$zerolndscp=="-0",]
write.csv(Summ5x5, file="0_data/processed/different scales/vegHF.600.2016.5x5.csv")


#600-m square data from 2017
vegHF.600.2017<-read.csv("0_data/processed/10_vegHF36000m2scale_2017_stationgridassignment.csv", header=TRUE)
str(vegHF.600.2017)#1500 obs

#Create unique "landscapes"
vegHF.600.2017$L2x2<-paste0("L-",vegHF.600.2017$Gridnum,"-",vegHF.600.2017$g2x2)
vegHF.600.2017$L3x3<-paste0("L-",vegHF.600.2017$Gridnum,"-",vegHF.600.2017$g3x3)
vegHF.600.2017$L4x4<-paste0("L-",vegHF.600.2017$Gridnum,"-",vegHF.600.2017$g4x4)
vegHF.600.2017$L5x5<-paste0("L-",vegHF.600.2017$Gridnum,"-",vegHF.600.2017$g5x5)
#The "L-" prefix for landscape is to keep the unique landscape ID
#from being converted to a date


Summ2x2<-vegHF.600.2017%>%
  group_by(L2x2)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ2x2<-data.frame(Summ2x2)
Summ2x2$PropConifer.600.2017<-(Summ2x2$CCPineR_sum+
                                 Summ2x2$CCPine1_sum+
                                 Summ2x2$CCPine2_sum+
                                 Summ2x2$CCPine3_sum+
                                 Summ2x2$CCPine4_sum+
                                 Summ2x2$CCSpruceR_sum+
                                 Summ2x2$CCSpruce1_sum+
                                 Summ2x2$CCSpruce2_sum+
                                 Summ2x2$CCSpruce3_sum+
                                 Summ2x2$CCSpruce4_sum+
                                 Summ2x2$TreedFenR_sum+
                                 Summ2x2$TreedFen1_sum+
                                 Summ2x2$TreedFen2_sum+
                                 Summ2x2$TreedFen3_sum+
                                 Summ2x2$TreedFen4_sum+
                                 Summ2x2$TreedFen5_sum+
                                 Summ2x2$TreedFen6_sum+
                                 Summ2x2$TreedFen7_sum+
                                 Summ2x2$TreedFen8_sum+
                                 Summ2x2$TreedFen9_sum+
                                 Summ2x2$TreedBogR_sum+
                                 Summ2x2$TreedBog1_sum+
                                 Summ2x2$TreedBog2_sum+
                                 Summ2x2$TreedBog3_sum+
                                 Summ2x2$TreedBog4_sum+
                                 Summ2x2$TreedBog5_sum+
                                 Summ2x2$TreedBog6_sum+
                                 Summ2x2$TreedBog7_sum+
                                 Summ2x2$TreedBog8_sum+
                                 Summ2x2$TreedBog9_sum+
                                 Summ2x2$SpruceR_sum+
                                 Summ2x2$Spruce1_sum+
                                 Summ2x2$Spruce2_sum+
                                 Summ2x2$Spruce3_sum+
                                 Summ2x2$Spruce4_sum+
                                 Summ2x2$Spruce5_sum+
                                 Summ2x2$Spruce6_sum+
                                 Summ2x2$Spruce7_sum+
                                 Summ2x2$Spruce8_sum+
                                 Summ2x2$Spruce9_sum+
                                 Summ2x2$PineR_sum+
                                 Summ2x2$Pine1_sum+
                                 Summ2x2$Pine2_sum+
                                 Summ2x2$Pine3_sum+
                                 Summ2x2$Pine4_sum+
                                 Summ2x2$Pine5_sum+
                                 Summ2x2$Pine6_sum+
                                 Summ2x2$Pine7_sum+
                                 Summ2x2$Pine8_sum+
                                 Summ2x2$Pine9_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$PropWet.600.2017<-(Summ2x2$ShrubbyBog_sum+
                             Summ2x2$ShrubbyFen_sum+
                             Summ2x2$ShrubbySwamp_sum+
                             Summ2x2$Marsh_sum+
                             Summ2x2$GraminoidFen_sum+
                             Summ2x2$TreedBogR_sum+
                             Summ2x2$TreedBog1_sum+
                             Summ2x2$TreedBog2_sum+
                             Summ2x2$TreedBog3_sum+
                             Summ2x2$TreedBog4_sum+
                             Summ2x2$TreedBog5_sum+
                             Summ2x2$TreedBog6_sum+
                             Summ2x2$TreedBog7_sum+
                             Summ2x2$TreedBog8_sum+
                             Summ2x2$TreedBog9_sum+
                             Summ2x2$TreedFenR_sum+
                             Summ2x2$TreedFen1_sum+
                             Summ2x2$TreedFen2_sum+
                             Summ2x2$TreedFen3_sum+
                             Summ2x2$TreedFen4_sum+
                             Summ2x2$TreedFen5_sum+
                             Summ2x2$TreedFen6_sum+
                             Summ2x2$TreedFen7_sum+
                             Summ2x2$TreedFen8_sum+
                             Summ2x2$TreedFen9_sum+
                             Summ2x2$TreedSwampR_sum+
                             Summ2x2$TreedSwamp1_sum+
                             Summ2x2$TreedSwamp2_sum+
                             Summ2x2$TreedSwamp3_sum+
                             Summ2x2$TreedSwamp4_sum+
                             Summ2x2$TreedSwamp5_sum+
                             Summ2x2$TreedSwamp6_sum+
                             Summ2x2$TreedSwamp7_sum+
                             Summ2x2$TreedSwamp8_sum+
                             Summ2x2$TreedSwamp9_sum+
                             Summ2x2$Water_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$TotalForest<-Summ2x2$TreedBogR_sum+
  Summ2x2$TreedBog1_sum+
  Summ2x2$TreedBog2_sum+
  Summ2x2$TreedBog3_sum+
  Summ2x2$TreedBog4_sum+
  Summ2x2$TreedBog5_sum+
  Summ2x2$TreedBog6_sum+
  Summ2x2$TreedBog7_sum+
  Summ2x2$TreedBog8_sum+
  Summ2x2$TreedBog9_sum+
  Summ2x2$TreedFenR_sum+
  Summ2x2$TreedFen1_sum+
  Summ2x2$TreedFen2_sum+
  Summ2x2$TreedFen3_sum+
  Summ2x2$TreedFen4_sum+
  Summ2x2$TreedFen5_sum+
  Summ2x2$TreedFen6_sum+
  Summ2x2$TreedFen7_sum+
  Summ2x2$TreedFen8_sum+
  Summ2x2$TreedFen9_sum+
  Summ2x2$TreedSwampR_sum+
  Summ2x2$TreedSwamp1_sum+
  Summ2x2$TreedSwamp2_sum+
  Summ2x2$TreedSwamp3_sum+
  Summ2x2$TreedSwamp4_sum+
  Summ2x2$TreedSwamp5_sum+
  Summ2x2$TreedSwamp6_sum+
  Summ2x2$TreedSwamp7_sum+
  Summ2x2$TreedSwamp8_sum+
  Summ2x2$TreedSwamp9_sum+
  Summ2x2$SpruceR_sum+
  Summ2x2$Spruce1_sum+
  Summ2x2$Spruce2_sum+
  Summ2x2$Spruce3_sum+
  Summ2x2$Spruce4_sum+
  Summ2x2$Spruce5_sum+
  Summ2x2$Spruce6_sum+
  Summ2x2$Spruce7_sum+
  Summ2x2$Spruce8_sum+
  Summ2x2$Spruce9_sum+
  Summ2x2$PineR_sum+
  Summ2x2$Pine1_sum+
  Summ2x2$Pine2_sum+
  Summ2x2$Pine3_sum+
  Summ2x2$Pine4_sum+
  Summ2x2$Pine5_sum+
  Summ2x2$Pine6_sum+
  Summ2x2$Pine7_sum+
  Summ2x2$Pine8_sum+
  Summ2x2$Pine9_sum+
  Summ2x2$DecidR_sum+
  Summ2x2$Decid1_sum+
  Summ2x2$Decid2_sum+
  Summ2x2$Decid3_sum+
  Summ2x2$Decid4_sum+
  Summ2x2$Decid5_sum+
  Summ2x2$Decid6_sum+
  Summ2x2$Decid7_sum+
  Summ2x2$Decid8_sum+
  Summ2x2$Decid9_sum+
  Summ2x2$MixedwoodR_sum+
  Summ2x2$Mixedwood1_sum+
  Summ2x2$Mixedwood2_sum+
  Summ2x2$Mixedwood3_sum+
  Summ2x2$Mixedwood4_sum+
  Summ2x2$Mixedwood5_sum+
  Summ2x2$Mixedwood6_sum+
  Summ2x2$Mixedwood7_sum+
  Summ2x2$Mixedwood8_sum+
  Summ2x2$Mixedwood9_sum+
  Summ2x2$CCPineR_sum+
  Summ2x2$CCPine1_sum+
  Summ2x2$CCPine2_sum+
  Summ2x2$CCPine3_sum+
  Summ2x2$CCPine4_sum+
  Summ2x2$CCSpruceR_sum+
  Summ2x2$CCSpruce1_sum+
  Summ2x2$CCSpruce2_sum+
  Summ2x2$CCSpruce3_sum+
  Summ2x2$CCSpruce4_sum+
  Summ2x2$CCDecidR_sum+
  Summ2x2$CCDecid1_sum+
  Summ2x2$CCDecid2_sum+
  Summ2x2$CCDecid3_sum+
  Summ2x2$CCDecid4_sum+
  Summ2x2$CCMixedwoodR_sum+
  Summ2x2$CCMixedwood1_sum+
  Summ2x2$CCMixedwood2_sum+
  Summ2x2$CCMixedwood3_sum+
  Summ2x2$CCMixedwood4_sum

Summ2x2$Wt.For.Age.600.2017<-(Summ2x2$TreedBogR_sum*4.5+#0-9 years
                                Summ2x2$TreedBog1_sum*10+#10-19 years
                                Summ2x2$TreedBog2_sum*20+#20-39 years
                                Summ2x2$TreedBog3_sum*40+#40-59 years
                                Summ2x2$TreedBog4_sum*60+#60-79 years
                                Summ2x2$TreedBog5_sum*80+#80-99 years
                                Summ2x2$TreedBog6_sum*100+#100-119 years
                                Summ2x2$TreedBog7_sum*120+#120-139 years
                                Summ2x2$TreedBog8_sum*140+#140-159 years
                                Summ2x2$TreedBog9_sum*160+#160 years and older
                                Summ2x2$TreedFenR_sum*4.5+
                                Summ2x2$TreedFen1_sum*10+
                                Summ2x2$TreedFen2_sum*20+
                                Summ2x2$TreedFen3_sum*40+
                                Summ2x2$TreedFen4_sum*60+
                                Summ2x2$TreedFen5_sum*80+
                                Summ2x2$TreedFen6_sum*100+
                                Summ2x2$TreedFen7_sum*120+
                                Summ2x2$TreedFen8_sum*140+
                                Summ2x2$TreedFen9_sum*160+
                                Summ2x2$TreedSwampR_sum*4.5+
                                Summ2x2$TreedSwamp1_sum*10+
                                Summ2x2$TreedSwamp2_sum*20+
                                Summ2x2$TreedSwamp3_sum*40+
                                Summ2x2$TreedSwamp4_sum*60+
                                Summ2x2$TreedSwamp5_sum*80+
                                Summ2x2$TreedSwamp6_sum*100+
                                Summ2x2$TreedSwamp7_sum*120+
                                Summ2x2$TreedSwamp8_sum*140+
                                Summ2x2$TreedSwamp9_sum*160+
                                Summ2x2$SpruceR_sum*4.5+
                                Summ2x2$Spruce1_sum*10+
                                Summ2x2$Spruce2_sum*20+
                                Summ2x2$Spruce3_sum*40+
                                Summ2x2$Spruce4_sum*60+
                                Summ2x2$Spruce5_sum*80+
                                Summ2x2$Spruce6_sum*100+
                                Summ2x2$Spruce7_sum*120+
                                Summ2x2$Spruce8_sum*140+
                                Summ2x2$Spruce9_sum*160+
                                Summ2x2$PineR_sum*4.5+
                                Summ2x2$Pine1_sum*10+
                                Summ2x2$Pine2_sum*20+
                                Summ2x2$Pine3_sum*40+
                                Summ2x2$Pine4_sum*60+
                                Summ2x2$Pine5_sum*80+
                                Summ2x2$Pine6_sum*100+
                                Summ2x2$Pine7_sum*120+
                                Summ2x2$Pine8_sum*140+
                                Summ2x2$Pine9_sum*160+
                                Summ2x2$DecidR_sum*4.5+
                                Summ2x2$Decid1_sum*10+
                                Summ2x2$Decid2_sum*20+
                                Summ2x2$Decid3_sum*40+
                                Summ2x2$Decid4_sum*60+
                                Summ2x2$Decid5_sum*80+
                                Summ2x2$Decid6_sum*100+
                                Summ2x2$Decid7_sum*120+
                                Summ2x2$Decid8_sum*140+
                                Summ2x2$Decid9_sum*160+
                                Summ2x2$MixedwoodR_sum*4.5+
                                Summ2x2$Mixedwood1_sum*10+
                                Summ2x2$Mixedwood2_sum*20+
                                Summ2x2$Mixedwood3_sum*40+
                                Summ2x2$Mixedwood4_sum*60+
                                Summ2x2$Mixedwood5_sum*80+
                                Summ2x2$Mixedwood6_sum*100+
                                Summ2x2$Mixedwood7_sum*120+
                                Summ2x2$Mixedwood8_sum*140+
                                Summ2x2$Mixedwood9_sum*160+
                                Summ2x2$CCPineR_sum*4.5+
                                Summ2x2$CCPine1_sum*10+
                                Summ2x2$CCPine2_sum*20+
                                Summ2x2$CCPine3_sum*40+
                                Summ2x2$CCPine4_sum*60+
                                Summ2x2$CCSpruceR_sum*4.5+
                                Summ2x2$CCSpruce1_sum*10+
                                Summ2x2$CCSpruce2_sum*20+
                                Summ2x2$CCSpruce3_sum*40+
                                Summ2x2$CCSpruce4_sum*60+
                                Summ2x2$CCDecidR_sum*4.5+
                                Summ2x2$CCDecid1_sum*10+
                                Summ2x2$CCDecid2_sum*20+
                                Summ2x2$CCDecid3_sum*40+
                                Summ2x2$CCDecid4_sum*60+
                                Summ2x2$CCMixedwoodR_sum*4.5+
                                Summ2x2$CCMixedwood1_sum*10+
                                Summ2x2$CCMixedwood2_sum*20+
                                Summ2x2$CCMixedwood3_sum*40+
                                Summ2x2$CCMixedwood4_sum*60)/Summ2x2$TotalForest  

Summ2x2$PropSoftLinear.600.2017<-(Summ2x2$SeismicLineNarrow_sum+
                                    Summ2x2$SeismicLineWide_sum+
                                    Summ2x2$Pipeline_sum+
                                    Summ2x2$TransmissionLine_sum+
                                    Summ2x2$RoadTrailVegetated_sum+
                                    Summ2x2$RoadVegetatedVerge_sum+
                                    Summ2x2$RailVegetatedVerge_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$PropHardLinear.600.2017<-(Summ2x2$RoadHardSurface_sum+
                                    Summ2x2$RailHardSurface_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$PropSoftPoly.600.2017<-(Summ2x2$OtherDisturbedVegetation_sum+
                                  Summ2x2$WellSite_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

Summ2x2$PropHardPoly.600.2017<-(Summ2x2$CultivationCrop_sum+
                                  Summ2x2$CultivationAbandoned_sum+
                                  Summ2x2$CultivationRoughPasture_sum+
                                  Summ2x2$CultivationTamePasture_sum+
                                  Summ2x2$HighDensityLivestockOperation_sum+
                                  Summ2x2$BorrowpitsDugoutsSumps_sum+
                                  Summ2x2$MunicipalWaterSewage_sum+
                                  Summ2x2$Reservoirs_sum+
                                  Summ2x2$Canals_sum+
                                  Summ2x2$UrbanIndustrial_sum+
                                  Summ2x2$UrbanResidence_sum+
                                  Summ2x2$RuralResidentialIndustrial_sum+
                                  Summ2x2$IndustrialSiteRural_sum+
                                  Summ2x2$WindGenerationFacility_sum+
                                  Summ2x2$MineSite_sum+
                                  Summ2x2$PeatMine_sum)/(1440000)#approximate area in sq.m of 2x2 landscape using land within 600 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ2x2$zerolndscp<-str_sub(Summ2x2$L2x2, -2, -1)#distinguishes '0' from '10', '20'
Summ2x2<-Summ2x2[!Summ2x2$zerolndscp=="-0",]
write.csv(Summ2x2, file="0_data/processed/different scales/vegHF.600.2017.2x2.csv")

Summ3x3<-vegHF.600.2017%>%
  group_by(L3x3)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ3x3<-data.frame(Summ3x3)
Summ3x3$PropConifer.600.2017<-(Summ3x3$CCPineR_sum+
                                 Summ3x3$CCPine1_sum+
                                 Summ3x3$CCPine2_sum+
                                 Summ3x3$CCPine3_sum+
                                 Summ3x3$CCPine4_sum+
                                 Summ3x3$CCSpruceR_sum+
                                 Summ3x3$CCSpruce1_sum+
                                 Summ3x3$CCSpruce2_sum+
                                 Summ3x3$CCSpruce3_sum+
                                 Summ3x3$CCSpruce4_sum+
                                 Summ3x3$TreedFenR_sum+
                                 Summ3x3$TreedFen1_sum+
                                 Summ3x3$TreedFen2_sum+
                                 Summ3x3$TreedFen3_sum+
                                 Summ3x3$TreedFen4_sum+
                                 Summ3x3$TreedFen5_sum+
                                 Summ3x3$TreedFen6_sum+
                                 Summ3x3$TreedFen7_sum+
                                 Summ3x3$TreedFen8_sum+
                                 Summ3x3$TreedFen9_sum+
                                 Summ3x3$TreedBogR_sum+
                                 Summ3x3$TreedBog1_sum+
                                 Summ3x3$TreedBog2_sum+
                                 Summ3x3$TreedBog3_sum+
                                 Summ3x3$TreedBog4_sum+
                                 Summ3x3$TreedBog5_sum+
                                 Summ3x3$TreedBog6_sum+
                                 Summ3x3$TreedBog7_sum+
                                 Summ3x3$TreedBog8_sum+
                                 Summ3x3$TreedBog9_sum+
                                 Summ3x3$SpruceR_sum+
                                 Summ3x3$Spruce1_sum+
                                 Summ3x3$Spruce2_sum+
                                 Summ3x3$Spruce3_sum+
                                 Summ3x3$Spruce4_sum+
                                 Summ3x3$Spruce5_sum+
                                 Summ3x3$Spruce6_sum+
                                 Summ3x3$Spruce7_sum+
                                 Summ3x3$Spruce8_sum+
                                 Summ3x3$Spruce9_sum+
                                 Summ3x3$PineR_sum+
                                 Summ3x3$Pine1_sum+
                                 Summ3x3$Pine2_sum+
                                 Summ3x3$Pine3_sum+
                                 Summ3x3$Pine4_sum+
                                 Summ3x3$Pine5_sum+
                                 Summ3x3$Pine6_sum+
                                 Summ3x3$Pine7_sum+
                                 Summ3x3$Pine8_sum+
                                 Summ3x3$Pine9_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$PropWet.600.2017<-(Summ3x3$ShrubbyBog_sum+
                             Summ3x3$ShrubbyFen_sum+
                             Summ3x3$ShrubbySwamp_sum+
                             Summ3x3$Marsh_sum+
                             Summ3x3$GraminoidFen_sum+
                             Summ3x3$TreedBogR_sum+
                             Summ3x3$TreedBog1_sum+
                             Summ3x3$TreedBog2_sum+
                             Summ3x3$TreedBog3_sum+
                             Summ3x3$TreedBog4_sum+
                             Summ3x3$TreedBog5_sum+
                             Summ3x3$TreedBog6_sum+
                             Summ3x3$TreedBog7_sum+
                             Summ3x3$TreedBog8_sum+
                             Summ3x3$TreedBog9_sum+
                             Summ3x3$TreedFenR_sum+
                             Summ3x3$TreedFen1_sum+
                             Summ3x3$TreedFen2_sum+
                             Summ3x3$TreedFen3_sum+
                             Summ3x3$TreedFen4_sum+
                             Summ3x3$TreedFen5_sum+
                             Summ3x3$TreedFen6_sum+
                             Summ3x3$TreedFen7_sum+
                             Summ3x3$TreedFen8_sum+
                             Summ3x3$TreedFen9_sum+
                             Summ3x3$TreedSwampR_sum+
                             Summ3x3$TreedSwamp1_sum+
                             Summ3x3$TreedSwamp2_sum+
                             Summ3x3$TreedSwamp3_sum+
                             Summ3x3$TreedSwamp4_sum+
                             Summ3x3$TreedSwamp5_sum+
                             Summ3x3$TreedSwamp6_sum+
                             Summ3x3$TreedSwamp7_sum+
                             Summ3x3$TreedSwamp8_sum+
                             Summ3x3$TreedSwamp9_sum+
                             Summ3x3$Water_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$TotalForest<-Summ3x3$TreedBogR_sum+
  Summ3x3$TreedBog1_sum+
  Summ3x3$TreedBog2_sum+
  Summ3x3$TreedBog3_sum+
  Summ3x3$TreedBog4_sum+
  Summ3x3$TreedBog5_sum+
  Summ3x3$TreedBog6_sum+
  Summ3x3$TreedBog7_sum+
  Summ3x3$TreedBog8_sum+
  Summ3x3$TreedBog9_sum+
  Summ3x3$TreedFenR_sum+
  Summ3x3$TreedFen1_sum+
  Summ3x3$TreedFen2_sum+
  Summ3x3$TreedFen3_sum+
  Summ3x3$TreedFen4_sum+
  Summ3x3$TreedFen5_sum+
  Summ3x3$TreedFen6_sum+
  Summ3x3$TreedFen7_sum+
  Summ3x3$TreedFen8_sum+
  Summ3x3$TreedFen9_sum+
  Summ3x3$TreedSwampR_sum+
  Summ3x3$TreedSwamp1_sum+
  Summ3x3$TreedSwamp2_sum+
  Summ3x3$TreedSwamp3_sum+
  Summ3x3$TreedSwamp4_sum+
  Summ3x3$TreedSwamp5_sum+
  Summ3x3$TreedSwamp6_sum+
  Summ3x3$TreedSwamp7_sum+
  Summ3x3$TreedSwamp8_sum+
  Summ3x3$TreedSwamp9_sum+
  Summ3x3$SpruceR_sum+
  Summ3x3$Spruce1_sum+
  Summ3x3$Spruce2_sum+
  Summ3x3$Spruce3_sum+
  Summ3x3$Spruce4_sum+
  Summ3x3$Spruce5_sum+
  Summ3x3$Spruce6_sum+
  Summ3x3$Spruce7_sum+
  Summ3x3$Spruce8_sum+
  Summ3x3$Spruce9_sum+
  Summ3x3$PineR_sum+
  Summ3x3$Pine1_sum+
  Summ3x3$Pine2_sum+
  Summ3x3$Pine3_sum+
  Summ3x3$Pine4_sum+
  Summ3x3$Pine5_sum+
  Summ3x3$Pine6_sum+
  Summ3x3$Pine7_sum+
  Summ3x3$Pine8_sum+
  Summ3x3$Pine9_sum+
  Summ3x3$DecidR_sum+
  Summ3x3$Decid1_sum+
  Summ3x3$Decid2_sum+
  Summ3x3$Decid3_sum+
  Summ3x3$Decid4_sum+
  Summ3x3$Decid5_sum+
  Summ3x3$Decid6_sum+
  Summ3x3$Decid7_sum+
  Summ3x3$Decid8_sum+
  Summ3x3$Decid9_sum+
  Summ3x3$MixedwoodR_sum+
  Summ3x3$Mixedwood1_sum+
  Summ3x3$Mixedwood2_sum+
  Summ3x3$Mixedwood3_sum+
  Summ3x3$Mixedwood4_sum+
  Summ3x3$Mixedwood5_sum+
  Summ3x3$Mixedwood6_sum+
  Summ3x3$Mixedwood7_sum+
  Summ3x3$Mixedwood8_sum+
  Summ3x3$Mixedwood9_sum+
  Summ3x3$CCPineR_sum+
  Summ3x3$CCPine1_sum+
  Summ3x3$CCPine2_sum+
  Summ3x3$CCPine3_sum+
  Summ3x3$CCPine4_sum+
  Summ3x3$CCSpruceR_sum+
  Summ3x3$CCSpruce1_sum+
  Summ3x3$CCSpruce2_sum+
  Summ3x3$CCSpruce3_sum+
  Summ3x3$CCSpruce4_sum+
  Summ3x3$CCDecidR_sum+
  Summ3x3$CCDecid1_sum+
  Summ3x3$CCDecid2_sum+
  Summ3x3$CCDecid3_sum+
  Summ3x3$CCDecid4_sum+
  Summ3x3$CCMixedwoodR_sum+
  Summ3x3$CCMixedwood1_sum+
  Summ3x3$CCMixedwood2_sum+
  Summ3x3$CCMixedwood3_sum+
  Summ3x3$CCMixedwood4_sum

Summ3x3$Wt.For.Age.600.2017<-(Summ3x3$TreedBogR_sum*4.5+#0-9 years
                                Summ3x3$TreedBog1_sum*10+#10-19 years
                                Summ3x3$TreedBog2_sum*20+#20-39 years
                                Summ3x3$TreedBog3_sum*40+#40-59 years
                                Summ3x3$TreedBog4_sum*60+#60-79 years
                                Summ3x3$TreedBog5_sum*80+#80-99 years
                                Summ3x3$TreedBog6_sum*100+#100-119 years
                                Summ3x3$TreedBog7_sum*120+#120-139 years
                                Summ3x3$TreedBog8_sum*140+#140-159 years
                                Summ3x3$TreedBog9_sum*160+#160 years and older
                                Summ3x3$TreedFenR_sum*4.5+
                                Summ3x3$TreedFen1_sum*10+
                                Summ3x3$TreedFen2_sum*20+
                                Summ3x3$TreedFen3_sum*40+
                                Summ3x3$TreedFen4_sum*60+
                                Summ3x3$TreedFen5_sum*80+
                                Summ3x3$TreedFen6_sum*100+
                                Summ3x3$TreedFen7_sum*120+
                                Summ3x3$TreedFen8_sum*140+
                                Summ3x3$TreedFen9_sum*160+
                                Summ3x3$TreedSwampR_sum*4.5+
                                Summ3x3$TreedSwamp1_sum*10+
                                Summ3x3$TreedSwamp2_sum*20+
                                Summ3x3$TreedSwamp3_sum*40+
                                Summ3x3$TreedSwamp4_sum*60+
                                Summ3x3$TreedSwamp5_sum*80+
                                Summ3x3$TreedSwamp6_sum*100+
                                Summ3x3$TreedSwamp7_sum*120+
                                Summ3x3$TreedSwamp8_sum*140+
                                Summ3x3$TreedSwamp9_sum*160+
                                Summ3x3$SpruceR_sum*4.5+
                                Summ3x3$Spruce1_sum*10+
                                Summ3x3$Spruce2_sum*20+
                                Summ3x3$Spruce3_sum*40+
                                Summ3x3$Spruce4_sum*60+
                                Summ3x3$Spruce5_sum*80+
                                Summ3x3$Spruce6_sum*100+
                                Summ3x3$Spruce7_sum*120+
                                Summ3x3$Spruce8_sum*140+
                                Summ3x3$Spruce9_sum*160+
                                Summ3x3$PineR_sum*4.5+
                                Summ3x3$Pine1_sum*10+
                                Summ3x3$Pine2_sum*20+
                                Summ3x3$Pine3_sum*40+
                                Summ3x3$Pine4_sum*60+
                                Summ3x3$Pine5_sum*80+
                                Summ3x3$Pine6_sum*100+
                                Summ3x3$Pine7_sum*120+
                                Summ3x3$Pine8_sum*140+
                                Summ3x3$Pine9_sum*160+
                                Summ3x3$DecidR_sum*4.5+
                                Summ3x3$Decid1_sum*10+
                                Summ3x3$Decid2_sum*20+
                                Summ3x3$Decid3_sum*40+
                                Summ3x3$Decid4_sum*60+
                                Summ3x3$Decid5_sum*80+
                                Summ3x3$Decid6_sum*100+
                                Summ3x3$Decid7_sum*120+
                                Summ3x3$Decid8_sum*140+
                                Summ3x3$Decid9_sum*160+
                                Summ3x3$MixedwoodR_sum*4.5+
                                Summ3x3$Mixedwood1_sum*10+
                                Summ3x3$Mixedwood2_sum*20+
                                Summ3x3$Mixedwood3_sum*40+
                                Summ3x3$Mixedwood4_sum*60+
                                Summ3x3$Mixedwood5_sum*80+
                                Summ3x3$Mixedwood6_sum*100+
                                Summ3x3$Mixedwood7_sum*120+
                                Summ3x3$Mixedwood8_sum*140+
                                Summ3x3$Mixedwood9_sum*160+
                                Summ3x3$CCPineR_sum*4.5+
                                Summ3x3$CCPine1_sum*10+
                                Summ3x3$CCPine2_sum*20+
                                Summ3x3$CCPine3_sum*40+
                                Summ3x3$CCPine4_sum*60+
                                Summ3x3$CCSpruceR_sum*4.5+
                                Summ3x3$CCSpruce1_sum*10+
                                Summ3x3$CCSpruce2_sum*20+
                                Summ3x3$CCSpruce3_sum*40+
                                Summ3x3$CCSpruce4_sum*60+
                                Summ3x3$CCDecidR_sum*4.5+
                                Summ3x3$CCDecid1_sum*10+
                                Summ3x3$CCDecid2_sum*20+
                                Summ3x3$CCDecid3_sum*40+
                                Summ3x3$CCDecid4_sum*60+
                                Summ3x3$CCMixedwoodR_sum*4.5+
                                Summ3x3$CCMixedwood1_sum*10+
                                Summ3x3$CCMixedwood2_sum*20+
                                Summ3x3$CCMixedwood3_sum*40+
                                Summ3x3$CCMixedwood4_sum*60)/Summ3x3$TotalForest  

Summ3x3$PropSoftLinear.600.2017<-(Summ3x3$SeismicLineNarrow_sum+
                                    Summ3x3$SeismicLineWide_sum+
                                    Summ3x3$Pipeline_sum+
                                    Summ3x3$TransmissionLine_sum+
                                    Summ3x3$RoadTrailVegetated_sum+
                                    Summ3x3$RoadVegetatedVerge_sum+
                                    Summ3x3$RailVegetatedVerge_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$PropHardLinear.600.2017<-(Summ3x3$RoadHardSurface_sum+
                                    Summ3x3$RailHardSurface_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$PropSoftPoly.600.2017<-(Summ3x3$OtherDisturbedVegetation_sum+
                                  Summ3x3$WellSite_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

Summ3x3$PropHardPoly.600.2017<-(Summ3x3$CultivationCrop_sum+
                                  Summ3x3$CultivationAbandoned_sum+
                                  Summ3x3$CultivationRoughPasture_sum+
                                  Summ3x3$CultivationTamePasture_sum+
                                  Summ3x3$HighDensityLivestockOperation_sum+
                                  Summ3x3$BorrowpitsDugoutsSumps_sum+
                                  Summ3x3$MunicipalWaterSewage_sum+
                                  Summ3x3$Reservoirs_sum+
                                  Summ3x3$Canals_sum+
                                  Summ3x3$UrbanIndustrial_sum+
                                  Summ3x3$UrbanResidence_sum+
                                  Summ3x3$RuralResidentialIndustrial_sum+
                                  Summ3x3$IndustrialSiteRural_sum+
                                  Summ3x3$WindGenerationFacility_sum+
                                  Summ3x3$MineSite_sum+
                                  Summ3x3$PeatMine_sum)/(3240000)#approximate area in sq.m of 3x3 landscape using land within 600 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ3x3$zerolndscp<-str_sub(Summ3x3$L3x3, -2, -1)#distinguishes '0' from '10', '20'
Summ3x3<-Summ3x3[!Summ3x3$zerolndscp=="-0",]
write.csv(Summ3x3, file="0_data/processed/different scales/vegHF.600.2017.3x3.csv")

Summ4x4<-vegHF.600.2017%>%
  group_by(L4x4)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ4x4<-data.frame(Summ4x4)
Summ4x4$PropConifer.600.2017<-(Summ4x4$CCPineR_sum+
                                 Summ4x4$CCPine1_sum+
                                 Summ4x4$CCPine2_sum+
                                 Summ4x4$CCPine3_sum+
                                 Summ4x4$CCPine4_sum+
                                 Summ4x4$CCSpruceR_sum+
                                 Summ4x4$CCSpruce1_sum+
                                 Summ4x4$CCSpruce2_sum+
                                 Summ4x4$CCSpruce3_sum+
                                 Summ4x4$CCSpruce4_sum+
                                 Summ4x4$TreedFenR_sum+
                                 Summ4x4$TreedFen1_sum+
                                 Summ4x4$TreedFen2_sum+
                                 Summ4x4$TreedFen3_sum+
                                 Summ4x4$TreedFen4_sum+
                                 Summ4x4$TreedFen5_sum+
                                 Summ4x4$TreedFen6_sum+
                                 Summ4x4$TreedFen7_sum+
                                 Summ4x4$TreedFen8_sum+
                                 Summ4x4$TreedFen9_sum+
                                 Summ4x4$TreedBogR_sum+
                                 Summ4x4$TreedBog1_sum+
                                 Summ4x4$TreedBog2_sum+
                                 Summ4x4$TreedBog3_sum+
                                 Summ4x4$TreedBog4_sum+
                                 Summ4x4$TreedBog5_sum+
                                 Summ4x4$TreedBog6_sum+
                                 Summ4x4$TreedBog7_sum+
                                 Summ4x4$TreedBog8_sum+
                                 Summ4x4$TreedBog9_sum+
                                 Summ4x4$SpruceR_sum+
                                 Summ4x4$Spruce1_sum+
                                 Summ4x4$Spruce2_sum+
                                 Summ4x4$Spruce3_sum+
                                 Summ4x4$Spruce4_sum+
                                 Summ4x4$Spruce5_sum+
                                 Summ4x4$Spruce6_sum+
                                 Summ4x4$Spruce7_sum+
                                 Summ4x4$Spruce8_sum+
                                 Summ4x4$Spruce9_sum+
                                 Summ4x4$PineR_sum+
                                 Summ4x4$Pine1_sum+
                                 Summ4x4$Pine2_sum+
                                 Summ4x4$Pine3_sum+
                                 Summ4x4$Pine4_sum+
                                 Summ4x4$Pine5_sum+
                                 Summ4x4$Pine6_sum+
                                 Summ4x4$Pine7_sum+
                                 Summ4x4$Pine8_sum+
                                 Summ4x4$Pine9_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$PropWet.600.2017<-(Summ4x4$ShrubbyBog_sum+
                             Summ4x4$ShrubbyFen_sum+
                             Summ4x4$ShrubbySwamp_sum+
                             Summ4x4$Marsh_sum+
                             Summ4x4$GraminoidFen_sum+
                             Summ4x4$TreedBogR_sum+
                             Summ4x4$TreedBog1_sum+
                             Summ4x4$TreedBog2_sum+
                             Summ4x4$TreedBog3_sum+
                             Summ4x4$TreedBog4_sum+
                             Summ4x4$TreedBog5_sum+
                             Summ4x4$TreedBog6_sum+
                             Summ4x4$TreedBog7_sum+
                             Summ4x4$TreedBog8_sum+
                             Summ4x4$TreedBog9_sum+
                             Summ4x4$TreedFenR_sum+
                             Summ4x4$TreedFen1_sum+
                             Summ4x4$TreedFen2_sum+
                             Summ4x4$TreedFen3_sum+
                             Summ4x4$TreedFen4_sum+
                             Summ4x4$TreedFen5_sum+
                             Summ4x4$TreedFen6_sum+
                             Summ4x4$TreedFen7_sum+
                             Summ4x4$TreedFen8_sum+
                             Summ4x4$TreedFen9_sum+
                             Summ4x4$TreedSwampR_sum+
                             Summ4x4$TreedSwamp1_sum+
                             Summ4x4$TreedSwamp2_sum+
                             Summ4x4$TreedSwamp3_sum+
                             Summ4x4$TreedSwamp4_sum+
                             Summ4x4$TreedSwamp5_sum+
                             Summ4x4$TreedSwamp6_sum+
                             Summ4x4$TreedSwamp7_sum+
                             Summ4x4$TreedSwamp8_sum+
                             Summ4x4$TreedSwamp9_sum+
                             Summ4x4$Water_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$TotalForest<-Summ4x4$TreedBogR_sum+
  Summ4x4$TreedBog1_sum+
  Summ4x4$TreedBog2_sum+
  Summ4x4$TreedBog3_sum+
  Summ4x4$TreedBog4_sum+
  Summ4x4$TreedBog5_sum+
  Summ4x4$TreedBog6_sum+
  Summ4x4$TreedBog7_sum+
  Summ4x4$TreedBog8_sum+
  Summ4x4$TreedBog9_sum+
  Summ4x4$TreedFenR_sum+
  Summ4x4$TreedFen1_sum+
  Summ4x4$TreedFen2_sum+
  Summ4x4$TreedFen3_sum+
  Summ4x4$TreedFen4_sum+
  Summ4x4$TreedFen5_sum+
  Summ4x4$TreedFen6_sum+
  Summ4x4$TreedFen7_sum+
  Summ4x4$TreedFen8_sum+
  Summ4x4$TreedFen9_sum+
  Summ4x4$TreedSwampR_sum+
  Summ4x4$TreedSwamp1_sum+
  Summ4x4$TreedSwamp2_sum+
  Summ4x4$TreedSwamp3_sum+
  Summ4x4$TreedSwamp4_sum+
  Summ4x4$TreedSwamp5_sum+
  Summ4x4$TreedSwamp6_sum+
  Summ4x4$TreedSwamp7_sum+
  Summ4x4$TreedSwamp8_sum+
  Summ4x4$TreedSwamp9_sum+
  Summ4x4$SpruceR_sum+
  Summ4x4$Spruce1_sum+
  Summ4x4$Spruce2_sum+
  Summ4x4$Spruce3_sum+
  Summ4x4$Spruce4_sum+
  Summ4x4$Spruce5_sum+
  Summ4x4$Spruce6_sum+
  Summ4x4$Spruce7_sum+
  Summ4x4$Spruce8_sum+
  Summ4x4$Spruce9_sum+
  Summ4x4$PineR_sum+
  Summ4x4$Pine1_sum+
  Summ4x4$Pine2_sum+
  Summ4x4$Pine3_sum+
  Summ4x4$Pine4_sum+
  Summ4x4$Pine5_sum+
  Summ4x4$Pine6_sum+
  Summ4x4$Pine7_sum+
  Summ4x4$Pine8_sum+
  Summ4x4$Pine9_sum+
  Summ4x4$DecidR_sum+
  Summ4x4$Decid1_sum+
  Summ4x4$Decid2_sum+
  Summ4x4$Decid3_sum+
  Summ4x4$Decid4_sum+
  Summ4x4$Decid5_sum+
  Summ4x4$Decid6_sum+
  Summ4x4$Decid7_sum+
  Summ4x4$Decid8_sum+
  Summ4x4$Decid9_sum+
  Summ4x4$MixedwoodR_sum+
  Summ4x4$Mixedwood1_sum+
  Summ4x4$Mixedwood2_sum+
  Summ4x4$Mixedwood3_sum+
  Summ4x4$Mixedwood4_sum+
  Summ4x4$Mixedwood5_sum+
  Summ4x4$Mixedwood6_sum+
  Summ4x4$Mixedwood7_sum+
  Summ4x4$Mixedwood8_sum+
  Summ4x4$Mixedwood9_sum+
  Summ4x4$CCPineR_sum+
  Summ4x4$CCPine1_sum+
  Summ4x4$CCPine2_sum+
  Summ4x4$CCPine3_sum+
  Summ4x4$CCPine4_sum+
  Summ4x4$CCSpruceR_sum+
  Summ4x4$CCSpruce1_sum+
  Summ4x4$CCSpruce2_sum+
  Summ4x4$CCSpruce3_sum+
  Summ4x4$CCSpruce4_sum+
  Summ4x4$CCDecidR_sum+
  Summ4x4$CCDecid1_sum+
  Summ4x4$CCDecid2_sum+
  Summ4x4$CCDecid3_sum+
  Summ4x4$CCDecid4_sum+
  Summ4x4$CCMixedwoodR_sum+
  Summ4x4$CCMixedwood1_sum+
  Summ4x4$CCMixedwood2_sum+
  Summ4x4$CCMixedwood3_sum+
  Summ4x4$CCMixedwood4_sum

Summ4x4$Wt.For.Age.600.2017<-(Summ4x4$TreedBogR_sum*4.5+#0-9 years
                                Summ4x4$TreedBog1_sum*10+#10-19 years
                                Summ4x4$TreedBog2_sum*20+#20-39 years
                                Summ4x4$TreedBog3_sum*40+#40-59 years
                                Summ4x4$TreedBog4_sum*60+#60-79 years
                                Summ4x4$TreedBog5_sum*80+#80-99 years
                                Summ4x4$TreedBog6_sum*100+#100-119 years
                                Summ4x4$TreedBog7_sum*120+#120-139 years
                                Summ4x4$TreedBog8_sum*140+#140-159 years
                                Summ4x4$TreedBog9_sum*160+#160 years and older
                                Summ4x4$TreedFenR_sum*4.5+
                                Summ4x4$TreedFen1_sum*10+
                                Summ4x4$TreedFen2_sum*20+
                                Summ4x4$TreedFen3_sum*40+
                                Summ4x4$TreedFen4_sum*60+
                                Summ4x4$TreedFen5_sum*80+
                                Summ4x4$TreedFen6_sum*100+
                                Summ4x4$TreedFen7_sum*120+
                                Summ4x4$TreedFen8_sum*140+
                                Summ4x4$TreedFen9_sum*160+
                                Summ4x4$TreedSwampR_sum*4.5+
                                Summ4x4$TreedSwamp1_sum*10+
                                Summ4x4$TreedSwamp2_sum*20+
                                Summ4x4$TreedSwamp3_sum*40+
                                Summ4x4$TreedSwamp4_sum*60+
                                Summ4x4$TreedSwamp5_sum*80+
                                Summ4x4$TreedSwamp6_sum*100+
                                Summ4x4$TreedSwamp7_sum*120+
                                Summ4x4$TreedSwamp8_sum*140+
                                Summ4x4$TreedSwamp9_sum*160+
                                Summ4x4$SpruceR_sum*4.5+
                                Summ4x4$Spruce1_sum*10+
                                Summ4x4$Spruce2_sum*20+
                                Summ4x4$Spruce3_sum*40+
                                Summ4x4$Spruce4_sum*60+
                                Summ4x4$Spruce5_sum*80+
                                Summ4x4$Spruce6_sum*100+
                                Summ4x4$Spruce7_sum*120+
                                Summ4x4$Spruce8_sum*140+
                                Summ4x4$Spruce9_sum*160+
                                Summ4x4$PineR_sum*4.5+
                                Summ4x4$Pine1_sum*10+
                                Summ4x4$Pine2_sum*20+
                                Summ4x4$Pine3_sum*40+
                                Summ4x4$Pine4_sum*60+
                                Summ4x4$Pine5_sum*80+
                                Summ4x4$Pine6_sum*100+
                                Summ4x4$Pine7_sum*120+
                                Summ4x4$Pine8_sum*140+
                                Summ4x4$Pine9_sum*160+
                                Summ4x4$DecidR_sum*4.5+
                                Summ4x4$Decid1_sum*10+
                                Summ4x4$Decid2_sum*20+
                                Summ4x4$Decid3_sum*40+
                                Summ4x4$Decid4_sum*60+
                                Summ4x4$Decid5_sum*80+
                                Summ4x4$Decid6_sum*100+
                                Summ4x4$Decid7_sum*120+
                                Summ4x4$Decid8_sum*140+
                                Summ4x4$Decid9_sum*160+
                                Summ4x4$MixedwoodR_sum*4.5+
                                Summ4x4$Mixedwood1_sum*10+
                                Summ4x4$Mixedwood2_sum*20+
                                Summ4x4$Mixedwood3_sum*40+
                                Summ4x4$Mixedwood4_sum*60+
                                Summ4x4$Mixedwood5_sum*80+
                                Summ4x4$Mixedwood6_sum*100+
                                Summ4x4$Mixedwood7_sum*120+
                                Summ4x4$Mixedwood8_sum*140+
                                Summ4x4$Mixedwood9_sum*160+
                                Summ4x4$CCPineR_sum*4.5+
                                Summ4x4$CCPine1_sum*10+
                                Summ4x4$CCPine2_sum*20+
                                Summ4x4$CCPine3_sum*40+
                                Summ4x4$CCPine4_sum*60+
                                Summ4x4$CCSpruceR_sum*4.5+
                                Summ4x4$CCSpruce1_sum*10+
                                Summ4x4$CCSpruce2_sum*20+
                                Summ4x4$CCSpruce3_sum*40+
                                Summ4x4$CCSpruce4_sum*60+
                                Summ4x4$CCDecidR_sum*4.5+
                                Summ4x4$CCDecid1_sum*10+
                                Summ4x4$CCDecid2_sum*20+
                                Summ4x4$CCDecid3_sum*40+
                                Summ4x4$CCDecid4_sum*60+
                                Summ4x4$CCMixedwoodR_sum*4.5+
                                Summ4x4$CCMixedwood1_sum*10+
                                Summ4x4$CCMixedwood2_sum*20+
                                Summ4x4$CCMixedwood3_sum*40+
                                Summ4x4$CCMixedwood4_sum*60)/Summ4x4$TotalForest  

Summ4x4$PropSoftLinear.600.2017<-(Summ4x4$SeismicLineNarrow_sum+
                                    Summ4x4$SeismicLineWide_sum+
                                    Summ4x4$Pipeline_sum+
                                    Summ4x4$TransmissionLine_sum+
                                    Summ4x4$RoadTrailVegetated_sum+
                                    Summ4x4$RoadVegetatedVerge_sum+
                                    Summ4x4$RailVegetatedVerge_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$PropHardLinear.600.2017<-(Summ4x4$RoadHardSurface_sum+
                                    Summ4x4$RailHardSurface_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$PropSoftPoly.600.2017<-(Summ4x4$OtherDisturbedVegetation_sum+
                                  Summ4x4$WellSite_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

Summ4x4$PropHardPoly.600.2017<-(Summ4x4$CultivationCrop_sum+
                                  Summ4x4$CultivationAbandoned_sum+
                                  Summ4x4$CultivationRoughPasture_sum+
                                  Summ4x4$CultivationTamePasture_sum+
                                  Summ4x4$HighDensityLivestockOperation_sum+
                                  Summ4x4$BorrowpitsDugoutsSumps_sum+
                                  Summ4x4$MunicipalWaterSewage_sum+
                                  Summ4x4$Reservoirs_sum+
                                  Summ4x4$Canals_sum+
                                  Summ4x4$UrbanIndustrial_sum+
                                  Summ4x4$UrbanResidence_sum+
                                  Summ4x4$RuralResidentialIndustrial_sum+
                                  Summ4x4$IndustrialSiteRural_sum+
                                  Summ4x4$WindGenerationFacility_sum+
                                  Summ4x4$MineSite_sum+
                                  Summ4x4$PeatMine_sum)/(5760000)#approximate area in sq.m of 4x4 landscape using land within 600 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ4x4$zerolndscp<-str_sub(Summ4x4$L4x4, -2, -1)#distinguishes '0' from '10', '20'
Summ4x4<-Summ4x4[!Summ4x4$zerolndscp=="-0",]
write.csv(Summ4x4, file="0_data/processed/different scales/vegHF.600.2017.4x4.csv")

Summ5x5<-vegHF.600.2017%>%
  group_by(L5x5)%>%
  summarise_at(vars(DecidR:CCSpruce4), list(sum=sum))
Summ5x5<-data.frame(Summ5x5)
Summ5x5$PropConifer.600.2017<-(Summ5x5$CCPineR_sum+
                                 Summ5x5$CCPine1_sum+
                                 Summ5x5$CCPine2_sum+
                                 Summ5x5$CCPine3_sum+
                                 Summ5x5$CCPine4_sum+
                                 Summ5x5$CCSpruceR_sum+
                                 Summ5x5$CCSpruce1_sum+
                                 Summ5x5$CCSpruce2_sum+
                                 Summ5x5$CCSpruce3_sum+
                                 Summ5x5$CCSpruce4_sum+
                                 Summ5x5$TreedFenR_sum+
                                 Summ5x5$TreedFen1_sum+
                                 Summ5x5$TreedFen2_sum+
                                 Summ5x5$TreedFen3_sum+
                                 Summ5x5$TreedFen4_sum+
                                 Summ5x5$TreedFen5_sum+
                                 Summ5x5$TreedFen6_sum+
                                 Summ5x5$TreedFen7_sum+
                                 Summ5x5$TreedFen8_sum+
                                 Summ5x5$TreedFen9_sum+
                                 Summ5x5$TreedBogR_sum+
                                 Summ5x5$TreedBog1_sum+
                                 Summ5x5$TreedBog2_sum+
                                 Summ5x5$TreedBog3_sum+
                                 Summ5x5$TreedBog4_sum+
                                 Summ5x5$TreedBog5_sum+
                                 Summ5x5$TreedBog6_sum+
                                 Summ5x5$TreedBog7_sum+
                                 Summ5x5$TreedBog8_sum+
                                 Summ5x5$TreedBog9_sum+
                                 Summ5x5$SpruceR_sum+
                                 Summ5x5$Spruce1_sum+
                                 Summ5x5$Spruce2_sum+
                                 Summ5x5$Spruce3_sum+
                                 Summ5x5$Spruce4_sum+
                                 Summ5x5$Spruce5_sum+
                                 Summ5x5$Spruce6_sum+
                                 Summ5x5$Spruce7_sum+
                                 Summ5x5$Spruce8_sum+
                                 Summ5x5$Spruce9_sum+
                                 Summ5x5$PineR_sum+
                                 Summ5x5$Pine1_sum+
                                 Summ5x5$Pine2_sum+
                                 Summ5x5$Pine3_sum+
                                 Summ5x5$Pine4_sum+
                                 Summ5x5$Pine5_sum+
                                 Summ5x5$Pine6_sum+
                                 Summ5x5$Pine7_sum+
                                 Summ5x5$Pine8_sum+
                                 Summ5x5$Pine9_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$PropWet.600.2017<-(Summ5x5$ShrubbyBog_sum+
                             Summ5x5$ShrubbyFen_sum+
                             Summ5x5$ShrubbySwamp_sum+
                             Summ5x5$Marsh_sum+
                             Summ5x5$GraminoidFen_sum+
                             Summ5x5$TreedBogR_sum+
                             Summ5x5$TreedBog1_sum+
                             Summ5x5$TreedBog2_sum+
                             Summ5x5$TreedBog3_sum+
                             Summ5x5$TreedBog4_sum+
                             Summ5x5$TreedBog5_sum+
                             Summ5x5$TreedBog6_sum+
                             Summ5x5$TreedBog7_sum+
                             Summ5x5$TreedBog8_sum+
                             Summ5x5$TreedBog9_sum+
                             Summ5x5$TreedFenR_sum+
                             Summ5x5$TreedFen1_sum+
                             Summ5x5$TreedFen2_sum+
                             Summ5x5$TreedFen3_sum+
                             Summ5x5$TreedFen4_sum+
                             Summ5x5$TreedFen5_sum+
                             Summ5x5$TreedFen6_sum+
                             Summ5x5$TreedFen7_sum+
                             Summ5x5$TreedFen8_sum+
                             Summ5x5$TreedFen9_sum+
                             Summ5x5$TreedSwampR_sum+
                             Summ5x5$TreedSwamp1_sum+
                             Summ5x5$TreedSwamp2_sum+
                             Summ5x5$TreedSwamp3_sum+
                             Summ5x5$TreedSwamp4_sum+
                             Summ5x5$TreedSwamp5_sum+
                             Summ5x5$TreedSwamp6_sum+
                             Summ5x5$TreedSwamp7_sum+
                             Summ5x5$TreedSwamp8_sum+
                             Summ5x5$TreedSwamp9_sum+
                             Summ5x5$Water_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$TotalForest<-Summ5x5$TreedBogR_sum+
  Summ5x5$TreedBog1_sum+
  Summ5x5$TreedBog2_sum+
  Summ5x5$TreedBog3_sum+
  Summ5x5$TreedBog4_sum+
  Summ5x5$TreedBog5_sum+
  Summ5x5$TreedBog6_sum+
  Summ5x5$TreedBog7_sum+
  Summ5x5$TreedBog8_sum+
  Summ5x5$TreedBog9_sum+
  Summ5x5$TreedFenR_sum+
  Summ5x5$TreedFen1_sum+
  Summ5x5$TreedFen2_sum+
  Summ5x5$TreedFen3_sum+
  Summ5x5$TreedFen4_sum+
  Summ5x5$TreedFen5_sum+
  Summ5x5$TreedFen6_sum+
  Summ5x5$TreedFen7_sum+
  Summ5x5$TreedFen8_sum+
  Summ5x5$TreedFen9_sum+
  Summ5x5$TreedSwampR_sum+
  Summ5x5$TreedSwamp1_sum+
  Summ5x5$TreedSwamp2_sum+
  Summ5x5$TreedSwamp3_sum+
  Summ5x5$TreedSwamp4_sum+
  Summ5x5$TreedSwamp5_sum+
  Summ5x5$TreedSwamp6_sum+
  Summ5x5$TreedSwamp7_sum+
  Summ5x5$TreedSwamp8_sum+
  Summ5x5$TreedSwamp9_sum+
  Summ5x5$SpruceR_sum+
  Summ5x5$Spruce1_sum+
  Summ5x5$Spruce2_sum+
  Summ5x5$Spruce3_sum+
  Summ5x5$Spruce4_sum+
  Summ5x5$Spruce5_sum+
  Summ5x5$Spruce6_sum+
  Summ5x5$Spruce7_sum+
  Summ5x5$Spruce8_sum+
  Summ5x5$Spruce9_sum+
  Summ5x5$PineR_sum+
  Summ5x5$Pine1_sum+
  Summ5x5$Pine2_sum+
  Summ5x5$Pine3_sum+
  Summ5x5$Pine4_sum+
  Summ5x5$Pine5_sum+
  Summ5x5$Pine6_sum+
  Summ5x5$Pine7_sum+
  Summ5x5$Pine8_sum+
  Summ5x5$Pine9_sum+
  Summ5x5$DecidR_sum+
  Summ5x5$Decid1_sum+
  Summ5x5$Decid2_sum+
  Summ5x5$Decid3_sum+
  Summ5x5$Decid4_sum+
  Summ5x5$Decid5_sum+
  Summ5x5$Decid6_sum+
  Summ5x5$Decid7_sum+
  Summ5x5$Decid8_sum+
  Summ5x5$Decid9_sum+
  Summ5x5$MixedwoodR_sum+
  Summ5x5$Mixedwood1_sum+
  Summ5x5$Mixedwood2_sum+
  Summ5x5$Mixedwood3_sum+
  Summ5x5$Mixedwood4_sum+
  Summ5x5$Mixedwood5_sum+
  Summ5x5$Mixedwood6_sum+
  Summ5x5$Mixedwood7_sum+
  Summ5x5$Mixedwood8_sum+
  Summ5x5$Mixedwood9_sum+
  Summ5x5$CCPineR_sum+
  Summ5x5$CCPine1_sum+
  Summ5x5$CCPine2_sum+
  Summ5x5$CCPine3_sum+
  Summ5x5$CCPine4_sum+
  Summ5x5$CCSpruceR_sum+
  Summ5x5$CCSpruce1_sum+
  Summ5x5$CCSpruce2_sum+
  Summ5x5$CCSpruce3_sum+
  Summ5x5$CCSpruce4_sum+
  Summ5x5$CCDecidR_sum+
  Summ5x5$CCDecid1_sum+
  Summ5x5$CCDecid2_sum+
  Summ5x5$CCDecid3_sum+
  Summ5x5$CCDecid4_sum+
  Summ5x5$CCMixedwoodR_sum+
  Summ5x5$CCMixedwood1_sum+
  Summ5x5$CCMixedwood2_sum+
  Summ5x5$CCMixedwood3_sum+
  Summ5x5$CCMixedwood4_sum

Summ5x5$Wt.For.Age.600.2017<-(Summ5x5$TreedBogR_sum*4.5+#0-9 years
                                Summ5x5$TreedBog1_sum*10+#10-19 years
                                Summ5x5$TreedBog2_sum*20+#20-39 years
                                Summ5x5$TreedBog3_sum*40+#40-59 years
                                Summ5x5$TreedBog4_sum*60+#60-79 years
                                Summ5x5$TreedBog5_sum*80+#80-99 years
                                Summ5x5$TreedBog6_sum*100+#100-119 years
                                Summ5x5$TreedBog7_sum*120+#120-139 years
                                Summ5x5$TreedBog8_sum*140+#140-159 years
                                Summ5x5$TreedBog9_sum*160+#160 years and older
                                Summ5x5$TreedFenR_sum*4.5+
                                Summ5x5$TreedFen1_sum*10+
                                Summ5x5$TreedFen2_sum*20+
                                Summ5x5$TreedFen3_sum*40+
                                Summ5x5$TreedFen4_sum*60+
                                Summ5x5$TreedFen5_sum*80+
                                Summ5x5$TreedFen6_sum*100+
                                Summ5x5$TreedFen7_sum*120+
                                Summ5x5$TreedFen8_sum*140+
                                Summ5x5$TreedFen9_sum*160+
                                Summ5x5$TreedSwampR_sum*4.5+
                                Summ5x5$TreedSwamp1_sum*10+
                                Summ5x5$TreedSwamp2_sum*20+
                                Summ5x5$TreedSwamp3_sum*40+
                                Summ5x5$TreedSwamp4_sum*60+
                                Summ5x5$TreedSwamp5_sum*80+
                                Summ5x5$TreedSwamp6_sum*100+
                                Summ5x5$TreedSwamp7_sum*120+
                                Summ5x5$TreedSwamp8_sum*140+
                                Summ5x5$TreedSwamp9_sum*160+
                                Summ5x5$SpruceR_sum*4.5+
                                Summ5x5$Spruce1_sum*10+
                                Summ5x5$Spruce2_sum*20+
                                Summ5x5$Spruce3_sum*40+
                                Summ5x5$Spruce4_sum*60+
                                Summ5x5$Spruce5_sum*80+
                                Summ5x5$Spruce6_sum*100+
                                Summ5x5$Spruce7_sum*120+
                                Summ5x5$Spruce8_sum*140+
                                Summ5x5$Spruce9_sum*160+
                                Summ5x5$PineR_sum*4.5+
                                Summ5x5$Pine1_sum*10+
                                Summ5x5$Pine2_sum*20+
                                Summ5x5$Pine3_sum*40+
                                Summ5x5$Pine4_sum*60+
                                Summ5x5$Pine5_sum*80+
                                Summ5x5$Pine6_sum*100+
                                Summ5x5$Pine7_sum*120+
                                Summ5x5$Pine8_sum*140+
                                Summ5x5$Pine9_sum*160+
                                Summ5x5$DecidR_sum*4.5+
                                Summ5x5$Decid1_sum*10+
                                Summ5x5$Decid2_sum*20+
                                Summ5x5$Decid3_sum*40+
                                Summ5x5$Decid4_sum*60+
                                Summ5x5$Decid5_sum*80+
                                Summ5x5$Decid6_sum*100+
                                Summ5x5$Decid7_sum*120+
                                Summ5x5$Decid8_sum*140+
                                Summ5x5$Decid9_sum*160+
                                Summ5x5$MixedwoodR_sum*4.5+
                                Summ5x5$Mixedwood1_sum*10+
                                Summ5x5$Mixedwood2_sum*20+
                                Summ5x5$Mixedwood3_sum*40+
                                Summ5x5$Mixedwood4_sum*60+
                                Summ5x5$Mixedwood5_sum*80+
                                Summ5x5$Mixedwood6_sum*100+
                                Summ5x5$Mixedwood7_sum*120+
                                Summ5x5$Mixedwood8_sum*140+
                                Summ5x5$Mixedwood9_sum*160+
                                Summ5x5$CCPineR_sum*4.5+
                                Summ5x5$CCPine1_sum*10+
                                Summ5x5$CCPine2_sum*20+
                                Summ5x5$CCPine3_sum*40+
                                Summ5x5$CCPine4_sum*60+
                                Summ5x5$CCSpruceR_sum*4.5+
                                Summ5x5$CCSpruce1_sum*10+
                                Summ5x5$CCSpruce2_sum*20+
                                Summ5x5$CCSpruce3_sum*40+
                                Summ5x5$CCSpruce4_sum*60+
                                Summ5x5$CCDecidR_sum*4.5+
                                Summ5x5$CCDecid1_sum*10+
                                Summ5x5$CCDecid2_sum*20+
                                Summ5x5$CCDecid3_sum*40+
                                Summ5x5$CCDecid4_sum*60+
                                Summ5x5$CCMixedwoodR_sum*4.5+
                                Summ5x5$CCMixedwood1_sum*10+
                                Summ5x5$CCMixedwood2_sum*20+
                                Summ5x5$CCMixedwood3_sum*40+
                                Summ5x5$CCMixedwood4_sum*60)/Summ5x5$TotalForest  

Summ5x5$PropSoftLinear.600.2017<-(Summ5x5$SeismicLineNarrow_sum+
                                    Summ5x5$SeismicLineWide_sum+
                                    Summ5x5$Pipeline_sum+
                                    Summ5x5$TransmissionLine_sum+
                                    Summ5x5$RoadTrailVegetated_sum+
                                    Summ5x5$RoadVegetatedVerge_sum+
                                    Summ5x5$RailVegetatedVerge_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$PropHardLinear.600.2017<-(Summ5x5$RoadHardSurface_sum+
                                    Summ5x5$RailHardSurface_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$PropSoftPoly.600.2017<-(Summ5x5$OtherDisturbedVegetation_sum+
                                  Summ5x5$WellSite_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

Summ5x5$PropHardPoly.600.2017<-(Summ5x5$CultivationCrop_sum+
                                  Summ5x5$CultivationAbandoned_sum+
                                  Summ5x5$CultivationRoughPasture_sum+
                                  Summ5x5$CultivationTamePasture_sum+
                                  Summ5x5$HighDensityLivestockOperation_sum+
                                  Summ5x5$BorrowpitsDugoutsSumps_sum+
                                  Summ5x5$MunicipalWaterSewage_sum+
                                  Summ5x5$Reservoirs_sum+
                                  Summ5x5$Canals_sum+
                                  Summ5x5$UrbanIndustrial_sum+
                                  Summ5x5$UrbanResidence_sum+
                                  Summ5x5$RuralResidentialIndustrial_sum+
                                  Summ5x5$IndustrialSiteRural_sum+
                                  Summ5x5$WindGenerationFacility_sum+
                                  Summ5x5$MineSite_sum+
                                  Summ5x5$PeatMine_sum)/(9000000)#approximate area in sq.m of 5x5 landscape using land within 600 m of points

#Now, get rid of the summary observations where the landscape ends in 0.
#Landscapes ending in '0' are composed of the point counts that could not 
#be assigned to a square array.
Summ5x5$zerolndscp<-str_sub(Summ5x5$L5x5, -2, -1)#distinguishes '0' from '10', '20'
Summ5x5<-Summ5x5[!Summ5x5$zerolndscp=="-0",]
write.csv(Summ5x5, file="0_data/processed/different scales/vegHF.600.2017.5x5.csv")

