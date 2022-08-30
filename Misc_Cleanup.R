library(data.table)
setDTthreads(4)

results <- data.table::fread("outputs/Caribou_Range_Disturbance_Summary.csv")
rangePolyDT <- fread("data/Range_Polygon_Data.csv")
#this script corrects some mislabeled data - August 30th 2022

#1) add the new Calf Cow estimates from Eric
CBM <- fread("data/Caribou Mtns.csv")
RedEarth <- fread("data/Red Earth.csv")
ESAR <- fread("data/ESAR CalfCow.csv")

rangePolyDT[PolygonID == "Dalerum67_CaribouMountains", CalfCow := mean(CBM$V2)/100]
rangePolyDT[PolygonID == "Dalerum67_RedEarth", CalfCow := mean(RedEarth$V2)/100]
rangePolyDT[PolygonID == "Dalerum67_ESAR", CalfCow := mean(ESAR$V2)/100]

#note the years though as calfcow is earlier
rangePolyDT[grep("Dalerum67", PolygonID), Note := "FSR and CalfCow derived from interpretation. Note FSM and CalfCow years are not identical"]

#2) fix some NAs coded as blank
rangePolyDT[CalfCow == "", CalfCow := NA]

summary(rangePolyDT[, .(`First Measurement Year`, `Last Measurement Year`, Digitization_Scale, AdultFemaleSurvivalRate,
                        Lambda, CalfCow, Mortality, Pregnancy, `Predator Control`)])


#3) ColdLakeSK was mislabeled as ColdLakes, and was missing measurement year
rangePolyDT[PolygonID == "Hervieux87_ColdLakesSK", c("PolygonID", "Last Measurement Year") := .("Hervieux87_ColdLakeSK", 2012)]
#fix names to avoid having to input '`'
setnames(rangePolyDT, old = c("First Measurement Year", "Last Measurement Year", "Predator Control"), 
         new = c("First_Measurement_Year", "Last_Measurement_Year", "Predator_Control"))

#4) Fix numeric attributes that are mislabeled as character (I originally noted "eric to review" in some of these)
#The Polygon James20_Athabascan is a duplicate of the data from StuartSmith_79, and the latter actually presents survival
rangePolyDT <- rangePolyDT[PolygonID != "James20_Athabasca",]
rangePolyDT[PolygonID == "Courtois97_Manicougan", AdultFemaleSurvivalRate := mean(c(73.3, 81.6, 84.7, 79.3, 94.0, 87.3, 93.4))/100]
#the Courtois calf:cow were derived from the reported "% calves" in Table 1 by asssuming 50/50 sex ratio
#e.g. 12.5% calf = 87.5% adult = 43.75% cow = 12.5:43.75 = 0.286
CourtoisCalfCow <-  mean(c(
  12.5/c((100 - 12.5)/2),
  20.9/c((100 - 20.9)/2),
  23.1/c((100 - 23.1)/2),
  26.7/c((100 - 26.7)/2),
  18.2/c((100 - 18.2)/2)
  ))
rangePolyDT[PolygonID == "Courtois97_Manicougan", CalfCow := CourtoisCalfCow]
rangePolyDT[, AdultFemaleSurvivalRate := as.numeric(AdultFemaleSurvivalRate)]
rangePolyDT[, CalfCow := as.numeric(CalfCow)]
#6) some calf cow ratios are clearly per 100 cows
rangePolyDT[grep("Culling115", PolygonID), CalfCow := CalfCow/100]
#still need to address the Ellington 2020 data
#Pregnancy is a character due to asterisk on 79 - I believe it was the calving rate, not the pregnancy rate
