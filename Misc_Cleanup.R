library(data.table)
setDTthreads(4)

results <- data.table::fread("outputs/Caribou_Range_Disturbance_Summary.csv")
rangePolyDT <- fread("data/Range_Polygon_Data.csv")
#this script corrects some mislabeled data - August 30th 2022

#1) add the new Calf Cow estimates
CBM <- fread("C:/users/ieddy/Downloads/Caribou Mtns.csv")
RedEarth <- fread("C:/users/ieddy/Downloads/Red Earth.csv")
ESAR <- fread("C:/users/ieddy/Downloads/ESAR CalfCow.csv")

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
rangePolyDT[AdultFemaleSurvivalRate == "Eric to review" | AdultFemaleSurvivalRate == "bug Eric"]
#The Polygon James20_Athabascan is a duplicate of the data from StuartSmith_79, and the latter actually presents survival
rangePolyDT <- rangePolyDT[PolygonID != "James20_Athabasca",]
rangePolyDT[PolygonID == "Courtois97_Manicougan", AdultFemaleSurvivalRate := mean(c(73.3, 81.6, 84.7, 79.3, 94.0, 87.3, 93.4))]
rangePolyDT[PolygonID == "Courtois97_Manicougan"]
rangePolyDT[, AdultFemaleSurvivalRate := as.numeric(AdultFemaleSurvivalRate)]
rangePolyDT[, CalfCow := as.numeric(CalfCow)]
#6) some calf cow ratios are clearly per 100 cows

rangePolyDT[grep("Culling115", PolygonID), CalfCow := CalfCow/100]
rangePolyDT
