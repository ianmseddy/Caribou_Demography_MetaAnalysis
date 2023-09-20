#treat Stuart-Smith79 as one study area when modelling lambda
#treat Rettie92 pregnancy as one polygon

library(ggplot2)
library(data.table)
library(quickPlot)
library(RColorBrewer)
#curious 

lnFootprints <- fread("outputs/Caribou_Range_LinearDisturbance_Summary.csv")
demography <- fread("data/Range_Polygon_Data.csv")
demography[, Note := NULL]
lnFootprints <- demography[lnFootprints, on = c("PolygonID")]
rsFootprints <- fread("outputs/Caribou_Range_Disturbance_Summary.csv")
setnames(rsFootprints, "lastYear", "Last_Measurement_Year")

#Province is bizarre not in any of these outputs..
prov <- sf::st_read("GIS/Digitized_Caribou_StudyAreas.shp") |>
  as.data.table()
prov <- unique(prov[, .(PolygonID, Province)])

#actually probably don't need so many columns
rsFootprints <- rsFootprints[, .(PolygonID, Npixels, pctBrnYr, pctHarvestYr, pctForest, 
                                forestB, landscapeB)]

caribouDF <- lnFootprints[rsFootprints, on = c("PolygonID")]
caribouDF <- prov[caribouDF, on = "PolygonID"]

setnames(caribouDF, "mPerKm2", "linearDist")

caribouDF[, .N, (Province)]


demography <- prov[demography, on = "PolygonID"]

provOrder <- data.table(Province = c("BC", "YK", "AB", "SK", "MB", "ON", "QC", "NF"),
                        order = 1:8)
demography <- demography[provOrder, on = "Province"]
setkey(demography, order, DocID)
demography$foo <- 1:nrow(demography)

ggplot(data = demography) + 
  geom_segment(aes(x = First_Measurement_Year, 
                   xend = Last_Measurement_Year, 
                   y = foo, yend = foo, col = Province), 
               size = 1.2) + 
  theme_bw() + 
  labs(x = "Study Period", y = "Count of study areas") + 
  scale_colour_brewer(palette = "Set2")

dev()
#variables of interest
VOI <- caribouDF[, .(pctBrnYr, AdultFemaleSurvivalRate, pctForest, forestB,
                    Lambda, pctHarvestYr, CalfCow, linearDist,  Last_Measurement_Year)]
plot(VOI)
cor(VOI, method = "pearson", use = "pairwise.complete.obs")

#obviously need to control for population size.. 

ggplot(caribouDF, aes(x = reorder(PolygonID, -linearDist), y = linearDist, fill = Province)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) + 
  labs(y = "linear disturbance (m/km2)", x = "Range and study")

#TODO: treat StuartSmith79 as one polygon
ggplot(caribouDF, aes(y = Lambda, x= linearDist)) + 
  geom_point() + 
  theme_bw() + 
  labs(x = "linear disturbance (m/Km2)", y = "Lambda")


ggplot(caribouDF, aes(y = AdultFemaleSurvivalRate, x= linearDist)) + 
  geom_point() + 
  theme_bw() + 
  labs(x = "linear disturbance (m/Km2)", y = "Adult Female Survival")


ggplot(caribouDF, aes(y = CalfCow, x= linearDist)) + 
  geom_point() + 
  theme_bw() + 
  labs(x = "linear disturbance (m/Km2)", y = "Calf:Cow")

ggplot(caribouDF, aes(y = pctHarvestYr, x = linearDist)) +
  geom_point(aes(colour = pctBrnYr), size = 3, alpha = 0.7) +
  scale_colour_viridis_c(option = "plasma", name = "burn rate (%/year)") + 
  theme_bw() + 
  labs(x = "linear disturbance (m/Km)", y = "harvest disturbance (%/year)")

ggplot(caribouDF, aes(y = pctHarvestYr, x = linearDist)) +
  geom_point(aes(colour =Lambda), size = 3, alpha = 0.7) +
  scale_colour_viridis_c(option = "viridis") + 
  theme_bw() + 
  labs(x = "linear disturbance (m/Km)", y = "harvest disturbance (%/year)")


ggplot(caribouDF, aes(y = pctHarvestYr, x = linearDist)) +
  geom_point(aes(colour = AdultFemaleSurvivalRate), size = 3, alpha = 0.7) +
  scale_colour_viridis_c(option = "viridis") + 
  theme_bw() + 
  labs(x = "linear disturbance (m/Km)", y = "harvest disturbance (%/year)")

temp <- caribouDF[!is.na(CalfCow)]
#this is idiotic
temp[CalfCow == 0, CalfCow := 0.000001]
out <- glm(formula = CalfCow ~ linearDist + pctHarvestYr + forestB + pctBrnYr, 
           data = caribouDF, family = gaussian())
summary(out)
plot(out)

#adult female survival should use beta regression as survival is 0:1 bound
#except it can't be 1
FSR <- copy(caribouDF)
FSR <- FSR[!is.na(AdultFemaleSurvivalRate) & AdultFemaleSurvivalRate < 0.999]
library("betareg")
fsrMod <- betareg(AdultFemaleSurvivalRate ~ linearDist + pctHarvestYr + forestB + pctBrnYr, 
                  data = FSR)
summary(fsrMod)
