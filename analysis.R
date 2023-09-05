#treat Stuart-Smith79 as one study area when modelling lambda
#treat Rettie92 pregnancy as one polygon
library(ggplot2)
library(data.table)


#TODO: find why MonasheeSouth is missing from linear features (should be zero)
#o


lnFootprints <- fread("outputs/Caribou_Range_LinearDisturbance_Summary.csv")
demography <- fread("data/Range_Polygon_Data.csv")
demography[, Note := NULL]
lnFootprints <- demography[lnFootprints, on = c("PolygonID")]
rsFootprints <- fread("outputs/Caribou_Range_Disturbance_Summary.csv")
#drop LandTrendR columns
setnames(rsFootprints, "lastYear", "Last_Measurement_Year")
rsFootprints[, c("meanMag", "pctDisturbed", "nDisturbed") := NULL]
#actually probably don't need so many columns
rsFootprints <- rsFootprints[, .(PolygonID, Npixels, pctBrnYr, pctHarvestYr, pctForest)]

caribouDF <- rsFootprints[lnFootprints, on = c("PolygonID")]
setnames(caribouDF, "mPerKm2", "linearDist")

ggplot(lnFootprints, aes(x = reorder(PolygonID, -linearDist), y = linearDist)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) + 
  labs(y = "linear disturbance (m/Km2)", x = "Range and study")

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
  scale_colour_viridis_c(option = "plasma") + 
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

out <- glm(formula = CalfCow ~ linearDist + pctHarvestYr, 
    data = caribouDF, family = gamma)
summary(out)
plot(out)

#adult female survival should use beta regression as survival is 0:1 bound
#except it can't be 1
FSR <- copy(caribouDF)
FSR <- FSR[!is.na(AdultFemaleSurvivalRate) & AdultFemaleSurvivalRate < 0.999]
library("betareg")
fsrMod <- betareg(AdultFemaleSurvivalRate ~ linearDist + pctHarvestYr, 
                  data = FSR)
summary(fsrMod)
