library(terra)
library(sf)
library(data.table)
library(googledrive) #I don't think we need googledrive anymore - it was only for hosting a single fire layer
library(purrr)
library(devtools)
library(reproducible) #run the above line if this fails


#caribou range polygons digitized from literature
RangePolys <- prepInputs(url = "https://drive.google.com/file/d/18gFYdnALVJIaJAmQlNnHQENqARWlfEYG/view?usp=sharing", 
                         targetFile = "Digitized_Caribou_StudyAreas.shp",
                         destinationPath = "GIS",
                         fun = "terra::vect")

#caribou demographic data - included in GitHub Repo
caribouDF <- fread("data/Range_Polygon_Data.csv")


#####auxiliary datasets ####  
# fire, harvest, and landcover - to give context to LandTrendR.
#they all live here https://opendata.nfis.org/mapserver/nfis-change_eng.html

# harvest <- terra::rast("C:/Ian/Data/C2C/CA_harvest_year_1985_2015.tif")
#this is now working with new prepInputs - but unzipping is slow. 
#the new unzip is needlessly extracting the CA_forest_harvest_mask, which is 30 GB
harvest <- prepInputs(url = paste0("https://opendata.nfis.org/downloads/forest_change",
                                   "/CA_Forest_Harvest_1985-2020.zip"),
                          targetFile = "CA_Forest_Harvest_1985-2020.tif", 
                          destinationPath = "GIS",
                          fun = "terra::rast")
NAflag(harvest) <- 0
# https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip

fire <- prepInputs(url = "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Fire_1985-2020.zip", 
                   fun = "terra::rast",
                   targetFile = "CA_Forest_Fire_1985-2020.tif",
                   destinationPath = "GIS")
NAflag(fire) <- 0
# https://opendata.nfis.org/downloads/forest_change/CA_forest_wildfire_year_DNBR_Magnitude_1985_2015.zip
#we only need fire year - an integer -  the delta NBR is a float, which causes the file size. 

#citation for both: Hermosilla, T., M.A. Wulder, J.C. White, N.C. Coops, G.W. Hobart, L.B. Campbell, 2016. 
#Mass data processing of time series Landsat imagery: pixels to data products for forest monitoring. 
#International Journal of Digital Earth 9(11), 1035-1054.

LCC <- prepInputs(url = "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE_2015.zip", 
                  destinationPath = "GIS", 
                  targetFile = "CA_forest_VLCE_2015.tif",
                  fun = "terra::rast")

Biomass <- prepInputs(url = paste0("https://opendata.nfis.org/downloads/",
                                   "forest_change/CA_forest_total_biomass_2015_NN.zip"),
                      destinationPath = "GIS", fun = "terra::rast", 
                      targetFile = "CA_forest_total_biomass_2015.tif")

#this is a similarly derived landcover file
# https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE_2015.zip
#cite: White, J.C., M.A. Wulder, T. Hermosilla, N.C. Coops, and G.W. Hobart. (2017). A nationwide annual characterization of 25 years of forest disturbance and recovery for Canada using Landsat time series. Remote Sensing of Environment. 192: 303-321. 

#20 is water, 31 snow_ice, 32 rock rubble, 33 barren, 40 bryoids, 50 shrub, 80 wetland, 81 wetland-treed, 100 = herbs
#210 coniferous, 220 broadleaf, 230 mixedwood
#we exclude water, snow/ice and rock/rubble in calculating habitat

####summarize data ####
#aggregate the polygons to avoid multipolygons being counted multiple times
RangePolys <- project(RangePolys, LCC)
###temporary subset 

#this will summarize biomass, landcover, and disturbance rates in each polygon
#the disturbance rates must be adjusted to account for available habitat

summarizeData <- function(SAname, SA, harvest, fire, biomass, lcc = LCC) {
  print(SAname)
  SA <- SA[SA$PolygonID == SAname,] #crop the study area poly
  #get fire data
  lastYear <- unique(SA$Meas_Years) #make unique in case dissolve was missed?
  outputDT <- data.table("PolygonID" = SAname, lastYear = lastYear)
  
  SAcrop <- project(SA, lcc)
  #I believe this will cut down object size for projecting
  
  ####summarize the LCC data ####
  lcc <- postProcess(lcc, cropTo = SAcrop, maskTo = SAcrop, method = "near")
  
  biomass <- postProcess(biomass, cropTo = lcc, projectTo = lcc,
                         maskTo = SAcrop, method = "bilinear")
  
  lccVal <- data.table(lcc = lcc[], biomass = biomass[])
  names(lccVal) <- c("lcc", "biomass") #these kept inherting layer names
  disturbablePix <- lccVal[lcc %in% c(210, 220, 230, 33, 40, 50, 80, 81, 100), .N]
  N <- lccVal[!is.na(lcc), .N]

  #assumes wetland is non-forest (as they wouldn't harvest here)
  forestPix <- lccVal[lcc %in% c(210, 220, 230), .N]
  coniferous <- lccVal[lcc %in% 210, .N]/N * 100
  decid <- lccVal[lcc %in% 220, .N]/N * 100
  mixed <- lccVal[lcc %in% 230, .N]/N * 100
  treedWetland <- lccVal[lcc %in% 81, .N]/N * 100 #non-treed wetland
  nonTreedWetland <- lccVal[lcc %in% 80, .N]/N * 100 #I assume this is only non-treed
  
  
  meanForestBiomass <- mean(lccVal[lcc %in% c(210, 220, 230),]$biomass, na.rm = TRUE)
  meanLandscapeBiomass <- mean(lccVal[lcc %in%  c(210, 220, 230, 33, 40, 50, 80, 81, 100)]$biomass,
                               na.rm = TRUE)
  pctForest <- forestPix/N * 100
  pctVeg <- disturbablePix/N * 100
  rm(lccVal, disturbablePix, forestPix)
  outputDT[, c("Npixels", "pctForest", "pctVeg", "forestB", "landscapeB") := 
             .(N, pctForest, pctVeg, meanForestBiomass, meanLandscapeBiomass)]
  outputDT[, c("pctCon", "pctDec", "pctMix", "pctTrWet", "pctWet") :=
             .(coniferous, decid, mixed, treedWetland, nonTreedWetland)]

  ####summarize the C2C harvest #####
  harvest <- postProcess(harvest, projectTo = lcc, cropTo = lcc, maskTo = SAcrop)
  harvestDT <- data.table(values(harvest)) 
  names(harvestDT) <- "harvested"
  pctHarvested <- harvestDT[harvested <= lastYear & harvested > 1900, .N]/N * 100
  totalPctHarvest <- nrow(harvestDT[harvested > 1900])/N * 100
  #the pctortion harvested from 1985 to last year of study
  outputDT[, pctHarvest := pctHarvested]
  #the total pctortion harvested 1985-2015
  outputDT[, pctHarvest_85to15 := totalPctHarvest]
  #the annual pctortion harvested (based on year of study, not 1985-2015) 
  outputDT[, pctHarvestYr := pctHarvest/c(lastYear - 1984)]
  rm(harvest, harvestDT)
  #this is the pctortion of pixels that were harvested before or during
  #the last year of caribou measurement
  
  fire <- postProcess(fire, cropTo = lcc, projectTo = lcc, maskTo = SAcrop, method = "near")
  burnDT <- data.table(values(fire)) 
  names(burnDT) <- "burned"
  outputDT[, pctBurned := burnDT[burned <= lastYear & burned > 1900, .N]/N * 100]
  outputDT[, pctBrnYr := pctBurned/c(lastYear - 1984)]
  rm(fire, burnDT)
  #this is the pctortion of pixels that were burned before or during
  #the last year of caribou measurement
  return(outputDT)
}

PolygonIDs <- unique(RangePolys$PolygonID)


#I had some memory issues reprojecting (100+ GB RAM used!) so this approach was safest
results <- rbindlist(lapply(PolygonIDs, summarizeData, SA = RangePolys, lcc = LCC,
                            harvest = harvest, fire = fire, biomass = Biomass))
#TODO: check results
write.csv(results, "outputs/Caribou_Range_Disturbance_Summary.csv")
