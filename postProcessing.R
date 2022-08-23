library(terra)
library(sf)
library(data.table)
library(magrittr) # for piping - I am still using R v4.02 like a chump

#location of LandTrendR result directory
resultsDir <- "outputs"

#caribou range polygons digitized from literature
RangePolys <- terra::vect("GIS/Digitized_Caribou_StudyAreas.shp")

#table with demographic data for each range
caribouDF <- fread("data/Range_Polygon_Data.csv")


#####auxiliary datasets ####  
# fire, harvest, and landcover - to give context to LandTrendR
#GIS data - these NFI datasets are enormous hence the hardcoded path.
#they all live here https://opendata.nfis.org/mapserver/nfis-change_eng.html

harvest <- terra::rast("C:/Ian/Data/C2C/CA_harvest_year_1985_2015.tif")
# https://opendata.nfis.org/downloads/forest_change/CA_forest_harvest_mask_year_1985_2015.zip
fire <- NULL
# https://opendata.nfis.org/downloads/forest_change/CA_forest_wildfire_year_DNBR_Magnitude_1985_2015.zip
#we only need fire year - the dNBR is the largest file, as it is stored as a float. Beware of unzipping

#citation for both: Hermosilla, T., M.A. Wulder, J.C. White, N.C. Coops, G.W. Hobart, L.B. Campbell, 2016. 
#Mass data processing of time series Landsat imagery: pixels to data products for forest monitoring. 
#International Journal of Digital Earth 9(11), 1035-1054.


LCC <- rast("C:/Ian/Data/C2C/CA_forest_VLCE_2015/CA_forest_VLCE_2015.tif")
#this is a similarly derived landcover file
# https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE_2015.zip
#cite: White, J.C., M.A. Wulder, T. Hermosilla, N.C. Coops, and G.W. Hobart. (2017). A nationwide annual characterization of 25 years of forest disturbance and recovery for Canada using Landsat time series. Remote Sensing of Environment. 192: 303-321. 

#20 is water, 31 snow_ice, 32 rock rubble, 33 barren, 40 bryoids, 50 shrub, 80 wetland, 81 wetland-treed, 100 = herbs
#210 coniferous, 220 broadleaf, 230 mixedwood

####summarize data ####

#aggregate the polygons to avoid multipolygons being counted multiple times
RangePolys <- project(RangePolys, LCC)
###temporary subset 

#this will summarize the LandTrendR, harvest, and fire stats within each polygon
summarizeData <- function(SAname, SA = RangePolys, dir = resultsDir, 
                          harvest = NULL, fire = NULL, lcc = LCC) {
 
 
  SA <- SA[SA$PolygonID == SAname,] #crop the study area poly
  #get fire data
  lastYear <- unique(SA$Meas_Years) #make unique in case dissolve was missed?
  outputDT <- data.table("PolygonID" = SAname, lastYear = lastYear)
  
  ####summarize the LCC data ####
  lcc <- crop(lcc, SA) %>% mask(., SA)
  lccVal <- data.table(values(lcc))
  names(lccVal) <- "lcc" #assigning it during creation is no longer working ?
  
  forestPix <- lccVal[lcc %in% c(210, 220, 230), .N]
  vegPix <- lccVal[lcc %in% c(210, 220, 230, 40, 50, 80, 81, 100), .N]
  N <- lccVal[!is.na(lcc), .N]
  propForest <- forestPix/N
  propVeg <- vegPix/N
  rm(lccVal)
  outputDT[, c("Npixels", "propForest", "propVeg") := .(N, propForest, propVeg)]
  
  ####summarize the LandTrendR data####
  LandTrendR <- file.path(dir, paste0(SAname, ".tif"))
  if (file.exists(LandTrendR)) {
    LandTrendR <- rast(LandTrendR)
    
    #six bands are the following:
    #yod - year of disturbance
    #mag - magnitude of disturbance, expressed as segment start - segment end value (delta)
    #dur - duration of disturbance, expressed as subtraction of start year from end year
    #preval - prechange event spectral value
    #rate - the rate of spectral change
    #csnr - I don't know but suspect it involves magnitude relative to rsme 
    
    if (!compareGeom(LandTrendR, lcc, stopOnError = FALSE)) {
      #this is cropped but not masked - however we must reproject. Since 0 is no data, we must reclassify
      LandTrendR <- terra::classify(LandTrendR, matrix(data = c(0, NA), ncol = 2))
      LandTrendR <- project(LandTrendR, lcc)
    }
    LandTrendR <- mask(LandTrendR, SA) #no data and no change as same otherwise
    LandTrendR <- as.data.table(values(LandTrendR)) #matrix to dt
    nDisturbed <- nrow(LandTrendR[yod <= lastYear])
    #I am unsure if non-forest is routinely classified as disturbed under LandTrendR
    propDisturbed <- nDisturbed/N
    meanMag <- mean(LandTrendR[mag > 0]$mag)
    outputDT[, c("nDisturbed", "propDisturbed", "meanMag") := .(nDisturbed, propDisturbed, meanMag)]
  }
  
  ####optionally summarize the C2C datasets####
  if (!is.null(harvest)) {
    SAh <- terra::project(SA, harvest)
    harvestSA <- crop(harvest, SAh) %>%
      mask(., SAh)
    harvested <- data.table(values(harvestSA)) 
    names(harvested) <- "harvest"
    propHarvested <- harvested[harvest <= lastYear & harvest > 0, .N]/N
    outputDT[, propHarvest := propHarvested]
    rm(harvestSA, harvested, SAh)
    #this is the proportion of pixels that were harvested before or during
    #the last year of caribou measurement
  }
  
  if (!is.null(fire)) {
    #THIS NEEDS TO FOLLOW WHAT FIRE IS DOING - don't add 1900
    SAf <- terra::project(SA, fire)
    fireSA <- crop(fire, SAf) %>%
      mask(fireSA, SAf)
    fireVal <- values(fireSA) + 1900 #convert to real years
    propFire <- length(fireVal[fireVal <= lastYear])/length(forestPix)
    outputDT[propFire := propFire]
    rm(fireSA, fireVal, SAf)
    #this is the proportion of pixels that were burned before or during
    #the last year of caribou measurement
  }
  
  return(outputDT)
}

RangeSubset <- RangePolys$PolygonID[c(1, 5, 6, 9)]

results <- rbindlist(lapply(RangeSubset, summarizeData, harvest = C2Charvest), fill = TRUE)

