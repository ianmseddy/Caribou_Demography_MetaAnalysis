library(terra)
library(sf)
library(data.table)
library(magrittr) # for piping - I am still using R v4.02 like a chump
library(googledrive)
library(purrr)
library(parallel)

#location of LandTrendR result directory

resultsDir <- "outputs"
resultFile <- file.path(resultsDir, "Caribou_LandTrendR_Results.zip")

#results are publicly accessible with link, think this is fine.. 
if (FALSE) {
  googledrive::drive_deauth()
  googledrive::drive_download(file = 'https://drive.google.com/file/d/1e-g2JrWi46GwZ0VIynd1DmZLxqk-9WGB/view?usp=sharing',
                       path = resultFile
  utils::unzip(zipfile = resultFile, exdir = resultsDir)
  #this was vastly easier than using googledrive tools, which fail due to API limits (I believe anyway)
  #however I had to download and re-upload the original directory...which seems avoidable?
}



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
fire <- terra::rast("C:/Ian/Data/C2C/CA_forest_wildfire_year_1985_2015.tif")
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
#this function could be more efficient - I was trying unsuccessfuly to debug parallelization arguments
summarizeData <- function(SAname, SA, LandTrendR, harvest, fire, lcc = LCC) {
  print(SAname)
  SA <- SA[SA$PolygonID == SAname,] #crop the study area poly
  #get fire data
  lastYear <- unique(SA$Meas_Years) #make unique in case dissolve was missed?
  outputDT <- data.table("PolygonID" = SAname, lastYear = lastYear)
  
  LandTrendR <- LandTrendR[[SAname]] %>%
    subset(., subset = c("mag", "yod"))
  
  SA <- project(SA, LandTrendR)
  SAcrop <- project(SA, lcc)
  #I believe this will cut down object size for projecting
  
  ####summarize the LCC data ####
  lcc <- project(lcc, LandTrendR, method = "near") %>%
    mask(., SA)
  lccVal <- data.table(values(lcc))
  names(lccVal) <- "lcc" #assigning it during creation is no longer working ?
  
  forestPix <- lccVal[lcc %in% c(210, 220, 230), .N]
  disturbablePix <- lccVal[lcc %in% c(210, 220, 230, 33, 40, 50, 80, 81, 100), .N]
  N <- lccVal[!is.na(lcc), .N]
  propForest <- forestPix/N
  propVeg <- disturbablePix/N
  rm(lccVal, disturbablePix, forestPix, lcc)
  outputDT[, c("Npixels", "propForest", "propVeg") := .(N, propForest, propVeg)]
  
  ####summarize the LandTrendR data####
  
  #six bands are the following:
  #yod - year of disturbance
  #mag - magnitude of disturbance, expressed as segment start - segment end value (delta)
  #dur - duration of disturbance, expressed as subtraction of start year from end year
  #preval - prechange event spectral value
  #rate - the rate of spectral change
  #csnr - I don't know but suspect it involves magnitude relative to rsme 
  
  LandTrendR <- mask(LandTrendR, SA)
  LandTrendRdt <- as.data.table(values(LandTrendR))
  nDisturbed <- nrow(LandTrendRdt[yod <= lastYear & yod > 0])
  #I am unsure if non-forest is routinely classified as disturbed under LandTrendR
  propDisturbed <- nDisturbed/N
  meanMag <- mean(LandTrendRdt[yod <= lastYear & mag > 0]$mag)
  outputDT[, c("nDisturbed", "propDisturbed", "meanMag") := .(nDisturbed, propDisturbed, meanMag)]

  ####summarize the C2C harvest #####
  harvest <- project(harvest, LandTrendR, method = "near") %>%
    mask(., SA)
  harvestDT <- data.table(values(harvest) + 1900) 
  names(harvestDT) <- "harvested"
  propHarvested <- harvestDT[harvested <= lastYear & harvested > 1900, .N]/N
  outputDT[, propHarvest := propHarvested]
  rm(harvest, harvestDT)
  #this is the proportion of pixels that were harvested before or during
  #the last year of caribou measurement
  
  fire <- project(fire, LandTrendR, method = "near") %>%
    mask(., SA)
  burnDT <- data.table(values(fire) + 1900) 
  names(burnDT) <- "burned"
  propBurned <- burnDT[burned <= lastYear & burned > 1900, .N]/N
  outputDT[, propBurned := propBurned]
  rm(fire, burnDT)
  #this is the proportion of pixels that were burned before or during
  #the last year of caribou measurement

  
  return(outputDT)
}

PolygonIDs <- unique(RangePolys$PolygonID)

# load LandTrendR results. 
LandTrendR <- file.path("outputs/Caribou_LandTrendR_Results", paste0(PolygonIDs, ".tif")) %>%
  lapply(., rast)
names(LandTrendR) <- PolygonIDs

#I had some memory issues reprojecting (100+ GB RAM used!) so this approach was safest
results <- rbindlist(lapply(PolygonIDs, summarizeData, 
                   LandTrendR = LandTrendR, SA = RangePolys, harvest = harvest, fire = fire))
#Red Earth is 22 
write.csv(results, "outputs/Caribou_Range_Disturbance_Summary.csv")

