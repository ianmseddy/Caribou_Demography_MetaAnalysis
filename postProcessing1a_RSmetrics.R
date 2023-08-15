library(terra)
library(sf)
library(data.table)
library(googledrive)
library(purrr)
library(devtools)
library(reproducible) #run the above line if this fails


drive_auth("ianmseddy@gmail.com") #figure this out

#TODO: fix this to use the google drive
#location of LandTrendR result directory
resultsDir <- "outputs"
resultFile <- file.path(resultsDir, "Caribou_LandTrendR_Results")

#results are publicly accessible with link, think this is fine.. 
if (!dir.exists(resultFile)) {
  zipPath <- paste0(resultFile, ".zip")
  googledrive::drive_deauth()
  googledrive::drive_download(file = 'https://drive.google.com/file/d/1e-g2JrWi46GwZ0VIynd1DmZLxqk-9WGB/view?usp=sharing',
                              path = resultFile)
  utils::unzip(zipfile = resultFile, exdir = resultsDir)
}

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
harvest <- prepInputs(url = "https://opendata.nfis.org/downloads/forest_change/CA_forest_harvest_mask_year_1985_2015.zip",
                          targetFile = "CA_harvest_year_1985_2015.tif", 
                          destinationPath = "GIS",
                          fun = "terra::rast")
# https://opendata.nfis.org/downloads/forest_change/CA_forest_harvest_mask_year_1985_2015.zip

#this is the extracted year layer of a composite image that is 90 GB. URL is given below
fire <- prepInputs(url = "https://drive.google.com/file/d/1tZIYz8QEZdrXqgvw3l50RQpRZb7mfIYa/view?usp=sharing", 
                   fun = "terra::rast", 
                   destinationPath = "GIS")
# https://opendata.nfis.org/downloads/forest_change/CA_forest_wildfire_year_DNBR_Magnitude_1985_2015.zip
#we only need fire year - an integer -  the delta NBR is a float, which causes the file size. 

#citation for both: Hermosilla, T., M.A. Wulder, J.C. White, N.C. Coops, G.W. Hobart, L.B. Campbell, 2016. 
#Mass data processing of time series Landsat imagery: pixels to data products for forest monitoring. 
#International Journal of Digital Earth 9(11), 1035-1054.

LCC <- prepInputs(url = "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE_2015.zip", 
                  destinationPath = "GIS", 
                  targetFile = "CA_forest_VLCE_2015.tif",
                  fun = "terra::rast")

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

#this will summarize the LandTrendR, harvest, and fire stats within each polygon
#the disturbance rates must be adjusted to account for available habitat
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
  pctForest <- forestPix/N * 100
  pctVeg <- disturbablePix/N * 100
  rm(lccVal, disturbablePix, forestPix, lcc)
  outputDT[, c("Npixels", "pctForest", "pctVeg") := .(N, pctForest, pctVeg)]
  
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
  #TODO: assess how disturbed non-forest performs under LandTrendR
  pctDisturbed <- nDisturbed/N * 100
  totalPctDisturbed <- nrow(LandTrendRdt[yod > 0])/N * 100
  meanMag <- mean(LandTrendRdt[yod <= lastYear & mag > 0]$mag)
  outputDT[, c("nDisturbed", "pctDisturbed", "meanMag", "pctDisturbed_86to19") := 
             .(nDisturbed, pctDisturbed, meanMag, totalPctDisturbed)]
  #the earliest LandTrendR detects change is 1986 because it requires a base year
  outputDT[, pctDisturbedYr := pctDisturbed/c(lastYear - 1985)]
  
  ####summarize the C2C harvest #####
  harvest <- project(harvest, LandTrendR, method = "near") %>%
    mask(., SA)
  harvestDT <- data.table(values(harvest) + 1900) 
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
  
  fire <- project(fire, LandTrendR, method = "near") %>%
    mask(., SA)
  burnDT <- data.table(values(fire) + 1900) 
  names(burnDT) <- "burned"
  outputDT[, pctBurned := burnDT[burned <= lastYear & burned > 1900, .N]/N * 100]
  outputDT[, pctBrnYr := pctBurned/c(lastYear - 1984)]
  rm(fire, burnDT)
  #this is the pctortion of pixels that were burned before or during
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
#TODO: check results
write.csv(results, "outputs/Caribou_Range_Disturbance_Summary.csv")
