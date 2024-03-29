library(terra)
library(data.table)
library(magrittr)
library(reproducible)
library(sf)

#this is GIS preparation for the study areas that require some digitizing
RangePolygons <- vect("GIS/Digitized_Caribou_StudyAreas.shp")
####building one NRN vector representing 2001:2021
#these NRN files were ultimately not used for anything but were helpful for 
# digitizing roads and confirming their existence at particular times
if (!file.exists("GIS/Linear_Features/CA/NRN/combinedNRN.shp")) {
  
  recrop <- function(NRN, rp = RangePolygons, year){
    NRN <- vect(NRN)
    NRN$year <- year

    rp1 <- project(rp, crs(NRN))
    # NRN <- crop(NRN, rp1) #cropping is causing self-intersections
    NRN <- mask(NRN, rp1)
    NRN <- project(NRN, rp)
    if (year == 2001){
      NRN$TYPE <- NRN$type
      NRN$NAME <- NRN$name
    }
    NRN <- NRN[, c("NAME", "TYPE", "year")]
    return(NRN)
  }
  NRN2001 <- recrop("GIS/Linear_Features/CA/NRN/archived_2001/grnf000r02ml_e_shp/grnf000r02ml_e.shp",
                       year = 2001)
  NRN2006 <- recrop("GIS/Linear_Features/CA/NRN/archived_2006/grgf000r06a_e/grgf000r06a_e.shp",
                       year = 2006)
  NRN2011 <- recrop("GIS/Linear_Features/CA/NRN/archived_2011/grnf000r11a_e/grnf000r11a_e.shp",
                       year = 2011)
  NRN2016 <- recrop("GIS/Linear_Features/CA/NRN/archived_2016/lrnf000r16a_e/lrnf000r16a_e.shp",
                       year = 2016)
  NRN2021 <- recrop("GIS/Linear_Features/CA/NRN/2021/lrnf000r21a_e/lrnf000r21a_e.shp", year = 2021)
  NRNall <- rbind(NRN2001, NRN2006, NRN2011, NRN2016, NRN2021)
  
  writeVector(NRNall, filename = "GIS/Linear_Features/CA/NRN/combinedNRN.shp", overwrite = TRUE)
  rm(NRN2001, NRN2006, NRN2011, NRN2016, NRN2021, NRNall)
  gc()
}
NRNall <- vect("GIS/Linear_Features/CA/NRN/combinedNRN.shp")

#load the other NRN vector datasets
NRN_powerlines <- vect("GIS/Linear_Features/CA/canvec_50K_CA_Res_MGT_shp/canvec_50K_CA_Res_MGT/power_line_1.shp")
NRN_pipelines <- vect("GIS/Linear_Features/CA/canvec_50K_CA_Res_MGT_shp/canvec_50K_CA_Res_MGT/pipeline_1.shp")
NRN_cmnlines <- vect("GIS/Linear_Features/CA/canvec_50K_CA_Res_MGT_shp/canvec_50K_CA_Res_MGT/communication_line_1.shp")
#there are provincial versions of these NRN datasets but as far as I can tell they are no different 

#load pulse seismic lines
seismicLines <- vect("GIS/Linear_Features/CA/PULSE_2D_WEB/PULSE_2D_WEB.shp")

#harvest year
harvestYear <- rast("GIS/CA_Forest_Harvest_1985-2020.tif")
terra::NAflag(harvestYear) <- 0

#utility function to crop and write 
prepOutputs <- function(infile, cropFile, outputName){
  if (class(infile) %in% "SpatVector"){
    out <- postProcess(infile, cropTo = cropFile, projectTo = cropFile, maskTo = cropFile)
    if (nrow(out) > 0){
      writeVector(out, filename = outputName, overwrite = TRUE)
    }
  } else {
    out <- postProcess(infile, cropTo = cropFile, projectTo = cropFile, 
                       maskTo = cropFile, res = c(30, 30), method = "near")
    if (any(!is.na(out[]))){
      writeRaster(out, filename = outputName, overwrite = TRUE)
    }
  }
}



#####Manitoba####
MB <- RangePolygons[RangePolygons$Province == "MB",]
MBroads <- vect("GIS/Linear_Features/MB/grnf046r10a_e/grnf046r10a_e.shp")
MBroads$year <- 2010
MBtemp <- vect("C:/Ian/Data/Canada/lpr_000b16a_e/lpr_000b16a_e.shp")
MBtemp <- MBtemp[MBtemp$PRENAME == "Manitoba",]
MBtemp <- project(MBtemp, NRNall)
NRNallMB <- crop(NRNall, MBtemp)
MBosmroad <- vect("GIS/Linear_features/MB/manitoba-latest-free.shp/gis_osm_roads_free_1.shp")
MBosmrail <- vect("GIS/Linear_features/MB/manitoba-latest-free.shp/gis_osm_railways_free_1.shp")
#get GIS files for each polygon
#crop the NRN roads because the file is huge
ManitobaGISprep <- function(PolyID, RangeSA = RangePolygons, roads = MBroads, NRNroads = NRNallMB, 
                            osmroad = MBosmroad, osmrail = MBosmrail, power = NRN_powerlines, 
                            pipe = NRN_pipelines, communication = NRN_cmnlines, 
                            seismic = seismicLines){
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  RangeTemplate <- rast(crs = crs(RangeSA), res = c(30, 30), extent = ext(RangeSA))
  
  prepOutputs(NRNroads, RangeSA, file.path(outDir, paste0(PolyID, "_NRNroads.shp")))
  prepOutputs(roads, RangeSA, file.path(outDir, paste0(PolyID, "_MBroads_2010.shp")))
  prepOutputs(power, RangeSA, outputName = file.path(outDir, paste0(PolyID, "_powerlines.shp")))
  prepOutputs(pipe, RangeSA, file.path(outDir, paste0(PolyID, "_pipelines.shp")))
  prepOutputs(NRN_cmnlines,RangeSA, file.path(outDir, paste0(PolyID, "_commlines.shp")))
  prepOutputs(seismic, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic.shp")))
  prepOutputs(osmroad, RangeSA, file.path(outDir, paste0(PolyID, "_osm_roads.shp")))
  prepOutputs(osmrail, RangeSA, file.path(outDir, paste0(PolyID, "_osm_rail.shp")))
  
  temp <- rast(paste0("outputs/Caribou_LandTrendR_Results/", PolyID,".tif"))
  temp <- temp$yod
  temp[temp == 0] <- NA
  prepOutputs(temp, RangeSA, file.path(outDir, paste0(PolyID,"_LandTrendR_yod.tif")))  
  prepOutputs(harvestYear, RangeSA, file.path(outDir, paste0(PolyID, "_harvestYear.tif")))

}
sapply(unique(MB$PolygonID), ManitobaGISprep)
rm(MB, MBosmrail, MBosmroad, MBroads, MBtemp, NRNallMB)
####Saskatchewan####
SK <- RangePolygons[RangePolygons$Province == "SK",]
SKroads <- vect("GIS/Linear_Features/SK/ROADSEG/ROADSEG.shp")
SKosmroad <- vect("GIS/Linear_features/SK/saskatchewan-latest-free.shp/gis_osm_roads_free_1.shp")
SKosmrail <- vect("GIS/Linear_features/SK/saskatchewan-latest-free.shp/gis_osm_railways_free_1.shp")

SaskatchewanGISprep <- function(PolyID, RangeSA = RangePolygons, roads = SKroads, NRNroads = NRNall, 
                            osmroad = SKosmroad, osmrail = SKosmrail, power = NRN_powerlines, 
                            pipe = NRN_pipelines, communication = NRN_cmnlines, 
                            seismic = seismicLines){
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  prepOutputs(NRNroads, RangeSA, file.path(outDir, paste0(PolyID, "_NRNroads.shp")))
  prepOutputs(roads, RangeSA, file.path(outDir, paste0(PolyID, "_SKroads_2010.shp")))
  prepOutputs(power, RangeSA, outputName = file.path(outDir, paste0(PolyID, "_powerlines.shp")))
  prepOutputs(pipe, RangeSA, file.path(outDir, paste0(PolyID, "_pipelines.shp")))
  prepOutputs(NRN_cmnlines,RangeSA, file.path(outDir, paste0(PolyID, "_commlines.shp")))
  prepOutputs(seismic, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic.shp")))
  prepOutputs(osmroad, RangeSA, file.path(outDir, paste0(PolyID, "_osm_roads.shp")))
  prepOutputs(osmrail, RangeSA, file.path(outDir, paste0(PolyID, "_osm_rail.shp")))
  
  temp <- rast(paste0("outputs/Caribou_LandTrendR_Results/", PolyID,".tif"))
  temp <- temp$yod
  temp[temp == 0] <- NA
  prepOutputs(temp, RangeSA, file.path(outDir, paste0(PolyID,"_LandTrendR_yod.tif")))  
  prepOutputs(harvestYear, RangeSA, file.path(outDir, paste0(PolyID, "_harvestYear.tif")))
  
}

SKPoly <- RangePolygons[RangePolygons$Province == "SK",]$PolygonID

sapply(unique(SK$PolygonID), SaskatchewanGISprep)
rm(SK, SKosmrail, SKosmroad, SKroads)

####Ontario####
ON <- RangePolygons[RangePolygons$Province == "ON",]
ONmnrf <- vect("GIS/Linear_Features/ON/MNRRDSEG/LIO-2023-04-20/MNRF_ROAD_SEGMENT.shp")
ONroads <- vect("GIS/Linear_features/ON/nrn_rrn_on_shp_en/NRN_ON_12_0_ROADSEG.shp")

#The OSM data for Ontario is limited and unlikely to be useful
OntarioGISprep <- function(PolyID, RangeSA = RangePolygons, roads = ONroads, 
                           NRNroads = NRNall, forestRoads = ONmnrf, power = NRN_powerlines, 
                           pipe = NRN_pipelines, communication = NRN_cmnlines, 
                           seismic = seismicLines){
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  
  
  MeasureYear <- unique(RangeSA$Meas_Years)
  roads$CREDATE_year <- as.numeric(substr(roads$CREDATE, start = 1, stop = 4)) #hopefully no na?  
  roads_subset <- roads[roads$CREDATE_year <= MeasureYear]
  forestRoads_subset <- forestRoads[forestRoads$YR_CONST <= MeasureYear]
  
  prepOutputs(NRNroads, RangeSA, file.path(outDir, paste0(PolyID, "_NRNroads.shp")))
  prepOutputs(power, RangeSA, outputName = file.path(outDir, paste0(PolyID, "_powerlines.shp")))
  prepOutputs(pipe, RangeSA, file.path(outDir, paste0(PolyID, "_pipelines.shp")))
  prepOutputs(NRN_cmnlines,RangeSA, file.path(outDir, paste0(PolyID, "_commlines.shp")))
  prepOutputs(seismic, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic.shp")))
  prepOutputs(forestRoads, RangeSA, file.path(outDir, paste0(PolyID, "_mnrf_roads_all.shp")))
  prepOutputs(forestRoads_subset, RangeSA, file.path(outDir, paste0(PolyID, "_mnrf_roads_subset.shp")))
  prepOutputs(roads, RangeSA, file.path(outDir, paste0(PolyID, "_ONroads_all.shp")))
  prepOutputs(roads_subset, RangeSA, file.path(outDir, paste0(PolyID, "_ONroads_subset.shp")))
  
  
  temp <- rast(paste0("outputs/Caribou_LandTrendR_Results/", PolyID,".tif"))
  temp <- temp$yod
  temp[temp == 0] <- NA
  prepOutputs(temp, RangeSA, file.path(outDir, paste0(PolyID,"_LandTrendR_yod.tif")))  
  prepOutputs(harvestYear, RangeSA, file.path(outDir, paste0(PolyID, "_harvestYear.tif")))
  
}

sapply(unique(ON$PolygonID), OntarioGISprep)
rm(ONroads, ONmnrf)
gc()


####British Columbia ####
BC <- RangePolygons[RangePolygons$Province == "BC",]
BCRoads <- vect("GIS/Linear_Features/BC/BCGW_7113060B_1685383327761_7348/DRA_DGTL_ROAD_ATLAS_MPAR_SP/DRA_MPAR_line.shp")
#TODO: discover why st_read is not finding this file - downloading as gdb from BCOGC
BCseismic <- vect("GIS/Linear_Features/BC/Legacy_2D_Seismic_Lines_with_Ecology.shp")
#at first glance the BC pipelines appears to capture stuff pulse is missing, but they overlap hugely. 
#The OSM data for BC is limited and unlikely to be useful
BCGISprep <- function(PolyID, RangeSA = RangePolygons, roads = BCRoads, 
                           NRNroads = NRNall, power = NRN_powerlines, 
                           pipe = NRN_pipelines, communication = NRN_cmnlines, 
                           seismic = seismicLines, seismic_BC = BCseismic) {
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  
  MeasureYear <- unique(RangeSA$Meas_Years)
  roads$year <- as.numeric(substr(roads$CPTRDATE, start = 1, stop = 4)) 
  roads_subset <- roads[roads$year <= MeasureYear]
  seismic$year <- as.numeric(substr(seismic$DATE_SHOT, start = 1, stop = 4))
  seismic_subset <- seismic[seismic$year <= MeasureYear]
  
  
  prepOutputs(NRNroads, RangeSA, file.path(outDir, paste0(PolyID, "_NRNroads.shp")))
  prepOutputs(power, RangeSA, outputName = file.path(outDir, paste0(PolyID, "_powerlines.shp")))
  prepOutputs(pipe, RangeSA, file.path(outDir, paste0(PolyID, "_pipelines.shp")))
  prepOutputs(NRN_cmnlines, RangeSA, file.path(outDir, paste0(PolyID, "_commlines.shp")))
  prepOutputs(seismic, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic.shp")))
  prepOutputs(seismic_subset, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic_sub.shp")))
  prepOutputs(roads, RangeSA, file.path(outDir, paste0(PolyID, "_BCroads_all.shp")))
  prepOutputs(roads_subset, RangeSA, file.path(outDir, paste0(PolyID, "_BCroads_subset.shp")))
  prepOutputs(BCseismic, RangeSA, file.path(outDir, paste0(PolyID, "_BCOGC_seismic.shp")))

  temp <- rast(paste0("outputs/Caribou_LandTrendR_Results/", PolyID,".tif"))
  temp <- temp$yod
  temp[temp == 0] <- NA
  prepOutputs(temp, RangeSA, file.path(outDir, paste0(PolyID,"_LandTrendR_yod.tif")))  
  prepOutputs(harvestYear, RangeSA, file.path(outDir, paste0(PolyID, "_harvestYear.tif")))
  
}

sapply(unique(BC$PolygonID), BCGISprep)
rm(BCroads,BCseismic)
gc()



####Alberta####
AB <- RangePolygons[RangePolygons$Province == "AB",]
ABMIdata <- "GIS/Linear_Features/AB/HFI2014_LinearFeatures_v2.gdb/HFI2014_LinearFeatures_v2.gdb/"
ABMI_roads <- vect(ABMIdata, layer = "o03_Road_Centerlines_HFI2014v2")
ABMI_powerlines <- vect(ABMIdata, layer = "o13_TransmissionLines_Centerlines_HFI2014v2")
ABMI_seismic <- vect(ABMIdata, layer = "o20_SeismicLines_Centerlines_HFI2014v2")
ABMI_rail <- vect(ABMIdata, layer = "o04_Rail_Centerlines_HFI2014v2")
#ABMI pipelines weren't publically available

#I believe ABMI is superior to every other dataset - except that Pulse has information re: year
ABGISprep <- function(PolyID, RangeSA = RangePolygons, roads = ABMI_roads, 
                      power = ABMI_powerlines, pipe = NRN_pipelines,
                      rail = ABMI_rail, seismic = seismicLines, seismic_ABMI = ABMI_seismic) {
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  
  MeasureYear <- unique(RangeSA$Meas_Years)
  seismic$year <- as.numeric(substr(seismic$DATE_SHOT, start = 1, stop = 4))
  seismic_preYear <- seismic[seismic$year <= MeasureYear]
  seismic_postYear <- seismic[seismic$year > MeasureYear]
  NoLowImpact <- seismic_ABMI[seismic_ABMI$FEATURE_TYPE != "LOW-IMPACT-SEISMIC",]

  prepOutputs(rail, RangeSA, outputName = file.path(outDir, paste0(PolyID, "_ABMI_rail.shp")))
  prepOutputs(power, RangeSA, outputName = file.path(outDir, paste0(PolyID, "_ABMI_powerlines.shp")))
  prepOutputs(pipe, RangeSA, file.path(outDir, paste0(PolyID, "_pipelines.shp")))
  prepOutputs(seismic_preYear, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic_preYear.shp")))
  prepOutputs(seismic_postYear, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic_postYear.shp")))
  prepOutputs(roads, RangeSA, file.path(outDir, paste0(PolyID, "_ABMI_roads.shp")))
  prepOutputs(seismic_ABMI, RangeSA, file.path(outDir, paste0(PolyID, "_ABMI_seismic.shp")))
  prepOutputs(NoLowImpact, RangeSA, file.path(outDir, paste0(PolyID, "_ABMI_seismic_NoLowImpact.shp")))
  
  temp <- rast(paste0("outputs/Caribou_LandTrendR_Results/", PolyID,".tif"))
  temp <- temp$yod
  temp[temp == 0] <- NA
  prepOutputs(temp, RangeSA, file.path(outDir, paste0(PolyID,"_LandTrendR_yod.tif")))  
  prepOutputs(harvestYear, RangeSA, file.path(outDir, paste0(PolyID, "_harvestYear.tif")))
  
}

sapply(unique(AB$PolygonID), ABGISprep)
rm(ABMI_powerlines, ABMI_rail, ABMIdata, ABMI_roads, ABMI_seismic)
gc()

library(googledrive)
####Quebec####
QC <- RangePolygons[RangePolygons$Province == "QC",]
#there are no railways, highways, or cycle tracks (route verte) through the QC study areas
QCroads <- vect("GIS/Linear_Features/QC/AQreseauPlus_SHP/ESRI(SHP)/Reseau_routier.shp")
  
QuebecGISprep <- function(PolyID, RangeSA = RangePolygons, roads = QCroads, NRNroads = NRNall, 
                                power = NRN_powerlines, pipe = NRN_pipelines, communication = NRN_cmnlines, 
                                seismic = seismicLines){
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  prepOutputs(NRNroads, RangeSA, file.path(outDir, paste0(PolyID, "_NRNroads.shp")))
  prepOutputs(roads, RangeSA, file.path(outDir, paste0(PolyID, "_QCroads_AQreseau.shp")))
  prepOutputs(power, RangeSA, outputName = file.path(outDir, paste0(PolyID, "_powerlines.shp")))
  prepOutputs(pipe, RangeSA, file.path(outDir, paste0(PolyID, "_pipelines.shp")))
  prepOutputs(NRN_cmnlines,RangeSA, file.path(outDir, paste0(PolyID, "_commlines.shp")))
  prepOutputs(seismic, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic.shp")))
  
  temp <- rast(paste0("outputs/Caribou_LandTrendR_Results/", PolyID,".tif"))
  temp <- temp$yod
  temp[temp == 0] <- NA
  prepOutputs(temp, RangeSA, file.path(outDir, paste0(PolyID,"_LandTrendR_yod.tif")))  
  prepOutputs(harvestYear, RangeSA, file.path(outDir, paste0(PolyID, "_harvestYear.tif")))
  
}

sapply(unique(QC$PolygonID), QuebecGISprep)
rm(QCroads)
gc()

# For removing roads that were built after measurement year.
# Pixels harvested after the measurement year are separated and buffered to ensure they overlap the road.
# The buffered pixels defer to the original classification for pixels harvested in or before the measurement year.
# Harvest year values of road segments are extracted, and the proportion of each class is determined
# (not harvested, harvested prior, harvested after). Segments with a 'Harvested after' value exceeding the threshold
# are then removed, under the assumption these roads were built later. 
QCroadAdjustment <- function(PolygonID, RangeSA = RangePolygons, threshold = .7, buffDist = 90) {
  gc()
  inDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolygonID)
  RangeSA <- RangeSA[RangeSA$PolygonID == PolygonID]
  msYr <- unique(RangeSA$Meas_Years) #unique because of multipolygons
  #the 2016-2020 period was added as of 2023
  harvestRas <- file.path("outputs/QC_accuracy_assessment", 
                          paste0(PolygonID, "_harvest1985_2020.tif"))
  if (!file.exists(harvestRas)){
    message("no harvest in ", PolygonID)
    return(NULL)
  }
  harvestRas <- rast(harvestRas)
  NAflag(harvestRas) <- 0
  roads <- vect(file.path(inDir, paste0(PolygonID, "_QCroads_AQreseau.shp")))
  roads <- project(roads, harvestRas)
  postMsHarvest <- harvestRas
  postMsHarvest[postMsHarvest <= msYr] <- NA
  preMsHarvest <- harvestRas
  preMsHarvest[preMsHarvest > msYr] <- NA
  postMsHarvestBuff <- buffer(postMsHarvest, buffDist)
  preMsHarvestBuff <- buffer(preMsHarvest, buffDist)
  
  # #respect the older 'pre' value in the buffer- as the roads are constructed prior to harvest
  #many instances of roads being built first, the AQreseau file partially overlaying, and then
  #harvest following
  harvestDT <- data.table(pixelID = 1:ncell(postMsHarvest), origVal = preMsHarvestBuff[], 
                          bufferVal = postMsHarvestBuff[])
  setnames(harvestDT, new = c("pixelID", "origVal", "buffer"))
  
  #0 = no harvest, 1 = harvested prior, 2 = harvested after
  harvestDT[, newVal := 0]
  harvestDT[origVal == TRUE, newVal := 1]
  harvestDT[newVal != 1 & buffer == TRUE, newVal := 2]
  
  postMsHarvestBuff <- setValues(postMsHarvestBuff, harvestDT$newVal)
  
  #extract the values of road segments
  #note that if x and y are different crs, this function does not error but returns nothing
  out <- terra::extract(postMsHarvestBuff, roads, fun = 'table', bind = TRUE)
  outMat <- as.matrix(out[2])

  linePixels <- rowSums(outMat)
  lineProp <- outMat/linePixels
  lineProp <- as.data.table(lineProp)
  
  if (ncol(lineProp) != 3) {
    #possible there is no harvest, only harvest after, or only harvest before... 
    #important to know if it is because harvest after or before msYr
    message("review harvest data in ", PolygonID)
    #write the vector as a subset file anyway to save headaches later
    terra::writeVector(roads, filename = paste0(inDir, "/", PolygonID, "_QCroad_subset.shp"),
                       overwrite = TRUE)
  } else {
    setnames(lineProp, new = c("no data", "preMs", "postMs"))
    lineProp[, id := out$ID]
    
    fwrite(lineProp, file.path(inDir, "road_subset_stats.csv"))
    #assign weight and subset
    roads$postMsYrHarvestWeight <- lineProp$postMs
    roads$premsYrHarvestWeight <- lineProp$preMs
    roads <- roads[roads$postMsYrHarvestWeight < threshold]
    terra::writeVector(roads, filename = paste0(inDir, "/", PolygonID, "_QCroad_subset.shp"),
                       overwrite = TRUE)
  }
}

sapply(unique(QC$PolygonID), QCroadAdjustment)


####Yukon####
####Yukon####
YK <- RangePolygons[RangePolygons$Province == "YK",]
YKroads <- vect("GIS/Linear_Features/YK/Yukon_Road_Network.shp/Yukon_Road_Network.shp")
YKtemp <- vect("C:/Ian/Data/Canada/lpr_000b16a_e/lpr_000b16a_e.shp")
YKtemp <- YKtemp[YKtemp$PRENAME == "Yukon",]
YKtemp <- project(YKtemp, NRNall)
NRNallYK <- crop(NRNall, YKtemp)
YKosmroad <- vect("GIS/Linear_features/YK/Yukon-latest-free.shp/gis_osm_roads_free_1.shp")
YKosmrail <- vect("GIS/Linear_features/YK/Yukon-latest-free.shp/gis_osm_railways_free_1.shp")
#get GIS files for each polygon
#crop the NRN roads because the file is huge
YukonGISprep <- function(PolyID, RangeSA = RangePolygons, roads = YKroads, NRNroads = NRNallYK, 
                            osmroad = YKosmroad, osmrail = YKosmrail, power = NRN_powerlines, 
                            pipe = NRN_pipelines, communication = NRN_cmnlines, 
                            seismic = seismicLines){
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  RangeTemplate <- rast(crs = crs(RangeSA), res = c(30, 30), extent = ext(RangeSA))
  
  prepOutputs(NRNroads, RangeSA, file.path(outDir, paste0(PolyID, "_NRNroads.shp")))
  prepOutputs(roads, RangeSA, file.path(outDir, paste0(PolyID, "_YKroads_2010.shp")))
  prepOutputs(power, RangeSA, outputName = file.path(outDir, paste0(PolyID, "_powerlines.shp")))
  prepOutputs(pipe, RangeSA, file.path(outDir, paste0(PolyID, "_pipelines.shp")))
  prepOutputs(NRN_cmnlines,RangeSA, file.path(outDir, paste0(PolyID, "_commlines.shp")))
  prepOutputs(seismic, RangeSA, file.path(outDir, paste0(PolyID, "_pulse_seismic.shp")))
  prepOutputs(osmroad, RangeSA, file.path(outDir, paste0(PolyID, "_osm_roads.shp")))
  prepOutputs(osmrail, RangeSA, file.path(outDir, paste0(PolyID, "_osm_rail.shp")))
  
  temp <- rast(paste0("outputs/Caribou_LandTrendR_Results/", PolyID,".tif"))
  temp <- temp$yod
  temp[temp == 0] <- NA
  prepOutputs(temp, RangeSA, file.path(outDir, paste0(PolyID,"_LandTrendR_yod.tif")))  
  prepOutputs(harvestYear, RangeSA, file.path(outDir, paste0(PolyID, "_harvestYear.tif")))
  
}
sapply(unique(YK$PolygonID), YukonGISprep)
rm(YK, YKosmrail, YKosmroad, YKroads, YKtemp, NRNallYK)



####source digitized files ####
#linear features in SK and MB study areas were digtiized by Brooke Bourbeau
#these get added to the folder above so they can be consolidated into one layer
SAproper <- unique(RangePolygons[RangePolygons$Province %in% c("SK", "MB")]$PolygonID)
DigitizedLineDir <- googledrive::drive_get(path = "https://drive.google.com/drive/folders/13Fv-rurk6rqKLspx-DXBbh3FWyLALgCr")
#eventually we can assume all study areas are present, and remove the grepping
SAsInDrive <- drive_ls(DigitizedLineDir)
whichToGet <- sapply(SAproper, grep, x = SAsInDrive$name)
ToDownload <- names(unlist(whichToGet))
options(googledrive_quiet = TRUE)
lapply(ToDownload, FUN = function(SA) {
  SAfolder <- SAsInDrive[grep(SA, SAsInDrive$name),]
  shapefiles <- drive_ls(SAfolder)
  for (i in 1:nrow(shapefiles)){
    drive_download(shapefiles[i, ], 
                   path  = file.path("GIS/Linear_Features/RangeSA_Digitization",
                                     SA, shapefiles[i, ]$name), 
                   overwrite = TRUE)
  }
})
