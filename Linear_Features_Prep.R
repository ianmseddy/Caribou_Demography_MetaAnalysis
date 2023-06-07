library(terra)
library(data.table)
library(magrittr)
library(reproducible)
library(sf)
RangePolygons <- vect("GIS/Digitized_Caribou_StudyAreas.shp")
RangePolygons <- terra::makeValid(RangePolygons)
####building one NRN vector representing 2001:2021

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
  writeVector(NRNall, filename = "GIS/Linear_Features/CA/NRN/combinedNRN.shp", overwrite =)
  rm(NRN2001, NRN2006, NRN2011, NRN2016, NRN2021, NRNall)
  gc()
}
NRNall <- vect("GIS/Linear_Features/CA/NRN/combinedNRN.shp")

#load the other NRN vector datasets
NRN_powerlines <- vect("GIS/Linear_Features/CA/canvec_50K_CA_Res_MGT_shp/canvec_50K_CA_Res_MGT/power_line_1.shp")
NRN_pipelines <- vect("GIS/Linear_Features/CA/canvec_50K_CA_Res_MGT_shp/canvec_50K_CA_Res_MGT/pipeline_1.shp")
NRN_cmnlines <- vect("GIS/Linear_Features/CA/canvec_50K_CA_Res_MGT_shp/canvec_50K_CA_Res_MGT/communication_line_1.shp")
#load pulse seismic lines
seismicLines <- vect("GIS/Linear_Features/CA/PULSE_2D_WEB/PULSE_2D_WEB.shp")

#utility function that will crop and write 
prepOutputs <- function(infile, cropFile, outputName){
  if (class(infile) %in% "SpatVector"){
    out <- postProcess(infile, cropTo = cropFile, projectTo = cropFile)
    if (nrow(out) > 0){
      writeVector(out, filename = outputName)
    }
  } else {
    out <- postProcess(infile, cropTo = cropFile, projectTo = cropFile, 
                       maskTo = cropFile, res = c(30, 30), method = "near")
    if (any(!is.na(out[]))){
      writeRaster(out, filename = outputName)
    }
  }
}

#harvest year
#TODO: figure this out 
harvestYear <- rast("C:/Ian/Data/C2C/CA_harvest_year_1985_2015.tif")

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

SK <- RangePolygons[RangePolygons$Province == "SK",]
SKroads <- vect("GIS/Linear_Features/SK/ROADSEG/ROADSEG.shp")
SKosmroad <- vect("GIS/Linear_features/SK/saskatchewan-latest-free.shp/gis_osm_roads_free_1.shp")
SKosmrail <- vect("GIS/Linear_features/SK/saskatchewan-latest-free.shp/gis_osm_railways_free_1.shp")

####Ontario####
OntarioGISprep <- function(PolyID, RangeSA = RangePolygons, roads = ONroads, NRNroads = NRNall, 
                                osmroad = ONosmroad, osmrail = ONosmrail, power = NRN_powerlines, 
                                pipe = NRN_pipelines, communication = NRN_cmnlines, 
                                seismic = seismicLines){
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  prepOutputs(NRNroads, RangeSA, file.path(outDir, paste0(PolyID, "_NRNroads.shp")))
  prepOutputs(roads, RangeSA, file.path(outDir, paste0(PolyID, "_ONroads_2010.shp")))
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

ONPoly <- RangePolygons[RangePolygons$Province == "ON",]$PolygonID

sapply(unique(ON$PolygonID), OntarioGISprep)
rm(ON, ONosmrail, ONosmroad, ONroads)
