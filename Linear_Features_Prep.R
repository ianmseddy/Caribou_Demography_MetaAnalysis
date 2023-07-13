library(terra)
library(data.table)
library(magrittr)
library(reproducible)
library(sf)

#this is GIS preparation for the study areas that require some digitizing
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
#there are provincial versions of these NRN datasets but as far as I can tell they are no different 

#load pulse seismic lines
seismicLines <- vect("GIS/Linear_Features/CA/PULSE_2D_WEB/PULSE_2D_WEB.shp")

#utility function to crop and write 
prepOutputs <- function(infile, cropFile, outputName){
  if (class(infile) %in% "SpatVector"){
    out <- postProcess(infile, cropTo = cropFile, projectTo = cropFile)
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

#harvest year
harvestYear <- rast("GIS/CA_harvest_year_1985_2015.tif")
terra::NAflag(harvestYear) <- 0

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

sapply(c("Walker64_Nakina", "Walker64_PickleLake"), OntarioGISprep)
rm(ONroads, ONmnrf)
gc()

#the Ontario MNRF roads and the ORN are often (but not always) counting the same road. 
# will try to buffer the ORN by 30 metres and take the difference with MNRF
sapply(unique(ON$PolygonID), function(PolyID, RangeSA = RangePolygons){
  outDir <- file.path("GIS/Linear_Features/RangeSA_Digitization", PolyID)
  RangeSA <- RangeSA[RangeSA$PolygonID == PolyID]
  forestRoads_subset <- vect(file.path(outDir, paste0(PolyID, "_mnrf_roads_subset.shp")))
  roads_subset <- vect(file.path(outDir, paste0(PolyID, "_ONroads_subset.shp")))
  roads_subset <- terra::buffer(roads_subset, width = 30, joinstyle = "bevel", capstyle = "flat")
  forestRoads_crop <- erase(forestRoads_subset, roads_subset)
  writeVector(forestRoads_crop, file.path(outDir, paste0(PolyID, "_mnrf_roads_subset.shp")), 
              overwrite = TRUE)
})

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

