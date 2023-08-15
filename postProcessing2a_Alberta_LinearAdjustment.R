####for calculating the proportion of the older Alberta study areas covered by OrbView3 imagery
library(terra)
library(sf)
library(magrittr)
library(reproducible)
library(data.table)
#don't use the prepackaged AOI because it assumes every image is used, even those with cloudcover > 25%
#1) build complete AOI from shapefiles
rsShape <- list.files("GIS/Linear_Features/RS imagery/Dalerum_StuartSmith", 
                      recursive = TRUE, pattern = ".shp", full.names = TRUE)
rsShape <- rsShape[grep("aoi", rsShape, invert = TRUE)]
rsShape <- rsShape[grep("src.shp", rsShape, invert = TRUE)]
rsShape <- lapply(rsShape, st_read) %>%
  do.call(rbind, .) %>%
  st_union(by_feature = FALSE) %>%
  vect()

# writeVector(rsShape, "outputs/corrected_Alberta_Polygons/OrbView_Extent.shp", overwrite = TRUE)

#2) crop linear features to each
rangePolys <- vect("GIS/Digitized_Caribou_StudyAreas.shp")
polygonIDs <- c("StuartSmith79_WSAR", "StuartSmith79_ESAR", 
             "Dalerum67_ESAR", "Dalerum67_RedEarth", "Dalerum67_CaribouMountains")
# rangePolys <- rangePolys[rangePolys$PolygonID %in% polygonIDs]
# rangePolyArea <- expanse(rangePolys)

#crop the linear features to the extent of the OrbView3 imagery for faster digitization 
sapply(polygonIDs[5], FUN = function(x){
  origLines <- vect(paste0("outputs/linear_features/", x, "_linear_features.shp"))
  cropped <- postProcess(origLines, cropTo = rsShape, maskTo = rsShape)
  #use postProcess as terra::mask does not crack features
  if (nrow(cropped) != 0){
    writeVector(cropped, paste0("outputs/corrected_Alberta_Polygons/", x, "_linear_features_past.shp"),
                overwrite = TRUE)
  }
})

#after removing lines that were not present in the 2005/2006 year, files were renamed with "corrected" suffix

#3 calculate the area covered
#first, project OrvView3 extents to Range polygons
# #then mask them to the relevant polygon - only StuartSmith_WSAR is covered at the moment
rsShape <- project(rsShape, rangePolys)
areaCovered <- lapply(polygonIDs, function(x, OrbView = rsShape){
  studyArea <- rangePolys[rangePolys$PolygonID == x,]
  OrbView <- postProcess(OrbView, cropTo = studyArea, maskTo = studyArea)
  areaCovered <- sum(expanse(OrbView, transform = TRUE))
  totalArea <- sum(expanse(studyArea, transform = TRUE))
  return(data.table(PolygonID = x, percentCovered = areaCovered/totalArea * 100))
})
areaCovered <- rbindlist(areaCovered)

#calculate the new and original lines
correctLinearFootprint <- function(x){
  originalLines <- vect(paste0("outputs/corrected_Alberta_Polygons/", 
                               x, "_linear_features_past.shp"))
  correctedLines <- vect(paste0("outputs/corrected_Alberta_Polygons/",
                                x, "_linear_features_corrected.shp"))
  correctedLines$length <- terra::perim(correctedLines)
  originalLines$length <- terra::perim(originalLines)
  
  return(data.table(PolygonID = x, 
                    oldLength = sum(originalLines$length),
                    newLength = sum(correctedLines$length),
                    measureYear = unique(correctedLines$MeasureYear)))
  
}

footprints <- rbindlist(lapply(polygonIDs, correctLinearFootprint))
footprints <- footprints[areaCovered, on = "PolygonID"]
footprints[, linearOffset := newLength/oldLength]
fwrite(footprints, "outputs/corrected_Alberta_Polygons/OrbView_Linear_offset.csv")


