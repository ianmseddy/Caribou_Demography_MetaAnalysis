####for calculating the proportion of the older Alberta study areas covered by OrbView3 imagery
library(terra)
library(sf)
library(magrittr)
library(reproducible)

#don't use the prepackaged AOI because it assumes every image is used, even those with cloudcover > 25%

rsShape <- list.files("GIS/Linear_Features/RS imagery/Dalerum_StuartSmith", 
                      recursive = TRUE, pattern = ".shp", full.names = TRUE)
rsShape <- rsShape[grep("aoi", rsShape, invert = TRUE)]
rsShape <- rsShape[grep("src.shp", rsShape, invert = TRUE)]
rsShape <- lapply(rsShape, st_read) %>%
  do.call(rbind, .) %>%
  st_union(by_feature = FALSE) %>%
  vect()

# writeVector(rsShape, "outputs/corrected_Alberta_Polygons/OrbView_Extent.shp", overwrite = TRUE)
rangePolys <- vect("GIS/Digitized_Caribou_StudyAreas.shp")
polyIDs <- c("StuartSmith79_WSAR", "StuartSmith79_ESAR", 
             "Dalerum67_ESAR", "Dalerum67_RedEarth", "Dalerum67_CaribouMountains")
# rangePolys <- rangePolys[rangePolys$PolygonID %in% polyIDs]
# rangePolyArea <- expanse(rangePolys)

#crop the linear features to the extent of the OrbView3 imagery for faster digitization 

if (FALSE) {
  sapply(polyIDs, FUN = function(x){
    origLines <- vect(paste0("outputs/linear_features/", x, "_linear_features.shp"))
    cropped <- postProcess(origLines, cropTo = rsShape, maskTo = rsShape)
    #use postProcess as terra::mask does not crack features
    if (nrow(cropped) != 0){
      writeVector(cropped, paste0("outputs/corrected_Alberta_Polygons/", x, "_linear_features_past.shp"),
                  overwrite = TRUE)
    }
  })
}

#first, project OrvView3 extents to Range polygons
# polys <- project(polys, rangePolys)
# #then mask them to the relevant polygon - only StuartSmith_WSAR is covered at the moment
# polys <- mask(polys, mask = rangePolys)
# OrbView3Area_1 <- expanse(polys, transform = TRUE)
# names(rangePolyArea) <- rangePolys$PolygonID
# #as a percentage
# OrbView3Area_1/rangePolyArea["StuartSmith79_WSAR"] * 100 
temp <- vect("outputs/corrected_Alberta_Polygons/StuartSmith79_ESAR_linear_features_past.shp")
dalerum <- rangePolys[grep("Dalerum67_ESAR", rangePolys$PolygonID),]
dalerum <- project(dalerum, y = temp)
temp <- terra::mask(temp, dalerum, inverse = TRUE)
plot(temp)
writeVector(temp, "StuartSmith79_ESAR_linear_features_past.shp", overwrite = TRUE)
#crop the original lines with the Dalerum polygon
