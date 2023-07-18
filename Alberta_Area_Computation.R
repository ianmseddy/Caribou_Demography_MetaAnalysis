####for calculating the proportion of the older Alberta study areas covered by OrbView3 imagery
library(terra)
library(sf)# at the moment combining geometries is easier in SF
library(magrittr)
rs <- list.files("GIS/Linear_Features/RS imagery/Dalerum_StuartSmith", 
                 recursive = TRUE, pattern = "aoi.shp", full.names = TRUE) |>
  lapply(sf::st_read) %>%
  do.call(rbind, .) %>%
  st_union(by_feature = FALSE) %>%
  vect()

rangePolys <- vect("GIS/Digitized_Caribou_StudyAreas.shp")
polyIDs <- c("StuartSmith79_WSAR", "StuartSmith79_ESAR", 
             "Dalerum67_ESAR", "Dalerum67_RedEarth", "Dalerum67_CaribouMountains")
rangePolys <- rangePolys[rangePolys$PolygonID %in% polyIDs]
rangePolyArea <- expanse(rangePolys)

#first, project OrvView3 extents to Range polygons
polys <- project(polys, rangePolys)
#then mask them to the relevant polygon - only StuartSmith_WSAR is covered at the moment
polys <- mask(polys, mask = rangePolys)
OrbView3Area_1 <- expanse(polys, transform = TRUE)
names(rangePolyArea) <- rangePolys$PolygonID
#as a percentage
OrbView3Area_1/rangePolyArea["StuartSmith79_WSAR"] * 100 
