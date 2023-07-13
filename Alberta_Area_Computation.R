##mosaic of OrbView3


library(terra)
rs <- list.files("GIS/Linear_Features/RS imagery/Dalerum_StuartSmith", 
                 recursive = TRUE, pattern = "3v05", full.names = TRUE) |>
  lapply(rast)
polys <- lapply(rs, rast) |>
  lapply(terra::ext) |>
  lapply(vect)
polys <- lapply(polys, terra::`crs<-`, value = crs(rs[[1]]))
first <- combineGeoms(polys[[1]], polys[[2]], dissolve = TRUE)
second <- combineGeoms(polys[[3]], polys[[4]], dissolve = TRUE)
polys <- combineGeoms(first, second, dissolve = TRUE)

#get StuartSmith and Dalerum polygons

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
