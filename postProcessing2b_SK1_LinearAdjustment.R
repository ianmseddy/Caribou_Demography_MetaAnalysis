#generating random points for study area SK1
library(terra)
library(sf)

SK1 <- vect("GIS/Digitized_Caribou_StudyAreas.shp")

SK1 <- SK1[grep("SK1", x = SK1$PolygonID),]

#to ensure buffers dont overlap, rasterize SK1 with 5000m pixels

# templateSK <- buffer()
# template <- rast(ext(SK1), res = c(10000), crs = crs(SK1))
# 
# template[] <- 1
# template <- mask(template, SK1)
# plot(template)
# 
# #terra is not keeping the points inside the extent.. 
# points <- terra::spatSample(x = SK1, method = "random", size = 30)
# 
# plot(points)
# plot(SK1, add = TRUE)
# 
# points <- buffer(points, 5000)
# plot(points)
# writeVector(points, "outputs/corrected_SK1_Polygons/SK1_random_5000m_buffer.shp")
#these do not change

#merge the polygons
digitized <- list.files("outputs/corrected_SK1_Polygons/digitized_random_samples", 
                        pattern = ".shp$", full.names = TRUE) |>
  lapply(vect)
digitized <- do.call(rbind, digitized)
plot(digitized)
