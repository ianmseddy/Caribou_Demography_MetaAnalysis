#generating random points for study area SK1
library(terra)
library(sf)

SK1 <- vect("GIS/Digitized_Caribou_StudyAreas.shp")

SK1 <- SK1[grep("SK1", x = SK1$PolygonID),]

#to ensure buffers do not overlap, rasterize SK1 with 5000m pixels
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
# writeVector(points, "outputs/corrected_SK_LinearFeatures/SK1_random_5000m_buffer.shp")
# these random circles were imported to Google Earth where linear features were digitized August 9-11 2023
#seismic lines narrower than 4 metres were ignored, to achieve a fair comparison

#merge the polygons
digitized_SK1_LinearFeatures <- list.files("outputs/corrected_SK_LinearFeatures/digitized_random_samples", 
                        pattern = ".shp$", full.names = TRUE) |>
  lapply(vect)
digitized_SK1_LinearFeatures <- do.call(rbind, digitized_SK1_LinearFeatures)
#calulate area
digitized_SK1_LinearFeatures$length <- perim(digitized_SK1_LinearFeatures)
#calculate area of the circles
SK1_LinearFeatures_sampleArea <- vect("outputs/corrected_SK_LinearFeatures/SK1_random_5000m_buffer.shp")
SK1_LinearFeatures_sampleArea$area <- expanse(SK1_LinearFeatures_sampleArea, unit = "km")
SK1_LinearFeatures_sampleArea$area
SK1_linear_density_mPerKm2 <- sum(digitized_SK1_LinearFeatures$length)/sum(SK1_LinearFeatures_sampleArea$area)
#SK linear feature density is 55.55 m/km2 - 26/30 polygons had no linear feaures at all

