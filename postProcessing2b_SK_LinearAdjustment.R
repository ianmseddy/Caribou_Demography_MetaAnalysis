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
digitized_SK1_Lines <- list.files("outputs/corrected_SK_LinearFeatures/SK1/digitized_random_samples", 
                        pattern = ".shp$", full.names = TRUE) |>
  lapply(vect)
digitized_SK1_Lines <- do.call(rbind, digitized_SK1_Lines)
#calulate area
digitized_SK1_Lines$length <- perim(digitized_SK1_Lines)
#calculate area of the circles
SK1_Lines_sampleArea <- vect("outputs/corrected_SK_LinearFeatures/SK1/SK1_random_5000m_buffer.shp")
#I don't think it is necesssary to transform to lat long but just in case
SK1_Lines_sampleArea <- project(SK1_Lines_sampleArea, crs(digitized_SK1_Lines))
SK1_Lines_sampleArea$area <- expanse(SK1_Lines_sampleArea, unit = "km", transform = TRUE)
SK1_Lines_sampleArea$area
SK1_linear_density_mPerKm2 <- sum(digitized_SK1_Lines$length)/sum(SK1_Lines_sampleArea$area)
#SK linear feature density is 55.55 m/km2 - 26/30 polygons had no linear features at all
# class,length,PolygonID,mPerKm2
#length is meaningless in this case
out <- data.table(class = "unknown", length = NA, PolygonID = SK1$PolygonID, mPerKm2 = SK1_linear_density_mPerKm2)
fwrite(out, "outputs/linear_feature_stats/McLoughlin86_SK1_linear_stats.csv")

####ColdlakesSK were also sampled as there were considerable  linear features here not included in the existing datasets
# and the area was too large to digitize (in addition to the insufficient satellite imagery)
digitized_CLSK_Lines <- vect("outputs/corrected_SK_LinearFeatures/ColdLakeSK/CldLkeSK_lines.shp")
CLSK_sampleArea <- vect("outputs/corrected_SK_LinearFeatures/ColdLakeSK/CldLkeSK_randomPoints.shp")
digitized_CLSK_Lines <- project(digitized_CLSK_Lines, crs(CLSK_sampleArea))
digitized_CLSK_Lines$length <- perim(digitized_CLSK_Lines)
CLSK_sampleArea <- project(CLSK_sampleArea, crs(digitized_CLSK_Lines))
CLSK_sampleArea$area <- expanse(CLSK_sampleArea, unit = "km")
CLSK_linear_density_mPerKm2 <- sum(digitized_CLSK_Lines$length)/sum(CLSK_sampleArea$area)
CLSK_linear_density_mPerKm2
out <- data.table(class = "unknown", length = NA, PolygonID = "Hervieux87_ColdLakeSK", 
                  mPerKm2 = CLSK_linear_density_mPerKm2)
fwrite(out, "outputs/linear_feature_stats/Hervieux87_ColdLakeSK_linear_stats.csv")
