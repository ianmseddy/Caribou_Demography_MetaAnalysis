library(terra)
library(data.table)
library(magrittr)

BCroads <- vect("C:/ian/data/Infrastructure/BC_forest_roads_WCB/BCGW_7113060B_1685058425218_15100/DRA_DGTL_ROAD_ATLAS_MPAR_SP/DRA_MPAR_line.shp")
MoberlyLT <- rast("outputs/Caribou_LandTrendR_Results/Johnson32_Moberly.tif")
NAflag(MoberlyLT) <- 0
plot(MoberlyLT)

test <- project(MoberlyLT, y = terra::cBCroads, method = "ngb", res = c(30, 30))

BCroads <- crop(BCroads, test)
terra::writeRaster(test, "C:/Ian/temp/MoberlyTest.tif")
terra::writeVector(BCroads, "C:/Ian/temp/BCroads_Moberly.shp")

plot(test$yod)
plot(BCroads, add = TRUE)
pulse <- terra::vect("C:/users/ieddy/Downloads/PULSE_2D_WEB/PULSE_2D_WEB.shp")
pulse <- project(pulse, BCroads)
pulse <- crop(pulse, BCroads)
writeVector(pulse, "C:/Ian/temp/Pulse_seismicLines_Moberly.shp")

#use the convex hull for ordering BC geodata
rangePolyBC <- terra::vect("GIS/Digitized_Caribou_StudyAreas.shp")
rangePolyBC <- rangePolyBC[rangePolyBC$Province == "BC"] %>%
  terra::convHull() %>%
  buffer(., 20000)
terra::writeVector(rangePolyBC, "GIS/temp/BC_SA_convexHull.shp")

#crop the NRN to roads
NRN <- vect("C:/Ian/Data/Infrastructure/roads/lrnf000r21a_e/lrnf000r21a_e.shp")
NRN <- terra::project(NRN, rangePoly)
NRN <- terra::mask(NRN, rangePoly)
