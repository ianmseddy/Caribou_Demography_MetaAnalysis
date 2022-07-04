library(sf)
library(data.table)
library(terra)

CaribouRange <- st_read("C:/Ian/Data/Wildlife/Caribou/b_Car_ranges_2012_alb.shp")
CaribouRange$area <- sf::st_area(CaribouRange)/10000/100
CaribouRange[CaribouRange$LABEL== "BER",]
st_write(CaribouRange, dsn = "C:/Ian/Data/Wildlife/Caribou", layer = "b_Car_ranges_2012.kml", driver = "KML")
#files are going to be around 100 MB
example <- terra::rast("C:/users/ieddy/Downloads/BER_ON2_Change_1984_2019.tif")

#noData value not read correctly?
# example$yod@file@nodatavalue
terra::NAflag(example$yod) <- 0

example <- terra::mask(example, mask = example$yod)
terra::plot(example)

#six bands are the following:
#yod - year of disturbance
#mag - magnitude of disturbance, expressed as segment start - segment end value (delta)
#dur - duration of disturbance, expressed as subtraction of start year from end year
#preval - prechange event spectral value
#rate - the rate of spectral change
#csnr - I don't know but suspect it involves magnitude relative to rsme 


#data calculations

BER <- CaribouRange[CaribouRange$LABEL == "BER",]
max(CaribouRange$area)/BER$area * 50 #Mb for all six images
#largest polygon will be ~ 1 GB but eveyrthing should be ~ 4.5 GB, not that bad. 
#but this represents one type of change detection

sum(CaribouRange$area/BER$area * 0.052)

# 
# var mag = endVal.subtract(startVal);  // substract segment start index value from segment end index value to
# calculate the delta of segments
# var rate = mag.divide(dur);                  // calculate the rate of spectral change    
# var dsnr = mag.divide(rmse);                 // make mag relative to fit rmse

