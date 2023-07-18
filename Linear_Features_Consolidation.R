library(terra)
library(data.table)
setDTthreads(2) 

RangePolygons <- vect("GIS/Digitized_Caribou_StudyAreas.shp")
outputPath <- "outputs/linear_features"
if (!dir.exists(outputPath)){dir.create(outputPath)}
####merging linear features#### 
# use buffer to eliminate duplicate features when presen; decide which to include in advance
# e.g. some roads are contained in both of Ontario's road layers.
# this script exists to consolidate lines into a final layer for each study area
consolidateLines <- function(polygonID, outputDir = outputPath,
                             maskPoly = RangePolygons, patternsToDrop = NULL) {
  
  #retrieve the specific polygon for masking
  maskPoly <- maskPoly[maskPoly$PolygonID == polygonID]
  inputDir <- file.path("GIS/Linear_features/RangeSA_Digitization", polygonID)
  lineFiles <- list.files(inputDir, pattern = ".shp", full.names = TRUE)
  
  if (length(lineFiles) > 0){
  #for recording the type of line file - in case... 
  for (i in patternsToDrop){
    lineFiles <- lineFiles[grep(lineFiles, pattern = i, invert = TRUE)]
  }

  lineFileClass <- lapply(lineFiles, guessClass)
  
  lineFiles <- lapply(lineFiles, st_read)
  
  for (i in 1:length(lineFileClass)){
    lineFiles[[i]]$class <- lineFileClass[[i]]
  }
  
  lineFiles <- lapply(lineFiles, "[", c("class"))
  lineFile <- do.call(rbind, lineFiles)
  
  #terra is not masking correctly so use intersection with sf
  maskPoly <- st_as_sf(maskPoly)
  lineFile <- st_intersection(lineFile, maskPoly)
  
  #back to terra for distance calculation
  lineFile <- vect(lineFile)
  
  #project all lines to long/lat for proper length/area calculation
  lineFile <- project(lineFile, "+proj=longlat +datum=WGS84")
  
  lineFile$length <- perim(lineFile)
  outputFilename <- paste0(outputDir, "/", polygonID, "_linear_features.shp")
  writeVector(lineFile, filename = outputFilename, overwrite = TRUE)
  } else {
    message("no line files for ", polygonID)
  }
}

#helper function to assign some  consistent class attributes  - unsure if needed
guessClass <- function(x){
  possibleClasses <- c("road", "unknown", "seismic", "pipeline", "powerline", "rail")
  theClass <- sapply(possibleClasses, grep,  x = x, ignore.case = TRUE, simplify = TRUE)
  theClass <- names(unlist(theClass))
  if (length(theClass) == 0) {
    theClass <- "unknown"
  }
  return(theClass)
}

######test out the consolidation#####

PolygonIDs <- unique(RangePolygons[grep("BC", RangePolygons$Province),]$PolygonID) #unique b/c of multipolygons
lapply(PolygonIDs, consolidateLines, patternsToDrop = c("pulse", "NRN", "all"))

# 'all' is the road file unfiltered by construction date
LCC <- rast("GIS/CA_forest_VLCE_2015.tif")
#this may be faster to do with the cropped polygon... v. slow 

#calculate length/area
#terra::distance and terra::expanse after correcting landcover
#return a data.table with PolygonID, area of polygons, and class area
#for distance - lines that are closer together than 30 metres are effectively the same line
LengthToArea <- function(PolygonID, lcc = LCC, RangePoly = RangePolygons, outDir = outputPath) {
  RangePoly <- RangePoly[RangePoly$PolygonID == PolygonID,]
  RangePolyTemp <- project(RangePoly, crs(lcc))
  
  lccFile <- file.path(outDir, paste0(PolygonID, "_lcc.tif"))
  lcc <- crop(lcc, RangePolyTemp)
  lcc <- project(lcc, y = crs(RangePoly), res = c(30, 30), method = "near") 
  lcc <- mask(lcc, mask = RangePoly) |>
    trim()
  lcc <- subst(lcc, from = c(20, 31, 32), to = NA)
  writeRaster(lcc, lccFile, overwrite = TRUE)
  
  LineFile <- file.path(outDir, paste0(PolygonID, "_linear_features.shp"))
  if (!file.exists(LineFile)){
    LineDF <- data.table(PolygonID = PolygonID, 
                         mPerKm2 = 0)
    return(NULL)
  }
  
  Lines <- vect(LineFile)
  
  RangeAreaKm <- terra::expanse(lcc, transform = TRUE, unit = "km") #true by default
  
  LineDF <- as.data.table(Lines) #this is in metres
  LineDF <- LineDF[, .(length = sum(length)), .(class)]  
  LineDF[, PolygonID := PolygonID]
  LineDF[, mPerKm2 := length/RangeAreaKm$area]
  #to calculate the mean minimum distance to a line 
  
  LineRasFile <- tempfile(fileext = ".tif")
  Lines <- project(Lines, lcc)
  LineRas <- rasterize(Lines, y = lcc)
  #this is required for whitebox
  LineRas[is.na(LineRas) & !is.na(lcc)] <- 0
  writeRaster(LineRas, LineRasFile, overwrite = TRUE)
  
  outFile <- file.path(outDir, paste0(PolygonID, "_linear_distance.tif"))
  
  whitebox::wbt_euclidean_distance(input = LineRasFile, 
                                   output = outFile)
  
  return(LineDF)
}

LineDfs <- lapply(PolygonIDs, LengthToArea)


