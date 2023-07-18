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
  RangePoly <- project(RangePoly, crs(lcc))
  lcc <- crop(lcc, RangePoly) |>
    mask(RangePoly) |>
    subst(from = c(20, 31, 32), to = NA)
  
  LineFile <- file.path(outDir, paste0(PolygonID, "_linear_features.shp"))
  if (!file.exists(LineFile)){
    LineDF <- data.table(PolygonID = PolygonID, 
                         mPerKm2 = 0)
    return(NULL)
  }
  
  Lines <- vect(LineFile)
  Lines <- mask(Lines, RangePoly)
  #put lcc in lonlat for later distance calculation
  lcc <- project(lcc, y = crs(Lines), method = "near")
  RangeAreaKm <- terra::expanse(lcc, transform = TRUE, unit = "km") #true by default

  LineDF <- as.data.table(Lines) #this is in metres
  LineDF <- LineDF[, .(length = sum(length)), .(class)]  
  LineDF[, PolygonID := PolygonID]
  LineDF[, mPerKm2 := length/RangeAreaKm$area]
  #to calculate the mean minimum distance to a line 
  
  LineRas <- rasterize(Lines, y = lcc)
  # LineRas[!is.na(lcc) & is.na(LineRas)] <- 0
  # LineRas[is.na(lcc)] <- NA
  
  sys1 <- Sys.time()
  
  d2l <- terra::distance(x = LineRas, unit = "m")
  sys2 <- Sys.time()
  browser()
  d2l <- terra::mask(d2l, lcc)
  plot(d2l)
  mean(as.vector(d2l), na.rm = TRUE)
    
}

LengthToArea(PolygonID = PolygonIDs[1])
#minimum distance to lines
#this is easy - 
#1. rasterize the study area polygon, 
#2. use terra::distance, which will rasterize lines, and compute distance for every cell
#3. I don't think we need to sample, but we could? anyway distance returned in metres - take mean


