library(terra)
RangePolygons <- vect("GIS/Digitized_Caribou_StudyAreas.shp")
RangePolygons <- terra::makeValid(RangePolygons)
outputPath <- "outputs/linear_features"
if (!dir.exists(outputPath)){dir.create(outputPath)}
####merging linear features#### 
# use buffer to eliminate duplicate features when presen; decide which to include in advance
# e.g. some roads are contained in both of Ontario's road layers.
# this script exists to consolidate lines into a final layer for each study area
#TODO: calculate length/area and median-distance-to-line
consolidateLines <- function(polygonID, outputDir = outputPath,
                             maskPoly = RangePolygons, patternsToDrop = NULL) {
  #retrieve the specific polygon for masking
  maskPoly <- maskPoly[maskPoly$PolygonID == polygonID]
  inputDir <- file.path("GIS/Linear_features/RangeSA_Digitization", polygonID)
    lineFiles <- list.files(inputDir, pattern = ".shp", full.names = TRUE)
  
  #for recording the type of line file - in case... 
  for (i in patternsToDrop){
    lineFiles <- lineFiles[grep(lineFiles, pattern = i, invert = TRUE)]
  }
  
  lineFileClass <- lapply(lineFiles, guessClass)
  
  #the files were cropped but not masked with a polygon - so that is first step
  lineFiles <- lapply(lineFiles, vect) |>
    lapply(mask, mask = maskPoly)
  
  for (i in 1:length(lineFileClass)){
    lineFiles[[i]]$class <- lineFileClass[[i]]
  }
  
  lineFiles <- lapply(lineFiles, "[", c("class"))
  lineFile <- do.call(rbind, lineFiles)
  lineFile$length <- terra::perim(lineFile)
  outputFilename <- paste0(outputDir, "/", polygonID, "_linear_features.shp")
  writeVector(lineFile, filename = outputFilename)
}

#helper function to assign some  consistent class attributes 
guessClass <- function(x){
  possibleClasses <- c("road", "unknown", "seismic", "pipeline", "powerline", "rail")
  theClass <- sapply(possibleClasses, grep,  x = x, ignore.case = TRUE, simplify = TRUE)
  theClass <- names(unlist(theClass))
  if (length(theClass) == 0) {
    theClass <- "unknown"
  }
  return(theClass)
}

PolygonIDs <- unique(RangePolygons[RangePolygons$Province == "BC",]$PolygonID) #unique b/c of multipolygons
lapply(PolygonIDs, consolidateLines, patternsToDrop = c("pulse", "NRN", "all"))
# 'all' is the road file unfiltered by construction date

test <- list.files("GIS/Linear_features/RangeSA_Digitization/Culling115_Chinchaga",
                   pattern = ".shp", full.names = TRUE)
chinchaga <- RangePolys[RangePolys$PolygonID == "Culling115_Chinchaga",]
testLines <- consolidateLines(lineFiles = test, patternsToDrop = c("pulse", "NRN", "all"),
                              maskPoly = chinchaga)


