#merging linear features - 
#we use a 50m buffer to remove linear features that fall within the same area, to avoid overcounting
#and to eliminate duplicate features when present (e.g. some roads are contained in both of Ontario's
#road layers)
consolidateLines <- function(lineFiles, buffDist = 50,
                             patternsToDrop = NULL, maskPoly) {
  
  #for recording the type of line file
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
  return(lineFile)
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
#still need to add some random point sampling to get median distance to linear feature 
