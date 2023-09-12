#accuracy assessment of the AQReseau data
library(terra)
library(reproducible)
library(data.table)
library(sf)
#take a random sample of the original AQreseau segments - 
RangePolygons <- vect("GIS/Digitized_Caribou_StudyAreas.shp")
QC <- unique(RangePolygons[RangePolygons$Province == "QC"]$PolygonID)

#overwrite is false so the accuracy assessment isn't lost... 
if (FALSE){
  makeRandomSample <- function(PolygonID, AAdir = "outputs/QC_accuracy_assessment/", targetN = 0.02,
                               lineDir = "GIS/linear_features/RangeSA_Digitization"){
    
    AQreseau <- vect(file.path(lineDir, PolygonID, paste0(PolygonID, "_QCroads_AQreseau.shp")))
    n <- sample(nrow(AQreseau), size = round(targetN * nrow(AQreseau), digits = 0), replace = FALSE)
    AA_sample <- AQreseau[n,]
    writeVector(AA_sample, file.path(AAdir, paste0(PolygonID, "_AQreseau_sample.shp")), overwrite = FALSE)
  }
  lapply(QC, makeRandomSample)
}



#new forest harvest with 2016-2020 - this should be added to postProcessing1a
forestHarvest <- rast("GIS/CA_Forest_Harvest_1985-2020/CA_Forest_Harvest_1985-2020.tif")
cropFun <- function(PolygonID, ras = forestHarvest, RangePolys = RangePolygons){
  outFileName <- paste0(PolygonID, "_harvest1985_2020.tif")
  maskFile <- RangePolygons[RangePolygons$PolygonID == PolygonID]
  postProcess(x = ras, maskTo = maskFile, cropTo = maskFile, filename2 = outFileName, 
              destinationPath = "outputs/QC_accuracy_assessment", methdo = "ngb")
}
lapply(QC, cropFun)
rm(forestHarvest, cropFun)


#accuracy of each random segment confirmed by assessing whether its inclusion or exclusion 
# was correct, based on historical Google earth imagery and occasionally Landsat imagery
# despite aiming to assess 2% of available segments, the true amount ranged from 0.25% to 1% of 
# four study areas (we did not verify McGerrigale due to low presence of roads), due to time constraints.
#type 1 and type 2 errors were influenced by the particular study year
#TODO: I should have called this QC QC - missed opportunity
#PolygonID and other RangePolygon attribtues are inconsistently included due to function changes
#the terra::intersect was not returning a perfect intsersection, so I switched to reproducible::postProcess
#however postProcess did not affix the intersection columns, which I did not realize at the time
#in case this information is relevant, I ensure they are added them here
AA <- list.files("outputs/QC_accuracy_assessment/", pattern = ".shp$", full.names = TRUE) |>
  lapply(st_read) |>
  lapply(as.data.table)
AApolygonID <- list.files("outputs/QC_accuracy_assessment/", pattern = ".shp") |>
  lapply(strsplit, split = "_AQ") |>
  lapply(sapply, "[[", 1)
addFun <- function(A, ID) {
  if (is.null(A$PolygonID)){
    A[, PolygonID := ID]  
  }
  A
}
AA <- rbindlist(Map(AA, AApolygonID, f = addFun), fill = TRUE)
AA <- AA[!is.na(Accuracy), .(PolygonID, Accuracy)]
rm(AApolygonID, addFun)

# This would have been sufficient if intersection columns were consistent
# AA <- list.files("outputs/QC_accuracy_assessment/", pattern = ".shp$", full.names = TRUE) |>
#   lapply(st_read) |> 
#   lapply(as.data.table) |>
#   rbindlist(fill = TRUE)


#fix typos
AA[Accuracy == "corect inclusion", Accuracy := "correct inclusion"]
AA[Accuracy == "corrrect inclusion", Accuracy := "correct inclusion"]

#TODO: discuss how to report - given bias in sample size + measure year
#NULL Hypothesis is that the road exists at time of measurement year
#Type 1 (false positive) = incorrect exclusion 
#Type 2 (false negative) = incorrect inclusion 
allSA <- AA[Accuracy != "unsure"]
overallError <- nrow(allSA[grep("incorrect", Accuracy)])/nrow(allSA)
c(allSA[, .N, .(Accuracy)]$N/nrow(allSA)) * 100
