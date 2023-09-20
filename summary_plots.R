library(data.table)
library(sf)
library(ggplot2)
library(reproducible)

RangePolygons <- st_read("gis/Digitized_Caribou_StudyAreas.shp")
RangePolygons <- st_make_valid(RangePolygons) #Mahoney80_MiddleRinge is invalid..?
RangePolygons$Id <- NULL
Canada <- prepInputs(url = paste0("https://www12.statcan.gc.ca/census-recensement/2011/",
                                  "geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip"),
                     destinationPath = "GIS",
                     fun = "st_read")
#actually since the polygons are equal area... use somethign else
Canada <- st_transform(Canada, st_crs(RangePolygons))
Canada <- st_crop(Canada, y = st_bbox(st_buffer(RangePolygons, 30000)))
#multiplying the bbox doesn't work because both ys are positive

landscapeStats <- fread("outputs/Caribou_Range_Disturbance_Summary.csv")
landscapeStats[, V1 := NULL]

lnFootprints <- fread("outputs/Caribou_Range_LinearDisturbance_Summary.csv")

RangePolygons <- dplyr::left_join(RangePolygons, landscapeStats, "PolygonID")
RangePolygons <- dplyr::left_join(RangePolygons, lnFootprints, "PolygonID")

demographicData <- fread("data/Range_Polygon_Data.csv")
demographicData[, Note := NULL]
demographicData[CalfCow > 1.5, CalfCow := CalfCow/100]
demographicData[, c("DocID", "Author") := NULL] #some authors were mismatched due to periods at end of et al

RangePolygons <- dplyr::left_join(RangePolygons, demographicData, c("PolygonID"))

##NOTE: none of the disturbance rates are adjusted for habitat area yet
# so harvest rates are lower in BC due to presence of non-forest

#TODO: pctVeg and pctForest are still proportions..
if (max(RangePolygons$pctForest) < 50){
  RangePolygons$pctForest <- RangePolygons$pctForest * 100
  RangePolygons$pctVeg <- RangePolygons$pctVeg * 100
}

#maps
makeMapGG <- function(df = RangePolygons, CA = Canada, stat, adjustCol = NULL,
                      fillLab, outputFilename) {
  df$statOfInterest <- df[stat][[1]]
  if (!is.null(adjustCol)){
    df$statOfInterest <- df$statOfInterest/df[adjustCol][[1]] * 100
  }
  
  mygg <- ggplot(df) + 
    geom_sf(data = CA, show.legend = FALSE) +
    geom_sf(data = df, aes(fill = statOfInterest)) + 
    scale_fill_continuous(type = "viridis") + 
    labs(fill = fillLab) + 
    # guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
    theme_bw() + 
    theme(legend.text=element_text(size=rel(1.2)), 
          legend.title=element_text(size=rel(1.2)))
  ggsave(mygg, filename = outputFilename, device = "png", height = 6, width = 15)
}


###harvest rate
makeMapGG(stat = "pctHarvestYr", adjustCol = "pctVeg", 
          fillLab = "harvest rate \n(%/year) \n1985 - study period",
          outputFilename = "figures/pctHarvestYrAdj_gg.png")

makeMapGG(stat = "pctBrnYr", adjustCol = "pctVeg", 
          fillLab = "burn rate \n(%/year) \n1985 - study period",
          outputFilename = "figures/pctBrnAdj_gg.png")

#demographic 
makeMapGG(stat = "Lambda", fillLab = "λ",
          outputFilename = "figures/Lambda_gg.png")

makeMapGG(stat = "CalfCow", fillLab = "Calf:Cow",
          outputFilename = "figures/CalfCow_gg.png")

makeMapGG(stat = "AdultFemaleSurvivalRate", 
          fillLab = "Adult Female \nSurvival Rate", 
          outputFilename = "figures/AdultFemaleSurvivalRate_gg.png")

makeMapGG(stat = "Pregnancy", fillLab = "Pregnacy rate", 
          outputFilename = "figures/Pregnancy_gg.png")

makeMapGG(stat = "mPerKm2", fillLab = "Linear dist. (m/km2)", 
          outputFilename = "figures/linearDist_gg.png")


outputTable <- as.data.table(RangePolygons) 
outputTable <- outputTable[, .(DocID, PolygonID, Province, 
                               `First Measurement Year`, `Last Measurement Year`,
                               Lambda, AdultFemaleSurvivalRate, CalfCow, Pregnancy,
                               pctDisturbedYr, pctHarvestYr, pctBrnYr, meanMag,
                               propForest, propVeg, `Predator Control`)]

#so far this is only for illustrative purposes, hence rounding
toRound <- c("pctDisturbedYr", "propForest", "propVeg", "meanMag", "pctHarvestYr", "pctBrnYr")
outputTable <- unique(outputTable) #unique b/c of multipolygons
outputTable[, (toRound) := lapply(.SD, round, digits = 3), .SDcol = toRound]
outputTable[, meanMag := round(meanMag, digits = 0)]
setnames(outputTable, 
         old = c("AdultFemaleSurvivalRate",
                 "First Measurement Year",
                 "Last Measurement Year", 
                 "Pregnancy", "CalfCow",
                 "meanMag", "pctDisturbedYr", "pctBrnYr",
                 "pctHarvestYr", "propForest", "propVeg",
                 "Predator Control"),
         new = c("adult female surv. rate",
                 "first msr. year",
                 "last msr. year",
                 "preg.", "Calf to Cow",
                 "mean mag", "prop. dist.", "prop. burn",
                 "prop. harv.", "prop. for.", "prop. veg.",
                 "pred. ctrl"))
write.csv(outputTable, "outputs/summary_table_all_info.csv", row.names = FALSE)
