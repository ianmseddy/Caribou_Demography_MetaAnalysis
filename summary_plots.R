library(data.table)
library(sf)
library(ggplot2)
library(reproducible)
library(quickPlot)
library(RColorBrewer)
library(ggVennDiagram)

setDTthreads(2) #YMMV

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

makeMapGG(stat = "forestB", fillLab = "ABG (Mg/ha)",
          outputFilename = "figures/Biomass_gg.png")

makeMapGG(stat = "landscapeB", fillLab = "all land ABG (Mg/ha)",
          outputFilename = "figures/landscapeBiomass_gg.png")


#treat Stuart-Smith79 as one study area when modelling lambda
#treat Rettie92 pregnancy as one polygon


#curious 
rm(list = ls())
#### I made these figures in a separate script originally 
### hence reading in all the data again
#TODO: clean this up 

lnFootprints <- fread("outputs/Caribou_Range_LinearDisturbance_Summary.csv")
demography <- fread("data/Range_Polygon_Data.csv")
demography[, Note := NULL]
lnFootprints <- demography[lnFootprints, on = c("PolygonID")]
rsFootprints <- fread("outputs/Caribou_Range_Disturbance_Summary.csv")
setnames(rsFootprints, "lastYear", "Last_Measurement_Year")

#Province is bizarre not in any of these outputs..
prov <- sf::st_read("GIS/Digitized_Caribou_StudyAreas.shp") |>
  as.data.table()
prov <- unique(prov[, .(PolygonID, Province)])
rsFootprints <- rsFootprints[, .(PolygonID, Npixels, pctBrnYr, pctHarvestYr, pctForest, 
                                 forestB, landscapeB)]

caribouDF <- lnFootprints[rsFootprints, on = c("PolygonID")]
caribouDF <- prov[caribouDF, on = "PolygonID"]

setnames(caribouDF, "mPerKm2", "linearDist")

caribouDF[, .N, (Province)]
demography <- prov[demography, on = "PolygonID"]

provOrder <- data.table(Province = c("BC", "YK", "AB", "SK", "MB", "ON", "QC", "NF"),
                        order = 1:8)
demography <- demography[provOrder, on = "Province"]
setkey(demography, order, DocID)
demography$foo <- 1:nrow(demography)
demography$Province <- factor(demography$Province)
demography$Province <- reorder(demography$Province, demography$order)
lineTimeSeries <- ggplot(data = demography) + 
  geom_segment(aes(x = First_Measurement_Year, 
                   xend = Last_Measurement_Year,
                   y = foo, yend = foo, col = Province), 
               size = 1.2) + 
  theme_bw(base_size = 14) + 
  labs(x = "Study Period", y = "Count of study areas") + 
  scale_colour_brewer(palette = "Set2")
ggsave(lineTimeSeries, device = "png", path = "figures", 
       filename = "RangePolygon_TimeSeries.png")
