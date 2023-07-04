library(data.table)
library(sf)
library(ggplot2)
library(reproducible)

rangePolys <- st_read("gis/Digitized_Caribou_StudyAreas.shp")
rangePolys$Id <- NULL
Canada <- prepInputs(url = paste0("https://www12.statcan.gc.ca/census-recensement/2011/",
                                  "geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip"),
                     destinationPath = "GIS",
                     fun = "st_read")
#actually since the polygons are equal area... use somethign else
Canada <- st_transform(Canada, st_crs(rangePolys))
Canada <- st_crop(Canada, y = st_bbox(st_buffer(rangePolys, 30000)))
#multiplying the bbox doesn't work because both ys are positive

rangeData_postGIS <- fread("outputs/Caribou_Range_Disturbance_Summary.csv")
rangeData_postGIS[, V1 := NULL]

rangePolys1 <- dplyr::left_join(rangePolys, rangeData_postGIS, "PolygonID")
demoData <- fread("Range_Polygon_Data.csv")
demoData[, c("V1", "Note") := NULL]
demoData[, CalfCow := as.numeric(CalfCow)] #TODO correct this properly
demoData[CalfCow > 1.5, CalfCow := CalfCow/100]
demoData[, c("DocID", "Author") := NULL] #some authors were mismatched due to periods at end of et al

rangePolys1 <- dplyr::left_join(rangePolys1, demoData, c("PolygonID"))


out1 <- ggplot(rangePolys1) + 
  geom_sf(data = Canada, show.legend = FALSE) +
  geom_sf(data = rangePolys1, aes(fill = propHarvest), alpha = 0.5) + 
  scale_fill_continuous(type = "viridis") + 
  labs(fill = "proportion harvested: \n 1985 - time of study") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_bw() + 
  theme(legend.text=element_text(size=rel(1.2)), 
        legend.title=element_text(size=rel(1.2)))
ggsave(plot = out1, filename = "figures/propHarvest_gg.png",
       device = "png", , height = 6, width = 15)

out2 <- ggplot(rangePolys1) + 
  geom_sf(data = Canada, show.legend = FALSE) +
  geom_sf(data = rangePolys1, aes(fill = propDisturbed), alpha = 0.5) + 
  scale_fill_continuous(type = "viridis") + 
  labs(fill = "proportion disturbed: \n1985 - time of study") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_bw() + 
  theme(legend.text=element_text(size=rel(1.2)), 
        legend.title=element_text(size=rel(1.2)))
ggsave(plot = out2, filename = "figures/propDist_gg.png", 
       device = "png", , height = 6, width = 15)

out3 <- ggplot(rangePolys1) + 
  geom_sf(data = Canada, show.legend = FALSE) +
  geom_sf(data = rangePolys1, aes(fill = propBurned), alpha = 0.5) + 
  scale_fill_continuous(type = "viridis") + 
  labs(fill = "proportion burned: \n 1985 - time of study") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_bw() + 
  theme(legend.text = element_text(size=rel(1.2)), 
        legend.title = element_text(size=rel(1.2)))
ggsave(plot = out3, filename = "figures/propBurned_gg.png", 
       device = "png", , height = 6, width = 15)


out4 <- ggplot(rangePolys1) + 
  geom_sf(data = Canada, show.legend = FALSE) +
  geom_sf(data = rangePolys1, aes(fill = Meas_Years), alpha = 0.5) + 
  scale_fill_continuous(type = "viridis") + 
  labs(fill = "last year\n of study") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_bw() + 
  theme(legend.text=element_text(size=rel(1.2)), 
        legend.title=element_text(size=rel(1.2)))
ggsave(plot = out4, filename = "figures/measYears_gg.png", 
       device = "png", , height = 6, width = 15)

out5 <- ggplot(rangePolys1) + 
  geom_sf(data = Canada, show.legend = FALSE) +
  geom_sf(data = rangePolys1, aes(fill = as.numeric(Lambda)), alpha = 0.5) + 
  scale_fill_continuous(type = "viridis") + 
  labs(fill = "lambda") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_bw() + 
  theme(legend.text=element_text(size=rel(1.2)), 
        legend.title=element_text(size=rel(1.2)))
ggsave(plot = out5, filename = "figures/Lambda_gg.png", 
       device = "png", , height = 6, width = 15)

out6 <- ggplot(rangePolys1) + 
  geom_sf(data = Canada, show.legend = FALSE) +
  geom_sf(data = rangePolys1, aes(fill = as.numeric(CalfCow)), alpha = 0.5) + 
  scale_fill_continuous(type = "viridis") + 
  labs(fill = "Calf:Cow") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_bw() + 
  theme(legend.text=element_text(size=rel(1.2)), 
        legend.title=element_text(size=rel(1.2)))
ggsave(plot = out6, filename = "figures/CalfCow_gg.png", 
       device = "png", , height = 6, width = 15)

out7 <- ggplot(rangePolys1) + 
  geom_sf(data = Canada, show.legend = FALSE) +
  geom_sf(data = rangePolys1, aes(fill = as.numeric(AdultFemaleSurvivalRate)), alpha = 0.5) + 
  scale_fill_continuous(type = "viridis") + 
  labs(fill = "Adult Female \nSurvival Rate") + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_bw() + 
  theme(legend.text=element_text(size=rel(1.2)), 
        legend.title=element_text(size=rel(1.2)))
ggsave(plot = out7, filename = "figures/AdultFemaleSurvivalRate_gg.png", 
       device = "png", , height = 6, width = 15)

out8 <- ggplot(rangePolys1) + 
  geom_sf(data = Canada, show.legend = FALSE) +
  geom_sf(data = rangePolys1, aes(fill = as.numeric(meanMag)), alpha = 0.5) + 
  scale_fill_continuous(type = "viridis") + 
  labs(fill = "Mean Disturbance \nMagnitude") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
  theme_bw() + 
  theme(legend.text=element_text(size=rel(1.2)), 
        legend.title=element_text(size=rel(1.2)))
ggsave(plot = out8, filename = "figures/meanMag_gg.png", 
       device = "png", height = 6, width = 15)

outputTable <- as.data.table(rangePolys1) 
outputTable <- outputTable[, .(DocID, PolygonID, Province, 
                               `First Measurement Year`, `Last Measurement Year`,
                               Lambda, AdultFemaleSurvivalRate, CalfCow, Pregnancy,
                               propDisturbed, propHarvest, propBurned, meanMag,
                               propForest, propVeg, `Predator Control`)]
toRound <- c("propDisturbed", "propForest", "propVeg", "meanMag", "propHarvest", "propBurned")
outputTable <- unique(outputTable) #unique b/c of multipolygons
outputTable[, (toRound) := lapply(.SD, round, digits = 3), .SDcol = toRound]
outputTable[, meanMag := round(meanMag, digits = 0)]
setnames(outputTable, 
         old = c("AdultFemaleSurvivalRate",
                 "First Measurement Year",
                 "Last Measurement Year", 
                 "Pregnancy", "CalfCow",
                 "meanMag", "propDisturbed", "propBurned",
                 "propHarvest", "propForest", "propVeg",
                 "Predator Control"),
         new = c("adult female surv. rate",
                 "first msr. year",
                 "last msr. year",
                 "preg.", "Calf to Cow",
                 "mean mag", "prop. dist.", "prop. burn",
                 "prop. harv.", "prop. for.", "prop. veg.",
                 "pred. ctrl"))
write.csv(outputTable, "outputs/summary_table_all_info.csv", row.names = FALSE)
