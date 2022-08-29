library(data.table)
library(stringr)
library(ggplot2)


#dt <- dt[`Sampled Years` != ""]
#dt[, smpYears := str_replace(`Sampled Years`, pattern = "-", replacement = "")]
#dt[, firstYear := substr(smpYears, start = 1, stop = 4)]
#dt[, endYear := substr(smpYears, start = 5, stop = 8)]

obs <- fread("data/Range_Polygon_Data.csv")

newDT <- melt.data.table(data = dt, id.vars = c("Doc_ID", "Useful", "Study Area"), 
                         measure.vars = c("firstYear", "endYear"), 
                         variable.name = "series", value.name = "year")

