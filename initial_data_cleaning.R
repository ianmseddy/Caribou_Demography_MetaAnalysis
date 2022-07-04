library(data.table)
library(stringr)
library(ggplot2)

dt <- fread("inputs/Caribou_DemographicData_March 2021 - SA descriptions.csv")
#temporary cleaning
dt <- dt[Useful %in% c("Yes", "Yes but", "Unsure")]
dt[`Sampled Years` == "2011-2046", `Sampled Years` := c("2011-2016")]
dt <- dt[`Study Area` != "Alaska"]

Provinces <- data.table(short = c("BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "NF", "PE", "YK", "NT", "NU"), 
                        long = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec",
                                  "New Brunswick", "Nova Scotia", "Newfoundland", "Prince Edward Island", 
                                  "Yukon", "Northwest Territories", "Nunavut"))

#Labrador is different
Labrador <- grep("Labrador", x = dt$`Study Area`)
grepLength <- function(x){unlist(length(grep(pattern = x, x = dt$`Study Area`)))}
Provinces[, count := grepLength(short), .(short)]
Provinces[, count2 := grepLength(long), .(long)]
Provinces[, fullCount := count + count2]
Provinces[, c("count2", "count") := NULL]
Provinces[short == "NF", fullCount := fullCount + length(Labrador)]


library(ggplot2)
ggplot(Provinces, aes(x = short, y = fullCount)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Province", y = "appearance in study area") + 
  theme_bw()

dt <- dt[`Sampled Years` != ""]
dt[, smpYears := str_replace(`Sampled Years`, pattern = "-", replacement = "")]
dt[, firstYear := substr(smpYears, start = 1, stop = 4)]
dt[, endYear := substr(smpYears, start = 5, stop = 8)]


newDT <- melt.data.table(data = dt, id.vars = c("Doc_ID", "Useful", "Study Area"), measure.vars = c("firstYear", "endYear"), variable.name = "series", value.name = "year")
newDT[, year := as.numeric(year)]

dups <- newDT[`Study Area` %in% c("Quebec-Labrador", "Quebec Ontario", "Alberta, BC, NWT")]


newDT[, Doc_ID := as.character(Doc_ID)]
newDT <- newDT[ order(year),]
ggplot(newDT, aes(x = year, y = reorder(Doc_ID, year))) + 
  geom_line(aes(group = Doc_ID)) + 
  geom_point(size = 2) + 
  theme_bw() + 
  labs(y = "Study", x = "study period") + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

