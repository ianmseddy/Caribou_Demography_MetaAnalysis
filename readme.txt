Aug 31st hand-off notes.

#### Google Earth Engine ####
#set up#
The code is saved on my GEE account with my NRCan email, so I'm not sure how to share that officially, but I cloned the repository to my github (ianmseddy/Caribou_LandTrendR), so you can fork it there
and uploaded it to your own GEE account. Or perhapst there's a way to set up some kind of shared option? Else you would also need to upload the Caribou_Range_Polygon shapefile and associated auxiliary 
files as an "asset" to GEE, and ensure it is called 'aoi'. This shouldn't be hard though. 
# LandTrendR
The script will subset the polygons one at a time, corresponding to the 'i' variable. This is annoying because I mananged to run it for every polygon at once in a Map call and it didn't take much time, 
but the bottleneck was that I couldn't export multiple images simultaneously. So instead I run one at a time (you can set up multiple runs, but it seems to take longer to export). 
However, there are duplicates in the list owing to multi-polygons, so you may want to expand the list in the console to ensure new runs aren't needless duplicates. 
Otherwise, the LandTrendR algorithm is unchanged from the defaults (it runs on the NRB spectral index, it calculates change based on the largest magnitude disturbance, 
and it masks out snow/ice/water using the USGS-provided QC layer). You could optionally run it to select "gains" instead of losses, or to track disturbance as the 'steepest slope', 
but neither seemed relevant to this project. 

#### Geospatial Analysis in R ####
The postProcessing.R script will download the LandTrendR results from my Google Drive, and it is retrieving three rasters from hard-coded file paths. I can host these on my google drive too.
The three rasters are a 2015 30m Land Cover map, a 1985-2015 Forest Wildfire time series, and a 1985-2015 Forest Harvest time series. These rasters are hosted on the NFI, and I give their URLs in the script.
Since we don't have any ranges where caribou were measured after 2015, we can ignore the potential problem of post-2015 disturbances (IAN CHECK WHAT LANDTRENDR DID HERE)
The R script iterates over each PolygonID, selecting the associated polygons and LandTrendR rasters. It crops, masks, and reprojects the global rasters to the LandTrendR result (for whatever reason, 
the reprojection would fail the other way, ie. LandTrendR-to-global raster). 
#landcover
Because LandTrendR does not distinguish between no disturbance, no data (e.g. a lake), and non-appplicable (outside the study area),
I use the LCC to determine the number of total pixels and number of forested/vegetated pixels for extra context. The current results do not include "barren" in vegetated pixels, but it may be relevant for caribou. 
However, I saved "barren" as a vegetaetd class, so if the script is rerun, it will be included. If we didn't care about landcover, we could simply mask each LandTrendR result with the polygon, and it could convert 
the disturbances that fall outside the study area to NA. However, lakes and glaciers inside the study areas would be counted as non-change, so I use the LCC. All proportions are based on the total number of pixels, 
NOT the number of vegetated or forested pixels. 
#fire and harvest
These rasters are pretty basic, with the year of disturbance coded as <year> - 1900 (so they are 0-255 integers). I simply add 1900 and count the total number of pixels with values less than or equal to the last year
of measurement, which is noted in the polygon data (as a side note, this field must be updated if new range polygons are added). The resulting count gives a proportion when divided by the total number of pixels. 
For the record, the wildfire raster is not hosted as a single raster layer online, but a composite tif. The tif includes float layers which are enormous (90 GB), which is why I write the disturbance year 
as a separate layer. 
#LandTrendR
The only two LandTrendR layers selected are the magnitude of disturbance and the year of disturbance, with rate, duration, the RMSE attributes, and the intitial spectral value excluded from further analysis.
From experience, duration is almost always 1, consistent with fire or harvest, which means rate (magnitude/duration) is the same as magnitude. Anyway, the pixels where disturbance occured in or prior to the
last measurement year are selected, mean magnitude is calculated, and these two metrics are recorded. 
The function returns a one-row data.table, so I just bind them all together after the iteration completes. 


####Outstanding Issues ####
These are documented more in-depth in the Misc Notes google doc, but essentially - the Johnson 2015 ranges (5 mountain caribou) have incorrect dates and do not actually come from Johnson collar data, but 
population surveys done by Seip et al, on behalf of BC Gov. This means the geospatial assessment of CE is also wrong, as the last 2 years would be omitted.
 The Ellington polygons likely need to be removed, as there is no useful demographic data obtainable from that paper. 
And the Mahoney et al paper needs to have 4 of 6 polygons removed, and the demographic estimate (lamdba) added for the remaining two.


#### Literature Review, Lingering Thoughts####
I think the elephant in the room is whether to include more gray literature - as at the moment we have already done so for BC (Seip reports for Boreal caribou) and SK (The SK1 paper is a University report). 
Additionally, one of the BC mountain caribou papers (Johnson 2015) is incorrectly cited, and is actually using data from a government report by Seip (that I did not manage to find). 
So I think including these repots would give us a much more robust dataset. However, we would need to devise some kind of structured approach to searching gray literature.
I also think searching for individual ranges might be fruitful (I tried this with Chilcotin, which is presently absent from our database, and almost immediately found a report by some consultants 
with usable demographic data). In hindsight, this would be a great co-op project. 

We could do a sensitivity analysis on the choice of remotely-sensed index - we use the default Normalized-Burn-Ratio, or the disturbance measure (we use largest magnitude, as opposed to 
steepest - which is the magnitude/rate, but since rate is generally 1, it should have no). 

Caribou_DemographicData_Worksheets.xlsx - Excel file used for the original lit review - I separated the sheets into CSVs inside the 'Data' folder
Caribou_LandTrendR_R.Rproj  - the R project
CaribouStudyAreas.kmz - I saved the range polygons as KMZs for Google Earth, but it might be missing a few (e.g. Mahoney_Buchans). 
data/ - this contains the demographic data, Range_Polygon_Data.csv, as well as lit review notes and some one-off worksheets used to populate the Range Polygon data. 
GIS/ - stores all the GIS data I used - lots of temp files for digitizing, like Provincial lakes and roads, that I gitignored. It also houses the CE layers used in the analysis
Misc_Cleanup.R - Dealt with some basic data cleaning and standardization of demographic parameters. Doesn't need to be run again, but it could be automated. 
outputs/ - Contains the LandTrendR outputs (Caribou_LandTrendR_Results) and the geospatial analysis result (Caribou_Range_Disturbance_Summary.csv).
This will eventually be joined with Range_Polygon_Data.csv on the PolygonID column to link CE with demographic variables 
postProcessing.R - this runs the  

 