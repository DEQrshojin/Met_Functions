# ---- GET CLIMATE DATA FROM NOAA OCDD ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 16 October, 2018
#

# Libraries ----
library(dplyr)
library(rgdal)
library(sf)
library(raster)
library(lubridate)

# Specify dates for data retrieval

strYear = 2004

endYear = 2018

# Specify Area of Interest (aoi) box by lat/lon ----

areaOfInterest = c(-124.1, -123.45, 44.65, 45) # xmin, xmax, ymin, ymax

# Expand the AoI by 20 percent

expansionFactor = 200 # e.g., 100 means expand the area by 100 percent, i.e., length or width x 2

areaExpansion = ((expansionFactor / 100) - 1) * c(areaOfInterest[2] - areaOfInterest[1],
                                            areaOfInterest[4] - areaOfInterest[3])

aoiExpanded = areaOfInterest + c(-areaExpansion[1] / 2, areaExpansion[1] / 2, 
                                 -areaExpansion[2] / 2, areaExpansion[2] / 2)


# Start a plot of the Area of Interest Box, basin shapefile and met stations

shapeDirectory = "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/GIS/001_data/shape/"

basinFileName = "catchments_HSPF_WGS84.shp"

basinShapfile = shapefile(paste0(shapeDirectory, basinFileName))

aoiOriginalXY = data.frame(cbind(c(areaOfInterest[1], areaOfInterest[1],
                                   areaOfInterest[2], areaOfInterest[2]),
                                 c(areaOfInterest[3], areaOfInterest[4],
                                   areaOfInterest[3], areaOfInterest[4])))

aoiExpandedXY = data.frame(cbind(c(aoiExpanded[1], aoiExpanded[1],
                                   aoiExpanded[2], aoiExpanded[2]),
                                 c(aoiExpanded[3], aoiExpanded[4],
                                   aoiExpanded[3], aoiExpanded[4])))

aoiShapefile = SpatialPoints(aoiOriginalXY, proj4string = CRS(basinShapfile@proj4string@projargs))

aoiExpShpfil = SpatialPoints(aoiExpandedXY, proj4string = CRS(basinShapfile@proj4string@projargs))

# Create a data frame of the stations and clean up ----

noaaStationList = 'ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv'

noaaStations = readLines(noaaStationList) # Raw station list file

noaaStations_df = do.call("rbind", strsplit(noaaStations, ","))

noaaStations_df = gsub("[\\\"\\+]", "", noaaStations_df)        # Remove the escapes and plus values

noaaStations_df[1, ] = gsub("\\s", "_", noaaStations_df[1, ])   # Replace spaces and "(" with "_"

noaaStations_df[1, ] = gsub("\\(", "_", noaaStations_df[1, ])   # Replace spaces and "(" with "_"

noaaStations_df[1, ] = gsub("\\)", "", noaaStations_df[1, ])    # Remove ")"

colnames(noaaStations_df) = noaaStations_df[1, ]                # Create column names from first row

noaaStations_df = noaaStations_df[-1, ]                         # Delete first row

noaaStations_df = data.frame(noaaStations_df, stringsAsFactors = FALSE) # Coerce to dataframe

noaaStations_df[, 7 : 9] = as.numeric(unlist(noaaStations_df[, 7 : 9]))

# Interrogate the NOAA stations list ----
# Which stations are within the Area of Interest (aoi) box

aoiCondition = which((noaaStations_df$LON >= aoiExpanded[1] &
                      noaaStations_df$LON <= aoiExpanded[2]) &
                     (noaaStations_df$LAT >= aoiExpanded[3] &
                      noaaStations_df$LAT <= aoiExpanded[4]))

aoiStations = noaaStations_df[aoiCondition, ]

stationsShpfil = SpatialPoints(aoiStations[, 8 : 7],
                               proj4string = CRS(basinShapfile@proj4string@projargs))

plot(aoiExpShpfil)

plot(stationsShpfil, add = TRUE, col = 'red')

plot(aoiShapefile, add = TRUE)

plot(basinShapfile, add = TRUE)

# Station dates

aoiStations$BEGIN = as.Date(aoiStations$BEGIN, "%Y%m%d")

aoiStations$END = as.Date(aoiStations$END, "%Y%m%d")

aoiStationsSelected = aoiStations[year(aoiStations$END) >= endYear, ]

selectedStationsShape = SpatialPoints(aoiStationsSelected[, 8 : 7],
                                      proj4string = CRS(basinShapfile@proj4string@projargs))

plot(selectedStationsShape, add = TRUE, col = 'blue')

# Data for Newport station #994280 were only available from 1985 - 2008
# 

# Retrieve the data from the selected stations ----

saveDir = "E:/slz_tmp/"

baseURL = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/"

strYear = 2004

endYear = 2018

years4Download = strYear : endYear

stationID = aoiStations[1, ]

weatherFileURL = paste0(baseURL, years4Download, "/",
                        stationID[, 1], "-",
                        stationID[, 2], "-",
                        years4Download, ".gz")

weatherFilDest = paste0(saveDir, "corv_muni_ap_1_",
                        stationID[, 1], "-",
                        stationID[, 2], "-",
                        years4Download, ".gz")

for (year in 1 : length(weatherFileURL))
{
    download.file(weatherFileURL[year],
                  weatherFilDest[year],
                  "libcurl", quiet = FALSE, mode = "w", cacheOK = TRUE)
}

