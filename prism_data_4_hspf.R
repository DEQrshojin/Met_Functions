# ---- Interrogate Oregon PRISM rasters at a point or a data.frame of point data ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 11 October 2018
# 
# From PRISM CLIMATE DATA:
# Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K., Taylor, G.H., Curtis, J.,
#   and Pasteris, P.P. 2008. Physiographically sensitive mapping of climatological temperature
#   and precipitation across the conterminous United States. Internation Journal Of Climatology
#   DOI: 10.1002/joc.1688

# Pass: lat/lon or df of lat/lon
# Pass: list (vector) of parameters: 



# Libraries ----
library(raster)
library(rgdal)
library(lubridate)
library(ggplot2)
library(reshape2)

# Variable and path declarations ----

fileDirectory = 'C:/Users/rshojin/Desktop/002_GIS/003_climate/PRISM_Oregon/'

climateVariable = 'tmax'

# Import of shapefile of basin centroids (make a variable when this becomes a function) ----

shpFilePath = "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/GIS/001_data/shape/"

shpFileName = "catchment_centroids_HSPF_wgs84.shp"

shpFile = shapefile(paste0(shpFilePath, shpFileName))

# Create a vector of dates, file dates, raster names, and raster paths ----

strDate = as.Date("1981-01-01", origin = "1970-01-01")

endDate = as.Date("2018-03-31", origin = "1970-01-01")

tsDates = format(as.Date(c(strDate : endDate), origin = "1970-01-01"),
                 "%Y-%m-%d")

fileDates = format(as.Date(c(strDate : endDate), origin = "1970-01-01"),
                   "%Y%m%d")

rasterFileNames = paste0("Oregon_", climateVariable, "_", fileDates, ".asc")

rasterDirectory = paste0(fileDirectory, climateVariable)

rasterFilePaths = paste0(rasterDirectory, "/", rasterFileNames)

# Create an empty data from of points for extraction ----

climateTS = data.frame(matrix(ncol = length(shpFile), nrow = 0))

# Load and interrogate rasters at the specified points to create a data from of precip ts data ----

for (raster in 1 : length(rasterFileNames))
{
    
    tempRaster = raster(rasterFilePaths[raster])
    
    climateTS = rbind(climateTS, extract(tempRaster, shpFile))
    
}

climateTS = cbind(tsDates, climateTS)

colnames(climateTS) = c("DATE", shpFile@data$HSPF_Bas)

write.csv(climateTS,
          paste0(fileDirectory,
                 "siletz_HSPF_",
                 climateVariable, "_",
                 year(strDate), "_",
                 year(endDate), ".csv"),
          row.names = FALSE)

# # Summary of the annual rainfall data by basin ----
# 
# climateTS$Month = month(climateTS$DATE)
# climateTS$Year = year(climateTS$DATE)
# 
# # Recast the data
# 
# climateTSmelted = melt(climateTS,
#                        id.vars = c("DATE", "Month", "Year"),
#                        variable.name = "Basin_No",
#                        value.name = climateVariable)
# 
# # Original units are in mm, so convert to inches/year
# 
# if(climateVariable == 'ppt')
# {
#     climateTSmelted$ppt = climateTSmelted$ppt / 25.4    
# }
# 
# 
# # Aggregate daily data into annual summaries per basin
# 
# climateSummaryByYear = dcast(climateTSmelted,
#                              Year ~ Basin_No,
#                              fun.aggregate = mean,
#                              value.var = climateVariable)
# 
# climateSummaryByYear = melt(climateSummaryByYear,
#                             id.vars = "Year",
#                             variable.name = "Basin_No",
#                             value.name = climateVariable)
# 
# # Box plot of annual rainfall per basin
# 
# siletzAnnualPrecip <- ggplot() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
#     geom_boxplot(data = climateSummaryByYear,
#                  aes(x = Basin_No, y = climateVariable, group = Basin_No),
#                  outlier.shape = 1, outlier.size = 1) +
#     ylab("temperature (oC)") + xlab("HSPF Basin Number") +  
#     ggtitle("PRISM Annual Precipitation Totals by HSPF Basin")
# 
# pngSaveDir = "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\005_reporting\\figures\\modeling_memo\\"
# 
# ggsave(paste0(pngSaveDir, "HSPF_basins_annual_precip.png"), siletzAnnualPrecip,
#        width = 6.5, height = 5, units = "in", dpi = 300)



