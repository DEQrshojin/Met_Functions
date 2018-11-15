# ---- DOWNLOAD OREGON PRISM DATA ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 11 October, 2018
#
# From PRISM CLIMATE DATA:
# Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K., Taylor, G.H., Curtis, J.,
#   and Pasteris, P.P. 2008.Physiographically sensitive mapping of climatological temperature
#   and precipitation across the conterminous United States. Internation Journal Of Climatology
#   DOI: 10.1002/joc.1688

# Libraries ----

library(raster)
library(rgdal)
library(prism)
library(lubridate)

# Global variables and declarations ----

prismProjection = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
fileDirectory = 'C:/Users/rshojin/Desktop/002_GIS/003_climate/PRISM_Oregon/'
climateVariables = c("ppt", "tmean", "tmin", "tmax")
strYear = 1981
endYear = 2018
directory = function(dateVector, climVar)
{
    paste0("PRISM_",
           climVar,
           ifelse(climVar == 'ppt',
                  "_stable_4kmD2_",
                  "_stable_4kmD1_"),
           as.character(dateVector),
           "_bil")
}

# Import Oregon state boundary; climate grids will be clipped to the extents of this ----

boundaryFile = 'or_state_nad83_buff6km.shp'
oregonBoundary = shapefile(paste0(fileDirectory, boundaryFile))
oregonBoundary = spTransform(oregonBoundary, crs(prismProjection)) # Set to projection of PRISM data
cat(paste0("Processing start time: ",
           format(as.POSIXct(Sys.time(), tz = "America/Los_Angeles"), "%H:%M:%S"), "\n"))

# Set the PRISM path for the temporary storage of the national daily climate grids ----

options(prism.path = paste0(fileDirectory, "temp"))

# Data download and processing; iterated through the 4 parameters ----

cat(paste0("Started raster processing at ",
           format(as.POSIXct(Sys.time(), tz = "America/Los_Angeles"), "%H:%M:%S"), "\n"))

for (parIndex in 1 : length(climateVariables)) # Change this to adjust which parameter to download
{                                              # Currently it is set to all parameters
    
    # Create a directory for the given parameter

    saveDirectory = paste0(fileDirectory, climateVariables[parIndex])

    # if (!dir.exists(saveDirectory))
    # {
    #     dir.create(saveDirectory, showWarnings = TRUE, recursive = FALSE)
    # }

    # Download and process raster data to clip to oregon size. Assumes acquiring full years of data
    
    for (year in strYear : endYear)
    {
        
        strHydroDate = as.Date(paste0(as.character(year), "-01-01"), origin = "1970-01-01")
        
        endHydroDate = as.Date(paste0(as.character(year), "-12-31"), origin = "1970-01-01")
        
        # 1) Download daily data for one parameter for one year
        
        get_prism_dailys(climateVariables[parIndex], strHydroDate, endHydroDate, keepZip = FALSE)
        
        # 2) Create a vector of the folders that were created in the download
        
        hydroDates = format(as.Date(c(strHydroDate : endHydroDate), origin = "1970-01-01"),
                            "%Y%m%d")
        
        tempDirectory = directory(hydroDates, climateVariables[parIndex])
        
        # Iterate through each day of the year, load the raster and clip to Oregon boundary
        
        fullDirectoryList = vector()
        
        for (day in 1 : length(tempDirectory))
        {
            
            tempRasterFile = paste0(fileDirectory, "temp/", tempDirectory[day], "/",
                                    tempDirectory[day], ".bil") # Name of the PRISM raster to clip
            
            if (file.exists(tempRasterFile))
            {
                
                tempRaster = raster(tempRasterFile)
                
                tempFileName = paste0(saveDirectory, "/Oregon_", climateVariables[parIndex],
                                      "_", hydroDates[day], ".asc") # Name of the new clipped raster
                
                tempOregonRaster = crop(tempRaster, oregonBoundary, snap = 'near',
                                        filename = tempFileName, overwrite = TRUE)                
            }
            
            fullDirectoryList = c(fullDirectoryList,
                                  paste0(fileDirectory, "temp/", tempDirectory[day]))
        }
        
        # Delete the rasters in the temp folder
        
        unlink(fullDirectoryList, recursive = TRUE)
        
        cat(paste0("Completed processing ", year, " at ",
                   format(as.POSIXct(Sys.time(), tz = "America/Los_Angeles"), "%H:%M:%S"), "\n"))
    }
}