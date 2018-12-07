# ---- READ CLIMATE DATA FROM WEB-QUERY FILE ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 16 October, 2018
#
# Libraries ----
library(utils)
library(ggplot2)
library(scales)
library(zoo)

# Read in data by fixed column width ----

dataDir = "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz/001_data/climate/noaa/"

dataFil = "noaa_data_with_RH.txt"

rawMetData = readLines(paste0(dataDir, dataFil))

# Set up data frame headers and remove header line from data

metDataHeader = do.call("rbind", strsplit(rawMetData[2], "\\s+"))

metData = rawMetData[-c(1 : 2)] # Remove header lines

# Parse data and coerce to data frame

metData = do.call("rbind", strsplit(metData, ","))

colnames(metData) = metDataHeader

# Clean data ----

# Remove empty spaces

removeSpaces = function(x) {gsub("\\s+", "", x)}

metData = apply(metData, 1, removeSpaces)
    
metData = data.frame(t(metData), stringsAsFactors = FALSE) # transpose back

# Delete unnecessary columns, and keep the following:
# USAF - 2, DATE - 4, HRMN - 5, DIR & Q - 9 & 10, Spd & Q - 12 & 13, Temp & Q - 18 & 19
# Dewpt & Q - 20 & 21, RHx - 24, Name - 1.

keepColumns = c(2, 4, 5, 9, 10, 12, 13, 18 : 21, 24, 1)

metData2 = metData[ , keepColumns]

# Create a data frame of the climate variables (climVars) that summarizes the characteristics
# of each climate component. This includes additiona information used in plotting and analysis.
# NoData (999) to NA; Disqualifying QA marks include: 2, 3, 6, 7. For all other values see
# "all_data_documentation.txt in main folder with data. No Data Values - Wind Dir = 999, ...
# Spd = 999.9, Temp = 999.9, Dew = 999.9, RH = 999

climVars = data.frame(cbind("Var1" = c("Dir", "Spd", "Temp", "Dewpt", "RHx"),
                            "FullName" = c("Wind Direction (degrees)",
                                           "Wind Speed (m/s)",
                                           "Air Temperature (deg C)",
                                           "Dew Point Temperature (deg C)",
                                           "Relative Humidity (%)"),
                            "ymin" = c(0, 0, -20, -50, 0),
                            "ymax" = c(360, 60, 40, 30, 100),
                            "dataCol" = c(4, 6, 8, 10, 12),
                            "noDataVals" = c(999, 999.9, 999.9, 999.9, 999)),
                      stringsAsFactors = FALSE)

climVars[, 3 : 5] = as.numeric(unlist(climVars[, 3: 5]))

metData2[, climVars$dataCol] = as.numeric(unlist(metData2[, climVars$dataCol]))

# Loop through the columns and to replace NoData = NA

for (n in 1 : nrow(climVars))
{
    metData2[, climVars[n, 5]] = ifelse(metData2[, climVars[n, 5]] != climVars[n, 6],
                                        metData2[, climVars[n, 5]], NA)    
}

# Concatentate DATETIME and convert to POSIXct

metData2$DateTime = as.POSIXct(paste0(metData2$Date, metData2$HrMn),
                               "%Y%m%d%H%M", tz = "America/Los_Angeles")

metData2$Date = as.Date(metData2$Date, "%Y%m%d")

# Reorder and rename columns

newOrder = c(14, 2, 1, 4 : 12, 13)

metData3 = metData2[, newOrder]

# Factor-ize the station number and station name columns

metData3$USAF = as.factor(metData3$USAF)

metData3$Name = as.factor(metData3$Name)

# Remove data from Station 994026 due to limited duration and mimicking of 994280

metData4 = metData3[which(metData3$USAF != 994026), ]

# Plot data ----

# metPlots = list()
# 
# for (n in 2 : length(climVars$Var1))
# {
#     
#     metPlots[[n]] = ggplot(metData4) +
#                     geom_point(aes(x = DateTime, y = metData4[, climVars[n, 5]]),
#                                size = 0.25, shape = 1) +
#                     scale_y_continuous(limits = c(climVars[n, 3], climVars[n, 4]),
#                                        breaks = seq(climVars[n, 3], climVars[n, 4],
#                                                     (climVars[n, 4] - climVars[n, 3]) / 5)) +
#                     xlab("Date") + ylab(climVars[n, 2]) + theme_bw() + 
#                     facet_wrap(~USAF, ncol = 1)
# 
#     ggsave(paste0(dataDir, climVars[n, 1], ".png"), metPlots[[n]],
#            width = 10, height = 6.5, units = "in", dpi = 300)
# 
# }

saveRDS(metData4, file = paste0(dataDir, "metData.RData"),
        ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)











