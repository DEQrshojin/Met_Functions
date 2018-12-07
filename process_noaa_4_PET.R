# ---- READ NOAA CLIMATE DATA AND PROCESS FOR PET ESTIMATIONS ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 16 October, 2018
#
# Libraries ----

library(utils)
library(ggplot2)
library(scales)
library(reshape2)
library(zoo)

# Read in data ----

dataDir = "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/siletz/001_data/climate/noaa/"

rawMetData = readRDS(paste0(dataDir, "metData_subdaily_raw.RData"))

rawMetData = rawMetData[which(rawMetData$USAF != "994026"), ]

# Rename the station names: 726950 = AR2; 726958 = AR1; 994280 = NWP

rawMetData$Name = ifelse(rawMetData$USAF == "726950", "AR2",
                         ifelse(rawMetData$USAF == "726958", "AR1", "NWP"))

# Data metadata matrix

stations = unique(as.character(rawMetData$USAF))

climVars = data.frame(cbind("VAR" = c("Spd", "Temp", "RHx"),
                            "NAME" = c("Wind Speed (m/s)",
                                       "Air Temperature (oC)",
                                       "Relative Humidity (%)"),
                            "COL" = c(6, 8, 10)),
                      stringsAsFactors = FALSE)

climVars[, 3] = as.numeric(unlist(climVars[, 3]))

# Loop for variables (temp, uz and RH)

metData = list(list(), list(), list())

for (var in 1 : nrow(climVars))
{
    
    metData[[var]][["min"]] = dcast(rawMetData, Date ~ Name, min, na.rm = TRUE,
                                    value.var = climVars[var, 1])
    
    metData[[var]][["mean"]] = dcast(rawMetData, Date ~ Name, mean, na.rm = TRUE,
                                     value.var = climVars[var, 1])
    
    metData[[var]][["max"]] = dcast(rawMetData, Date ~ Name, max, na.rm = TRUE,
                                    value.var = climVars[var, 1])
    
    names(metData)[var] = climVars[var, 1]
    
}

# Merge the lists back into a data frame

metData2 = data.frame(cbind(metData[["Spd"]][["min"]],
                            metData[["Spd"]][["mean"]],
                            metData[["Spd"]][["max"]],
                            metData[["Temp"]][["min"]],
                            metData[["Temp"]][["mean"]],
                            metData[["Temp"]][["max"]],
                            metData[["RHx"]][["min"]],
                            metData[["RHx"]][["mean"]],
                            metData[["RHx"]][["max"]]),
                      stringsAsFactors = FALSE)

# Remove extra dates columns

colDelete = seq(5, 33, 4)

metData2 = metData2[, -colDelete]

# Rename columns with specific variable data:
#  VAR              STAT            STN
#  U - Wind Speed   i - Minimum     AR1 - Airport Station 1
#  T - Air Temp     m - Mean        AR2 - Airport Station 2
#  R - Rel. Humid   x - Maximum     NWP - Newport Gage

varIDs = data.frame(cbind(c("U", "T", "R"), c("i", "m", "x"), c("AR1", "AR2", "NWP")),
                    stringsAsFactors = FALSE)

newNames = vector(mode="character", length = 28)

newNames[1] = "Date"

n = 2

for (i in 1 : 3) {for (j in 1 : 3) {for (k in 1 : 3)
{
    newNames[n] = paste0(varIDs[i, 1], varIDs[j, 2], varIDs[k, 3])
    n = n + 1
}}}

colnames(metData2) = newNames

# Change out Inf to NA in the Min mean DO table

for (z in 2 : ncol(metData2))
{
    metData2[, z][is.infinite(metData2[, z])] = NA
}

saveRDS(metData2, paste0(dataDir, "metData2_daily_stats.RData"))
