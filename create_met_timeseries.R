# CREATE POTENTIAL EVAPOTRANSPIRATION TIMESERIES ----
# Utilizes the Penman-Monteith Method
# Ryan Shojinaga, Water Quality Analyst
# Watershed Management Section, Oregon DEQ
# Shojinaga.Ryan@DEQ.State.OR.US, 503-229-5777
# 22 October 2018

# General data info ----
# Rename columns with specific variable data:
#  VAR              STAT            STN
#  U - Wind Speed   i - Minimum     AR1 - Airport Station 1
#  T - Air Temp     m - Mean        AR2 - Airport Station 2
#  R - Rel. Humid   x - Maximum     NWP - Newport Gage

# Rename the station names: 726950 = AR2; 726958 = AR1; 994280 = NWP

# LIBRARIES ----

library(lubridate)
library(zoo)
library(ggplot2)
library(stats)
library(reshape2)
library(magrittr)
library(imputeTS)
library(Hmisc)

# WORKING DIRECTORY ----

dataDir = "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\Middle_Siletz_River_1710020405\\001_data\\met_data\\"

# WINDSPEED ----

# Read data

metData = readRDS(paste0(dataDir, "metData2_daily_stats.RData"))

# Replace NaN with NA

metData <- data.frame(sapply(metData, function(x) ifelse(is.nan(x), NA, x)))

# Convert dates

metData$Date = as.Date(metData$Date, origin = "1970-01-01")

# Subset the data: NWP - AR1

dataSubset = subset(metData, is.na(UmNWP) == FALSE & is.na(UmAR1) == FALSE, select = c(UmNWP,UmAR1))

# Regression model

uzReg1 = lm(dataSubset$UmAR1 ~ dataSubset$UmNWP)

uzSumm1 = summary(uzReg1)

uzCoeff1 = uzSumm1[["coefficients"]]

# Based on the results of the regression between Newport and the Airport 1 data, the Airport 1 data
# regression is:
# y = 0.60 x + 0.69, p-vals << 0.05, R2 = 0.65

# Subset the data: NWP - AR2

dataSubset = subset(metData, is.na(UmNWP) == FALSE & is.na(UmAR2) == FALSE, select = c(UmNWP,UmAR2))

# Regression model

uzReg2 = lm(dataSubset$UmAR2 ~ dataSubset$UmNWP)

uzSumm2 = summary(uzReg2)

uzCoeff2 = uzSumm2[["coefficients"]]

# Based on the results of the regression between Newport and the Airport 1 data, the Airport 1 data
# regression is:
# y = 0.775 x, p-val for slope << 0.05, R2 = 0.90 (p-val for int = 0.7 - assumed to be 0)

# Create the time series of wind data -- Use Newport (NWP) preferrentially, then use AR2, then AR1
# to fill gaps where AR2 doesn't

windTS = metData[, c("Date", "UmNWP")]

windTS[, 2] = ifelse(is.na(windTS[, 2]),
                     ifelse(is.na(metData[, "UmAR2"]),
                            0.60 * metData[, "UmAR1"] + 0.69,
                            0.775 * metData[, "UmAR2"]),
                            windTS[, 2]) # Based on the regressions above

# Manually fill NAs with 0 values

windTS[which(is.na(windTS[, 2])), 2] = 0

# TEMPERATURE ----

# Read in temperature data

tMin = rhMin = read.csv(paste0(dataDir, "siletz_HSPF_tmin_1981_2018.csv"),
                        stringsAsFactors = FALSE)

tMax =  rhMax = read.csv(paste0(dataDir, "siletz_HSPF_tmax_1981_2018.csv"),
                         stringsAsFactors = FALSE)

tMin$DATE = as.Date(tMin$DATE)
tMax$DATE = as.Date(tMax$DATE)
rhMin$DATE = as.Date(rhMin$DATE)
rhMax$DATE = as.Date(rhMax$DATE)

# RELATIVE HUMIDITY ----
# Extracted from PRISM (OSU) data from specific points and aggregated based on 
# the following basins due to climate similarity
# 1 - Lat: 44.9191; Lon: -123.7021; Elevation: 1535ft (468m)
# 2 : 6 - Near Sunshine confluence
# 7 : 15 & 17 - Near Logsden
# 16 - Centroid of Basin 16

# Read in Dewpoint data

suffixFiles = c("Basin_1", "Basin_2_6", "Basin_7_15", "Basin_16")

dewpTemp = read.csv(paste0(dataDir, "PRISM_tdmean_stable_4km_19810101_20180331_",
                           suffixFiles[1], ".csv"), skip = 10, header = TRUE,
                    stringsAsFactors = FALSE)

colnames(dewpTemp) <- c("Date", suffixFiles[1])

for (n in 2 : 4)
{

    dewpTemp = cbind(dewpTemp,
                     read.csv(paste0(dataDir, "PRISM_tdmean_stable_4km_19810101_20180331_",
                                     suffixFiles[n], ".csv"), skip = 10, header = TRUE))
    
    colnames(dewpTemp)[colnames(dewpTemp) == "tdmean..degrees.F."] <- suffixFiles[n]
    
    dewpTemp = dewpTemp[, -(n + 1)]
    
}

# Convert from deg F to deg C

dewpTemp[, 2 : length(dewpTemp)] = round((5 / 9) * (dewpTemp[, 2 : length(dewpTemp)] - 32), 2)

# Expand into a matrix with one dewpoint column for each basin

dewpTemp = dewpTemp[, c(1, 2, rep(3, 5), rep(4, 9), 5, 4)]

colnames(dewpTemp) = c("Date", as.character(seq(1, 17, 1)))

dewpTemp$Date = as.Date(dewpTemp$Date)

# The following calculations for relative humidity are found in
# Vaisala Oyj 2013. Humidity Conversion Forumula.

m = 7.591386

tn = 240.7263

relHumFunc = function(td, ta) {100 * 10^(m * td / (td + tn) - m * ta / (ta + tn))} 

for (column in 2 : length(dewpTemp))
{
    
    rhMin[, column] = relHumFunc(dewpTemp[, column], tMax[, column])
    
    rhMax[, column] = ifelse(relHumFunc(dewpTemp[, column], tMin[, column]) > 100, 100,
                             relHumFunc(dewpTemp[, column], tMin[, column]))
    
}

# SUNSHINE HOURS ----

sunHours = read.csv(paste0(dataDir, "solar_hours_timeseries.csv"),
                    stringsAsFactors = FALSE)

sunHours$Date = as.Date(sunHours$Date)

# AGGREGATE THE TIME SERIES DATA INTO LIST ----
# See methods for the package "Evapotranspiration"

# Need to create a list of n lists for each basin

baseData = list()

dates = c(strDate = max(min(tMin$DATE), min(sunHours$Date), min(windTS$Date)),
          endDate = min(max(tMin$DATE), max(sunHours$Date), max(windTS$Date)))

timeSeries = seq(dates[1], dates[2], 1)

# Create dates list

baseData[["Date.daily"]] = timeSeries

# Create yearmon list

baseData[["Date.monthly"]] = as.yearmon(baseData[["Date.daily"]][which(day(baseData[["Date.daily"]]) == 1)])

# Create Julian Day list

tmpDates = as.POSIXlt(baseData[["Date.daily"]])

jDay = data.frame(cbind(format(timeSeries, "%Y-%m-%d"), tmpDates$yday + 1),
                  stringsAsFactors = FALSE)

jDay$X1 = as.Date(jDay$X1)

jDay = zoo(as.numeric(jDay$X2), order.by = jDay$X1)

baseData[["J"]] = jDay

# Other timeseries data

baseData[["i"]] = month(baseData[["Date.monthly"]]) # Add i = month

baseData[["Ndays"]] = monthDays(baseData[["Date.monthly"]]) # add numer of days per month

# Add wind time series

# Regularize

uzTS = zoo(windTS$UmNWP, baseData[["Date.daily"]])

# Interpolate NA values

uzTS = na.interpolation(uzTS, option = "linear")

# Add to wind to the data list

baseData[["uz"]] = uzTS

# Add sunshine hours

sunHoursTS = zoo(sunHours$hours, baseData[["Date.daily"]])

baseData[["n"]] = sunHoursTS

ETMetData = list(list())

for (n in 1 : 17) # Number of basins or regions of specific data
{
    
    ETMetData[[n]] = baseData
    
    # Add maximum temperatures
    tMaxTmp = tMax[tMax$DATE %in% timeSeries, c(1, n + 1)]
    tMaxTmp = zoo(tMaxTmp[, 2], baseData[["Date.daily"]])
    ETMetData[[n]][["Tmax"]] = tMaxTmp
    
    # Add dewpoint temperatures
    tDewTmp = dewpTemp[dewpTemp$Date %in% timeSeries, c(1, n + 1)]
    tDewTmp = zoo(tDewTmp[, 2], baseData[["Date.daily"]])
    ETMetData[[n]][["Tdew"]] = tDewTmp

    # Add minimum temperatures
    tMinTmp = tMin[tMin$DATE %in% timeSeries, c(1, n + 1)]
    tMinTmp = zoo(tMinTmp[, 2], baseData[["Date.daily"]])
    ETMetData[[n]][["Tmin"]] = tMinTmp
    
    # Add maximum relative humidity
    rhMaxTmp = rhMax[rhMax$DATE %in% timeSeries, c(1, n + 1)]
    rhMaxTmp = zoo(rhMaxTmp[, 2], baseData[["Date.daily"]])
    ETMetData[[n]][["RHmax"]] = rhMaxTmp
    
    # Add minimum relative humidity
    rhMinTmp = rhMin[rhMin$DATE %in% timeSeries, c(1, n + 1)]
    rhMinTmp = zoo(rhMinTmp[, 2], baseData[["Date.daily"]])
    ETMetData[[n]][["RHmin"]] = rhMinTmp
    
}

saveRDS(ETMetData, paste0(dataDir, "full_met_for_PET.RData"))







    

    
    