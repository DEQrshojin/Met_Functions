# ---- READ NOAA CLIMATE DATA AND PROCESS FOR PET ESTIMATIONS ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777; 16 October, 2018
#
# Libraries ----
library(lubridate)
library(reshape2)
library(ggplot2)
library(imputeTS)

# Read in data ----
dataDir <- "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_Siletz_River_1710020405/001_data/met_data/"
rawMetData <- readRDS(paste0(dataDir, "metData_subdaily_raw.RData"))
rawMetData <- rawMetData[which(rawMetData$USAF != "994280"), ] # Remove NWPT Muni Sta. - sparse data
rawMetData <- rawMetData[complete.cases(rawMetData[, 1]), ]
coDate = as.POSIXct('2004-01-01', '%Y-%m-%d', tz = "America/Los_Angeles")
rawMetData <- rawMetData[which(rawMetData$DateTime >= coDate), ]

# Rename the station names: 726950 = AR2; 726958 = AR1
rawMetData$Name <- ifelse(rawMetData$USAF == "726950", "AR2", "AR1")
rawMetData = rawMetData[, -c(2 : 5, 7, 9 : 11)]

# Create a regularized time series based on the mean values for that hour (+/-1/2 hour)
rawMetData$yr <- year(rawMetData$DateTime)
rawMetData$mo <- month(rawMetData$DateTime)
rawMetData$dy <- day(rawMetData$DateTime)
rawMetData$hr <- hour(rawMetData$DateTime)
rawMetData$mn <- minute(rawMetData$DateTime)
rawMetData$hr <- ifelse(rawMetData$mn <= 30, rawMetData$hr, rawMetData$hr + 1)
rawMetData$dy <- ifelse(rawMetData$hr == 24, rawMetData$dy + 1, rawMetData$dy)
rawMetData$hr <- ifelse(rawMetData$hr == 24, 0, rawMetData$hr)
rawMetData$DT2 <- paste0(rawMetData$yr, '-', rawMetData$mo, '-', rawMetData$dy, ' ',
                         rawMetData$hr, ':00')
rawMetData$DT2 <- as.POSIXct(rawMetData$DT2, '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
rawMetData = rawMetData[, -c(6 : 10)]

metData = melt(rawMetData, id.vars = c('DateTime', 'DT2', 'Name'))
metData = dcast(metData, DT2 ~ Name + variable, fun.aggregate = mean, value.var = 'value')

# This is where I have selected which station to use preferrentially. That is this script creates
# only one time series based on two stations of data with slightly overlapping time periods.
# The data are identical.
metData$Spd = ifelse(is.nan(metData$AR1_Spd), metData$AR2_Spd, metData$AR1_Spd)
metData$Tmp = ifelse(is.nan(metData$AR1_Temp), metData$AR2_Temp, metData$AR1_Temp)
metData$Rhx = ifelse(is.nan(metData$AR1_RHx), metData$AR2_RHx, metData$AR1_RHx)
metData = metData[, -c(2 : 7)]
metDatPlt = melt(metData, id.vars = 'DT2')

dlims = c('2004-01-01', '2018-04-01')
dlims = as.POSIXct(dlims, '%Y-%m-%d', tz = 'America/Los_Angeles')

# set the bounds of the time vector, create a regularized sequence and merge the two
bnds <- c(min(as.numeric(metData$DT2), na.rm = TRUE), max(as.numeric(metData$DT2), na.rm = TRUE))
ts <- data.frame('DateTime' = as.POSIXct(seq(bnds[1], bnds[2], 3600),
                                        origin = '1970-01-01',
                                        tz = 'America/Los_Angeles'),
                stringsAsFactors = FALSE)
metData <- merge(ts, metData, by.x = 'DateTime', by.y = 'DT2', all.x = TRUE)
metData <- metData[complete.cases(metData[, 1]), ]

# Examine the NAs for consecutive NA values.
metDatNA <- metData[which(is.na(metData[, 2 : 4])), ] # Isolate the lines that are NA
metDatNA <- metDatNA[complete.cases(metDatNA[, 1]), ]
metDatNA$Spd <- ifelse(is.na(metDatNA$Spd), -100, NA) # Replace NA w/ a dummy and non-NA w/ NA
metDatNA$Tmp <- ifelse(is.na(metDatNA$Tmp), -200, NA)
metDatNA$Rhx <- ifelse(is.na(metDatNA$Rhx), -300, NA)
# metDatNAPlt <- melt(metDatNA, id.vars = 'DateTime') # For plotting purposes
# Looks mostly well distributed with not significant consecuting values except for windspeed
# Check consecutive NAs
consecutives <- list()
for (i in 2 : ncol(metDatNA)) {
  tmp <- metDatNA[, c(1, i)]
  tmp <- tmp[complete.cases(tmp[, 2]), ]
  tmp$con <- tmp$max <- 0
  # Loop through each time step and count consecutive ones
  for (j in 1 : nrow(tmp)) {
    if (j == 1) {
      tmp[j, 3] <- 1
    } else {
      # Hour of current time step is not equal to the hour of the previous time step plus one
      if(hour(tmp[j, 1]) != hour(tmp[j - 1, 1]) + 1) { 
        tmp[j, 3] <- 1 # Then start the counter over again at one
      } else {
        tmp[j, 3] <- tmp[j - 1, 3] + 1 # Otherwise, add to the counter
      }
    }
  }
  for (j in 1 : nrow(tmp)) {
    if (j == nrow(tmp)) {
      tmp[j, 4] <- tmp[j, 3]
    } else {
      if (tmp[j + 1, 3] == 1) {
        tmp[j, 4] <- tmp[j, 3]
      } else {
        NA
      }      
    }
  }
  tmp <- tmp[which(tmp[, 4] != 0), c(1, 4)]
  tmp$variable <- i
  consecutives[[i - 1]] <- tmp
}

consecutivesDF <- rbind(consecutives[[1]],
                        consecutives[[2]],
                        consecutives[[3]])
consecutivesDF = consecutivesDF[, c(1, 3, 2)]
plot_metfigs(consecutivesDF, dlims)

# Based on analysis of the above, I prospose to do this - if the gap is 12 hours or fewer,
# interpolate the values, otherwise, give daily mean values.
# Calculate the daily means first, then interpolate.
metData$doy <- as.numeric(strftime(metData$DateTime, format = "%j"))
metDataLong <- melt(metData, id.vars = c('DateTime', 'doy'), na.rm = TRUE) # Set to long format
dailyMeans <- dcast(metDataLong, doy ~ variable, fun.aggregate = mean)
dailyMeansPlt <- melt(dailyMeans, id.vars = 'doy') # Set to long format for plotting
metDataLong <- metDataLong[, -2]
metDataLong$variable <- as.character(metDataLong$variable)
metDatNA <- metData[which(is.na(metData[, 2 : 4])), ] # Isolate the lines that are NA
metDatNA <- metDatNA[complete.cases(metDatNA[, 1]), ]
metDatNA$Spd <- ifelse(is.na(metDatNA$Spd), -100, NA) # Replace NA w/ a dummy and non-NA w/ NA
metDatNA$Tmp <- ifelse(is.na(metDatNA$Tmp), -200, NA)
metDatNA$Rhx <- ifelse(is.na(metDatNA$Rhx), -300, NA)

row.names(metDatNA) <- 1 : nrow(metDatNA)
naFilMean <- NULL

for (i in 2 : (ncol(metDatNA) - 1)) {
  tmp <- metDatNA[, c(1, i)]
  tmp <- tmp[complete.cases(tmp[, 2]), ]
  tmp$doy <- as.numeric(strftime(tmp$DateTime, format = "%j"))
  tmp$count <- tmp$max <- tmp$con <- 0
  # Loop through each time step and count consecutive ones
  for (j in 1 : nrow(tmp)) {
    if (j == 1) {
      tmp[j, 4] <- 1
    } else {
      # Hour of current time step is not equal to the hour of the previous time step plus one
      if(hour(tmp[j, 1]) != hour(tmp[j - 1, 1]) + 1) { 
        tmp[j, 4] <- 1 # Then start the counter over again at one
      } else {
        tmp[j, 4] <- tmp[j - 1, 4] + 1 # Otherwise, add to the counter
      }
    }
  }
  # Loop to extract the last number of consecutive NA 
  for (j in 1 : nrow(tmp)) {
    if (j == nrow(tmp)) {
      tmp[j, 5] <- tmp[j, 4]
    } else {
      if (tmp[j + 1, 4] == 1) {
        tmp[j, 5] <- tmp[j, 4]
      } else {
        NA
      }      
    }
  }
  # Loop to assign the max number to all prequent consecutive time steps
  for (j in nrow(tmp) : 1) {
    if (j == nrow(tmp)) {
      tmp[j, 6] = tmp[j, 5]
    } else {
      if (tmp[j, 5] == 0) {
        tmp[j, 6] = tmp[j + 1, 6]  
      } else {
        tmp[j, 6] = tmp[j, 5]
      }
    }
  }
  # Replace NAs with 12 or more consecutive NAs:
  # Subset the daily mean values by constituent:
  tmp2 <- dailyMeans[, c(1, i)]
  # merge the tables
  tmp$doy <- ifelse(tmp$count < 12, NA, tmp$doy)
  tmp <- merge(tmp, tmp2, by.x = 'doy', by.y = 'doy', all.x = TRUE)
  names(tmp) <- c('doy', 'DateTime', 'variable', 'con', 'max', 'count', 'value')
  naFilMean <- rbind(naFilMean, tmp)
}

naFilMean <- naFilMean[, -c(1, 4 : 6)]
naFilMean$variable <- ifelse(naFilMean$variable == -100, 'Spd',
                             ifelse(naFilMean$variable == -200, "Tmp", "Rhx"))
metData <- rbind(metDataLong, naFilMean)
metData <- dcast(metData, DateTime ~ variable, fun.aggregate = mean)
metData$Spd = na.interpolation(metData$Spd, option = 'linear')
metData$Tmp = na.interpolation(metData$Tmp, option = 'linear')
metData$Rhx = na.interpolation(metData$Rhx, option = 'linear')
metData[, 2 : 4] = round(metData[, 2 : 4], 1)

saveRDS(metData, 'E:/slz_tmp/noaa_met_hourly_proc.RData', ascii = FALSE)