# ---- READ NOAA CLIMATE DATA AND PROCESS FOR PET ESTIMATIONS ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777; 10 Dec, 2018

# Libraries ----
library(lubridate)
library(reshape2)
library(ggplot2)
library(imputeTS)

# Read in data ----
pFil <- 'E:/slz_tmp/ncdc_precip_data.txt' # Now exists in a zip file -- unpack first!
pDat <- readLines(pFil)

# Clean data ----
pHed <- pDat[1] # Isolate header
pHed <- unlist(strsplit(pHed, '\\s+'))
pHed <- pHed[-1] # Remove first entry (blank)
pHed[3] <- 'Date'
pDat <- pDat[-1] # Remove header
pDat <- data.frame(do.call('rbind', strsplit(pDat, '\\s+')), stringsAsFactors = FALSE)
names(pDat) <- pHed
pDat <- pDat[, c(1, 3, 29)] # Isolate precip data

# Process the data ----
pDat <- pDat[which(pDat$USAF != '994280' & pDat$USAF != '994026'), ] # No data for 994280/994026
pDat$USAF <- as.factor(pDat$USAF)# Factorize Stations
pDat$Date <- as.POSIXct(pDat$Date, '%Y%m%d%H%M', tz = 'America/Los_Angeles') # Coerce dates
pDat$DtHr <- ceiling_date(pDat$Date, unit = 'hour') # round up to nearest hour
pDat$PCP01 <- as.numeric(pDat$PCP01) # Remove (make NA) precip no data
coDate = as.POSIXct('2004-01-01', '%Y-%m-%d', tz = "America/Los_Angeles") # Data start in 2004
pDat <- pDat[which(pDat$Date >= coDate), ]
pDatBu <- pDat
pDat <- pDatBu
pDat <- dcast(pDat, DtHr ~ USAF, value.var = 'PCP01', fun.aggregate = sum) # aggregate to hourly
names(pDat) <- c('Date', 'AR2', 'AR1')

# Use USAF 726958 preferentially (they're identical)
pDat$P <- ifelse((is.na(pDat$AR1) | pDat$AR1 == 0), pDat$AR2, pDat$AR1) 
pDat <- pDat[, -c(2, 3)]

# Make a regular time series ----
dlims = as.POSIXct(c('2004-01-01', '2018-04-01'), '%Y-%m-%d', tz = 'America/Los_Angeles')
ts <- data.frame('DateTime' = as.POSIXct(seq(dlims[1], dlims[2], 3600),
                                         origin = '1970-01-01',
                                         tz = 'America/Los_Angeles'),
                 stringsAsFactors = FALSE)
pDat <- merge(ts, pDat, by.x = 'DateTime', by.y = 'Date', all.x = TRUE, all.y = FALSE) # Merge
pDat$P <- ifelse(is.na(pDat$P), 0, pDat$P)

# Save the data to import into the other met data function
write.csv(pDat, 'E:/slz_tmp/pcp_data.csv', row.names = FALSE)
