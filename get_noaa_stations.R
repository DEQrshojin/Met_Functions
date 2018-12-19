# ---- GET STATIONS FROM FROM NOAA OCDD ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 19 December 2018
# Returns a data frame of NOAA stations based on user arguments of lat/lon box OR shapefile,
# dates and factor of AOI expansion. 

get_noaa_stations <- function(aoI = NULL, expF = NULL, strD = NULL, endD = NULL) {
  
  # LIBRARIES ----
  library(raster)
  library(sp)
  
  # AREA OF INTEREST ----
  aoiCRS = crs('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
  if (!is.numeric(aoI)) {
    
    aoI = spTransform(aoI, aoiCRS)
    
    aoI = aoI@bbox
    
    aoI = c(aoI[1, 1], aoI[1, 2], aoI[2, 1], aoI[2, 2]) # min X, max X, min Y, max Y

  }
  
  # DATES ----
  if (is.null(strD)) {strD = as.Date('1900-01-01', '%Y-%m-%d')}
  
  if (is.null(endD)) {endD = as.Date(Sys.time())}
  
  dates = as.Date(c(strD, endD))
  
  # EXPANSION FACTOR ----
  if (is.null(expF)) {expF = 1}

  areaEx = (expF - 1) * c(aoI[2] - aoI[1], aoI[4] - aoI[3])
  
  aoI = aoI + c(-areaEx[1] / 2, areaEx[1] / 2, -areaEx[2] / 2, areaEx[2] / 2)

  # STATION INTERROGATION ----
  allStat = station_list()
  
  aoiIndx = which((allStat$LON >= aoI[1] & allStat$LON <= aoI[2]) &
                    (allStat$LAT >= aoI[3] & allStat$LAT <= aoI[4]))
  
  aoiStns = allStat[aoiIndx, ]
  
  aoiStns$BEGIN = as.Date(aoiStns$BEGIN, "%Y%m%d")
  
  aoiStns$END = as.Date(aoiStns$END, "%Y%m%d")
  
  aoiStns = aoiStns[aoiStns$END >= dates[1], ]
  
  return(aoiStns)
  
}

station_list <- function() {
  
  noaaStationList = 'ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv'
  
  noaaStations = readLines(noaaStationList) # Raw station list file
  
  noaaStations_df = do.call("rbind", strsplit(noaaStations, ","))
  
  noaaStations_df = gsub("[\\\"\\+]", "", noaaStations_df)
  
  noaaStations_df[1, ] = gsub("\\s", "_", noaaStations_df[1, ])
  
  noaaStations_df[1, ] = gsub("\\(", "_", noaaStations_df[1, ])   
  
  noaaStations_df[1, ] = gsub("\\)", "", noaaStations_df[1, ])
  
  colnames(noaaStations_df) = noaaStations_df[1, ]
  
  noaaStations_df = noaaStations_df[-1, ]
  
  noaaStations_df = data.frame(noaaStations_df, stringsAsFactors = FALSE)
  
  noaaStations_df[, 7 : 9] = as.numeric(unlist(noaaStations_df[, 7 : 9]))
  
  return(noaaStations_df)
  
}
