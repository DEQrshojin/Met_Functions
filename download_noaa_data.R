# ---- GET STATIONS FROM FROM NOAA OCDD ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 16 October, 2018

get_noaa_stations <- function(stns = NULL, strD = NULL, endD = NULL, svDr = NULL) {
  
  library(lubridate)
  
  baseURL = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/"
  
  # DATES ----
  if (is.null(strD)) {strD = as.Date('1900-01-01', '%Y-%m-%d')}
  
  if (is.null(endD)) {endD = as.Date(Sys.time())}
  
  dates = as.Date(c(strD, endD))
  
  strYr = year(strD)
  
  endYr = year(endD)
  
  years4Download = strYr : endYr

  # STATIONS ----
  
  # move to a seperate function
  # replace with an "_": " ", "-", "/", 
  # replace with nothing: ")", ")", ":", ".", "'", 
  # Do something about duplicate names
    
  weatherFileURL = paste0(baseURL, years4Download, "/",
                          stationID[, 1], "-",
                          stationID[, 2], "-",
                          years4Download, ".gz")
  
  # Make a folder for each station ####
  
  weatherFilDest = paste0(saveDir, "corv_muni_ap_1_",
                          stationID[, 1], "-",
                          stationID[, 2], "-",
                          years4Download, ".gz")
  
  for (year in 1 : length(weatherFileURL)) {
    
    download.file(weatherFileURL[year],
                  weatherFilDest[year],
                  "libcurl", quiet = FALSE, mode = "w", cacheOK = TRUE)
    
    # OUTPUT MESSAGE ####
  
  }

}