# This function calculates photosynthically active radiation (PAR) based on clear-sky solar radiation.
# The assumption of this function is that PAR is directly proportional to direct beam radiation calculated
# based on the location, time of year and other specified environmental parameters.
# This function requires the input of location, timezone and atmospheric conditions, as well as a time vector
# and returns a time series of PAR based on clear-sky conditions.
# Future updates to this should include determining the time zone and atmospheric conditions from the lat/lon
# Also it would be good to build into this other cloud cover and effective shading, both vector parameters
# passed into the function
#
# Ryan Shojinaga, Water Quality Analyst
# Watershed Management Section, Oregon DEQ
# Shojinaga.Ryan@DEQ.State.OR.US, 503-229-5777
# 5 September 2018

solar2par <- function(x) {

# Load libraries ---------------------------------------------------------------------------------------------------------------------
library(lubridate)

# Specified variables ----------------------------------------------------------------------------------------------------------------
lat = 45                                # Required - latitude in decimal degrees
lon = -124                              # Required - longitude in decimal degrees
tm.zn = -8                              # Required - time zone in hours relative to Greenwich (pst = -8)
pres = 1013                             # Required - static atmospheric pressure relative to MSL = 1013 millibars
oz = 0.3                                # Optional - ozone thickness in centimeters; default = 0.3 cm
h2o = 1.5                               # Optional - water vapor thickness in centimeters; default = 1.5 cm
aod500 = 0.1                            # Optional - aerosol optical depth at 500nm in nanometers; default = 0.1 nm
aod380 = 0.1                            # Optional - aerosol optical depth at 380nm in nanometers; default = 0.1 nm
taua = 0.2758 * aod380 + 0.35 * aod500  # Not a parameter - Broadband aerosol optical depth aggregate of aod500 and aod 380
# ____________________________________________________________________________________________________________________________________

# Read the input file, clean it up and process the times and dates into julian day and decimal hour ----------------------------------
# x = read.csv(file.choose())
# x$DATE.TIME = as.POSIXct(x$DATE.TIME, "%m/%d/%Y %H:%M", tz = "America/Los_Angeles")
# x <- x[complete.cases(x[ , 5:8]), ] # Remove NAs
# x <- x[, -c(2, 4, 6, 9)]
x$hr = hour(x$ts) + minute(x$ts) / 60 # Hour of day expressed as a decimal 
x$doy = as.numeric(strftime(x$ts, format = "%j"))  # Day of year

# Begin intermediate calculations-----------------------------------------------------------------------------------------------------
# These equations come from the Bird Clear-sky model:
# Bird, R.E. and Hulstrom, R.L 1991. A simplified clear sky mModel for direct and diffuse insolation on horizontal surfaces,
#    SERI Technical Report SERI/TR-642-761, Feb 1991. Solar Energy Research Institute, Golden, CO.

x$etr <- 1367 * (1.00011 + 0.034221 * cos(2 * pi * (x$doy - 1)/ 365) +
                0.00128 * sin(2 * pi * (x$doy - 1) / 365) + 0.000719 *
                cos(2 * (2 * pi * (x$doy - 1) / 365)) + 0.000077 *
                sin(2 * (2 * pi * (x$doy - 1)/365))) # Extraterrestrial direct beam intensity;
x$dang <- 6.283185 * (x$doy - 1) / 365 # Day angle representing the position of the earth around the sun
x$dec <- (180 / pi) * (0.006918 - 0.399912 * cos(x$dang) + 0.070257 *
                      sin(x$dang) - 0.006758 * cos(2 * x$dang) +
                      0.000907 * sin(2 * x$dang) - 0.002697 *
                      cos(3 * x$dang) + 0.00148 * sin(3 * x$dang)) # solar decliniation
x$eqt <- 229.18 * (0.0000075 + 0.001868 * cos(x$dang) - 0.032077 * sin(x$dang) -
                  0.014615 * cos(2 * x$dang) - 0.040849 * sin(2 * x$dang)) # equation of time for the sun
x$hang <- 15 * (x$hr - 12.5) + (lon) - (tm.zn) * 15 + x$eqt / 4 # hour angle of the sun with respect to 0 = azimuth of 180 degrees
x$zang <- (180 / pi) * acos(cos(x$dec / (180 / pi)) * cos(lat / (180 / pi)) * cos(x$hang / (180 / pi)) +
                            sin(x$dec / (180 / pi)) * sin(lat / (180 / pi))) # Zenith angle
x$m.air <- ifelse(x$zang < 89, 1 / (cos(x$zang / (180 / pi)) + 0.15 / (93.885 - x$zang)^1.25), 0) # Geometrical air mass
x$t.ray <- ifelse(x$m.air > 0, exp(-0.0903 * (pres * x$m.air / 1013) ^ 0.84*(1 + pres * x$m.air / 1013 - (pres * x$m.air / 1013)^1.01)), 0)
x$t.oz <- ifelse(x$m.air > 0, 1 - 0.1611 * (oz * x$m.air) * (1 + 139.48 * (oz * x$m.air))^-0.3034 - 0.002715 * (oz * x$m.air) /
               (1 + 0.044*(oz * x$m.air) + 0.0003 * (oz * x$m.air)^2), 0)
x$t.gas <- ifelse(x$m.air > 0, exp(-0.0127 * (x$m.air * pres / 1013)^0.26), 0)
x$t.h2o <- ifelse(x$m.air > 0, 1 - 2.4959 * x$m.air * h2o / ((1 + 79.034 * h2o * x$m.air)^0.6828 + 6.385 * h2o * x$m.air), 0)
x$t.aer <- ifelse(x$m.air > 0, exp(-(taua^0.873) * (1 + taua - taua^0.7088) * x$m.air^0.9108), 0)
x$solar <- ifelse(x$m.air > 0, 0.9662 * x$etr * x$t.ray * x$t.oz * x$t.gas * x$t.h2o * x$t.aer, 0) # Direct beam solar Units are W/m2
                                                                                               
# Calculate PAR from direct solar-----------------------------------------------------------------------------------------------------
# Sager, J.C. and McFarlane, J.C. 1997. Plant Growth Chamber Handbook, Chapter 1, Radiation.  
#    Iowa State University North Central Regional Research Publication No. 340. 
# Calculates PAR based on assumption of conversion from W/m2 to umol/m2/s at a factor of 4.57
# Only 45 percent of which is within the 400-700 nm wavelength range.
# Units are in photons <- umol/m2/s

x$PAR <- x$solar * 2.1 

return(x$PAR)

}
