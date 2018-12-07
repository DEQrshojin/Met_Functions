# DIRECT BEAM SOLAR TO PAR ----
# Ryan Shojinaga, Oregon DEQ, Shojinaga.Ryan@DEQ.State.OR.US, 503-229-5777
# 5 September 2018

# Creates a timeseries of photosynthically active radiation (PAR) based on clear-sky solar
# radiation. Requires the input of: start/end time, time step, location (lat/lon),
# timezone and atmospheric conditions (only constant pressure at the moment)
# Returns a data.frame of Date/Time, direct beam solar and PAR

# Improvements:
# 1) Exception if no end data is specified (NULLs)
# 2) Pass a vector of atmospheric pressure, or go get P_atm from nearby met station if possible.

solar2par <- function(strDate = NULL,     # as.POSIXct for the moment, coerce in the future
                      endDate = NULL,     # as.POSIXct for the moment, coerce in the future
                      timeStep = NULL,    # in hours
                      lat = NULL,         # in decimal degrees
                      lon = NULL,         # in decimal degrees
                      tz = NULL,          # as an integer, PST = -8
                      pres = NULL) {      # static pressure relative to MSL = 1013 millibars

# LIBRARIES ----
library(lubridate)

# INITIALIZE INTERNAL VARIABLES ----
oz = 0.3                # Optional - ozone thickness in centimeters; default = 0.3 cm
h2o = 1.5               # Optional - water vapor thickness in centimeters; default = 1.5 cm
aod500 = 0.1            # Optional - aerosol optical depth at 500nm; default = 0.1 nm
aod380 = 0.1            # Optional - aerosol optical depth at 380nm; default = 0.1 nm
taua = 0.2758 * aod380 + 0.35 * aod500  # Broadband aerosol optical depth
options(stringsAsFactors = FALSE)

# CONSTRUCT TIMESERIES ----
timeStep = timeStep * 3600 # Convert timeStep from hours to seconds

# When I pass the timeStep as a fraction (minutes/60), what is being passed minutes/60 or the float
x = data.frame(seq(strDate, endDate, timeStep)) 
names(x) = "ts"
x$hr = hour(x$ts) + minute(x$ts) / 60 # Hour of day expressed as a decimal 
x$doy = as.numeric(strftime(x$ts, format = "%j"))  # Day of year

# CALCULATIONS ----
# These equations come from the Bird Clear-sky model:
# Bird, R.E. and Hulstrom, R.L 1991. A simplified clear sky mModel for direct and diffuse
#    insolation on horizontal surfaces, SERI Technical Report SERI/TR-642-761, Feb 1991.
#    Solar Energy Research Institute, Golden, CO.

x$etr <- 1367 * (1.00011 + 0.034221 * cos(2 * pi * (x$doy - 1)/ 365) +
    0.00128 * sin(2 * pi * (x$doy - 1) / 365) + 0.000719 *
    cos(2 * (2 * pi * (x$doy - 1) / 365)) + 0.000077 *
    sin(2 * (2 * pi * (x$doy - 1)/365))) # Extraterrestrial direct beam intensity;
x$dAng <- 6.283185 * (x$doy - 1) / 365 # Day angle for position of earth around sun
x$dec <- (180 / pi) * (0.006918 - 0.399912 * cos(x$dAng) + 0.070257 *
    sin(x$dAng) - 0.006758 * cos(2 * x$dAng) +
    0.000907 * sin(2 * x$dAng) - 0.002697 *
    cos(3 * x$dAng) + 0.00148 * sin(3 * x$dAng)) # solar decliniation
x$eqt <- 229.18 * (0.0000075 + 0.001868 * cos(x$dAng) - 0.032077 * sin(x$dAng) -
    0.014615 * cos(2 * x$dAng) - 0.040849 * sin(2 * x$dAng)) # equation of sun time
x$hAng <- 15 * (x$hr - 12.5) + (lon) - (tz) * 15 + x$eqt / 4 # hour angle of the sun to horizon
x$zAng <- (180 / pi) * acos(cos(x$dec / (180 / pi)) * cos(lat / (180 / pi)) *
    cos(x$hAng / (180 / pi)) + sin(x$dec / (180 / pi)) *
    sin(lat / (180 / pi))) # Zenith angle
x$mAir <- ifelse(x$zAng < 89,
                 1 / (cos(x$zAng / (180 / pi)) + 0.15 / (93.885 - x$zAng)^1.25),
                 0) # Geometrical air mass
x$tRay <- ifelse(x$mAir > 0,
                 exp(-0.0903 * (pres * x$mAir / 1013) ^ 0.84 *
                 (1 + pres * x$mAir / 1013 - (pres * x$mAir / 1013)^1.01)),
                 0)
x$tOz <- ifelse(x$mAir > 0,
                1 - 0.1611 * (oz * x$mAir) * (1 + 139.48 * (oz * x$mAir))^-0.3034 -
                0.002715 * (oz * x$mAir) / (1 + 0.044*(oz * x$mAir) + 0.0003 *
                (oz * x$mAir)^2),
                0)
x$tGas <- ifelse(x$mAir > 0,
                 exp(-0.0127 * (x$mAir * pres / 1013)^0.26),
                 0)
x$tH2O <- ifelse(x$mAir > 0,
                 1 - 2.4959 * x$mAir * h2o /
                 ((1 + 79.034 * h2o * x$mAir)^0.6828 + 6.385 * h2o * x$mAir),
                 0)
x$tAer <- ifelse(x$mAir > 0,
                 exp(-(taua^0.873) * (1 + taua - taua^0.7088) * x$mAir^0.9108),
                 0)
# Calculation of direct beam solar Units in W/m2
x$solar <- ifelse(x$mAir > 0,
                  0.9662 * x$etr * x$tRay * x$tOz * x$tGas * x$tH2O * x$tAer,
                  0)
                                                                                               
# Calculate PAR from direct solar ----
# Sager, J.C. and McFarlane, J.C. 1997. Plant Growth Chamber Handbook, Chapter 1, Radiation.  
#    Iowa State University North Central Regional Research Publication No. 340. 
# Conversion factor from W/m2 to umol/m2/s = 4.57; only 45% tho in the 400-700 nm wavelength range.

x$PAR <- x$solar * 2.1 

x = x[, -c(2 : 15)] # Remove the interim calculations

return(x)

}
