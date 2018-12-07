# ---- PLOT MEAN ANNUAL TEMPERATURE DATA ----
# Ryan Shojinaga, Water Quality Analyst, Oregon DEQ, Watershed Management Section
# shojinaga.ryan@deq.state.or.us, 503-229-5777
# 11 October 2018
# 
# From PRISM CLIMATE DATA:
# Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K., Taylor, G.H., Curtis, J.,
#   and Pasteris, P.P. 2008.Physiographically sensitive mapping of climatological temperature
#   and precipitation across the conterminous United States. Internation Journal Of Climatology
#   DOI: 10.1002/joc.1688

# Libraries ----
library(raster)
library(rgdal)
library(lubridate)
library(ggplot2)
library(reshape2)

# Read data ----

dataFil1 = "\\\\deqhq1\\tmdl\\TMDL_WR\\MidCoast\\Models\\Dissolved Oxygen\\"
dataFil2 = "Middle_Siletz_River_1710020405\\001_data\\met_data\\"
tmeanFil = "siletz_HSPF_tmean_1981_2018.csv"
tmean = read.csv(paste0(dataFil1, dataFil2, tmeanFil))

# Rename columns to remove X

names(tmean) = c("DATE", as.character(1 : 17))

# Create a "Year" column for annual statistics

tmean$Year = year(tmean$DATE)

# Reshape the data into a long data frame

tmeanMelted = melt(tmean, id.vars = c("DATE", "Year"))

# Recast to calculate annual means

tmeanAnnual = dcast(tmeanMelted, Year ~ variable, fun.aggregate = mean, value.var = "value")

# Melt back to long data frame of annual values

tmeanAnnual = melt(tmeanAnnual, id.vars = "Year")

siletzTMeanAnnual <- ggplot() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    geom_boxplot(data = tmeanAnnual, aes(x = variable, y = value, group = variable),
                 outlier.shape = 1, outlier.size = 1) + ylab("temperature (oC)") +
    xlab("HSPF Basin Number") + ggtitle("PRISM Mean Annual Temperatures by HSPF Basin")

ggsave(paste0(dataFil1, dataFil2, "HSPF_basins_mean_annual_temp.png"), siletzTMeanAnnual,
       width = 6.5, height = 5, units = "in", dpi = 300)
