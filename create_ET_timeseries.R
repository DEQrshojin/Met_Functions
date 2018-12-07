# CREATE POTENTIAL EVAPOTRANSPIRATION TIMESERIES ----
# Utilizes the Penman-Monteith Method
# Ryan Shojinaga, Water Quality Analyst
# Watershed Management Section, Oregon DEQ
# Shojinaga.Ryan@DEQ.State.OR.US, 503-229-5777
# 22 October 2018

# Libraries ----
library(lubridate)
library(zoo)
library(raster)
library(Evapotranspiration)


# Import of the the subset met data ----

dir1 = "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle_Siletz_River_1710020405/"
dir2 = "001_data/met_data/"
dir3 = "005_reporting/figures/modeling_memo/"

metData = readRDS(paste0(dir1, dir2, "full_met_for_PET.RData"))

# Import and amend constants for specific locations ----

data("constants")

# Import basin specific information from shapefile -- need to change: z, lat

shpFilDir = "C:/Users/rshojin/Desktop/001_projects/mid_coast_DO_tmdls/GIS/001_data/shape/"

shpFil = "catchment_centroids_HSPF_z.shp"

basinCent = shapefile(paste0(shpFilDir, shpFil))

siletzKs = list()

for (basin in 1 : length(metData))
{
    
    siletzKs[[basin]] = constants # assign global constants
    
    siletzKs[[basin]][["lat"]] = basinCent@data[basin, 2]  # apply latitude
    
    siletzKs[[basin]][["lat_rad"]] = siletzKs[[basin]][["lat"]] * pi / (180)
    
    siletzKs[[basin]][["Elev"]] = basinCent@data[basin, 5] # apply elevation
    
}

# Calculate PET for each basin

siletzPET = list()

petYearTS = 1985 : 2017

petDayTS = as.vector(metData[[1]][["Date.daily"]])

for (basin in 1 : length(metData))
{
    
    siletzPET[[basin]] = ET.PenmanMonteith(metData[[basin]], siletzKs[[basin]],
                                           ts = "daily", solar = "sunshine hours",
                                           wind = "yes", crop = "tall", message = "yes")

    petYearTS = cbind(petYearTS,
                      as.vector(siletzPET[[basin]][["ET.Annual"]][1 : length(siletzPET[[basin]][["ET.Annual"]]) - 1]))
    
    petDayTS = cbind(petDayTS,
                     as.vector(siletzPET[[basin]][["ET.Daily"]]))
    
}

petDayTS = data.frame(petDayTS, stringsAsFactors = FALSE)

colnames(petDayTS) = c("DAY", as.character(seq(1 : 17)))

petDayTS$DAY = as.Date(petDayTS$DAY)

write.csv(petDayTS, paste0(dir1, dir2, "siletz_HSPF_PET.csv"), row.names = FALSE)

# Boxplot of mean annual PET by basin

# colnames(petYearTS) = c("YR", as.character(seq(1 : 17)))
# 
# petTS = data.frame(petYearTS, stringsAsFactors = FALSE)
# 
# petSummaryByYear = melt(petYearTS, id.vars = "YR", variable.name = "Basin_No",
#                         value.name = "annPET")
# 
# siletzAnnualPET <- ggplot() + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
#     geom_boxplot(data = petSummaryByYear,
#                  aes(x = Basin_No, y = annPET, group = Basin_No),
#                  outlier.shape = 1, outlier.size = 1) +
#     ylab("Potential Evapotranspiration") + xlab("HSPF Basin Number") +
#     ggtitle("Annual PET by HSPF Basin")
# 
# pngSaveDir = paste0(dir1, dir3)
# 
# ggsave(paste0(pngSaveDir, "Annual_PET_by_basin.png"), siletzAnnualPET,
#        width = 6.5, height = 5, units = "in", dpi = 300)

