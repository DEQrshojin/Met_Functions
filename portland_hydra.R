

# LIBRARIES ----
library(XML)

# GET RAIN GAGE FILE NAMES ----
hydra.site <- "https://or.water.usgs.gov/non-usgs/bes/"
full.text <- readLines(hydra.site) # Complete read of the website
gage.lines <- full.text[grep("\\.rain", full.text)] # Extracts the lines that have the rain gage files
quote.loc <- gregexpr("\"", gage.lines) # Return a list of 
gages <- list("gage.file" = character(),
              "gage.name" = character(),
              "gage.site" = character(),
              "gage.data" = list(vector()))
tmp <- NULL
for (i in 1 : length(quote.loc))
{
    gages[["gage.file"]][i] <- substr(gage.lines[i], quote.loc[[i]][1] + 1, quote.loc[[i]][2] - 1)
    gages[["gage.name"]][i] <- substr(gage.lines[i], quote.loc[[i]][1] + 1, quote.loc[[i]][2] - 6)
    gages[["gage.site"]][i] <- paste0("https://or.water.usgs.gov/non-usgs/bes/", gages[["gage.file"]][i])
    tmp[i] <- readLines(gages[["gage.site"]][i])
    # gages[["gage.data"]][i] <- readLines(gages[["gage.site"]][i])
}


