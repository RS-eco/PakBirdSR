#' ---
#' title: "Analysing trend in population for Pakistan"
#' author: "RS-eco"
#' ---

#' Load packages
library(raster)
library(dplyr)
library(magrittr)
library(readr)

# Set working directory
getwd()
# Files are going to be saved and read from your working directory

# Set file directory
filedir <- "/mnt/sda1/Documents/Wissenschaft/Data"

# Get GADM of Pakistan
gadm <- getData(name="GADM", country="PAK", level=1)

########################################

## Population histsoc

# Load the readISIMIP function
source("R/functions/readISIMIP.R")

#' **Note:** You also need to have access to all the ISIMIP Data files (multiple GBs), 
#' located at your specified filedir, to get the functions to work. 
#' Also, running this will take quite some time!

# Get data
histsoc <- readISIMIP(path=filedir, type="population", scenario="histsoc", var="totals", 
                             startyear=1861, endyear=2005)

# Crop and mask data & turn into dataframe
histsoc %<>% crop(gadm) %>% mask(gadm) %>% rasterToPoints %>% as.data.frame %>% tidyr::gather(year, value, -c(x,y))

# Turn years into numeric
histsoc$year <- as.numeric(sub("X", "", histsoc$year))

# Save to file
readr::write_csv(histsoc, "data/histsoc_population_pak.csv.xz")

########################################

## Population 2005soc

# Read data
soc2005 <- readISIMIP(path=filedir, type="population", scenario="2005soc", var="totals")

# Crop and mask data
soc2005 <- mask(crop(soc2005, gadm), gadm)
soc2005 <- as.data.frame(rasterToPoints(soc2005)) 
colnames(soc2005) <- c("x","y", "population")
soc2005$year <- 2005

# Save to file
readr::write_csv(soc2005, "data/2005soc_population_pak.csv.xz")

########################################

#' ## Population ssp2soc

# Read data
ssp2soc <- readISIMIP(path=filedir, type="population", scenario="ssp2soc", startyear=2006, endyear=2099)

# Crop data & turn into dataframe
ssp2soc %<>% crop(gadm)%>% mask(gadm) %>% rasterToPoints %>% as.data.frame %>% tidyr::gather(year, value, -c(x,y))

# Turn years into numeric
ssp2soc$year <- as.numeric(sub("X", "", ssp2soc$year))

# Save to file
readr::write_csv(ssp2soc, "data/ssp2soc_population_pak.csv.xz")

########################################

#' ## Population 2100soc

#Read data
soc_2100 <- readISIMIP(path=filedir, type="population", scenario="2100soc", startyear=2100, endyear=2299)
  
# Crop data & turn into dataframe
soc_2100 %<>% crop(gadm) %>% mask(gadm) %>% as.data.frame(rasterToPoints(data)) %>% tidyr::gather(year, value, -c(x,y))

# Turn years into numeric
data$year <- as.numeric(sub("X", "", data$year))

# Save to file
readr::write_csv(soc_2100, "data/2100soc_population_pak.csv.xz")
