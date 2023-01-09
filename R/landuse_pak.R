#' ---
#' title: "Analyse the land-use change in Pakistan"
#' author: "RS-eco"
#' ---

#' Load packages
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

# Set working directory
getwd()
# Files are going to be saved and read from your working directory

# Set file directory
filedir <- "/mnt/sda1/Documents/Wissenschaft/Data"

# Get GADM of Pakistan
gadm <- getData(name="GADM", country="PAK", level=1)

########################################

## Landuse histsoc

# Load the readISIMIP function
source("R/functions/readISIMIP.R")

#' **Note:** You also need to have access to all the ISIMIP Data files (multiple GBs), 
#' located at your specified filedir, to get the functions to work. 
#' Also, running this will take quite some time!

# Get and join data
histsoc <- append(readISIMIP(path=filedir, type="landuse", scenario="histsoc", var="totals", 
                             startyear=1861, endyear=2005), 
                  list(readISIMIP(path=filedir, type="landuse", scenario="histsoc", 
                                  var="urbanareas", startyear=1861, endyear=2005)))
names(histsoc)[length(histsoc)] <- "urbanareas"

# Crop and mask data
histsoc <- lapply(histsoc, function(x) mask(crop(x, gadm), gadm))

# Turn into dataframe
histsoc <- lapply(1:length(histsoc), function(x){
  data <- as.data.frame(rasterToPoints(histsoc[[x]])) %>% tidyr::gather(year, value, -c(x,y))
  colnames(data)[4] <- names(histsoc)[x]
  return(data)
})
histsoc <- Reduce(function(...) dplyr::left_join(..., by=c("x","y","year"), all.x=TRUE), histsoc)

# Turn years into numeric
histsoc$year <- as.numeric(sub("X", "", histsoc$year))
colnames(histsoc)

# Save to file
readr::write_csv(histsoc, "data/histsoc_landuse_pak.csv.xz")

########################################

## Landuse 2005soc

# Read and join data
soc2005 <- stack(readISIMIP(path=filedir, type="landuse", scenario="2005soc", var="totals"), 
                 readISIMIP(path=filedir, type="landuse", scenario="2005soc", var="urbanareas"))
names(soc2005) <- c("cropland_irrigated", "cropland_rainfed", "cropland_total", "pastures", "urbanareas")

# Crop and mask data
soc2005 <- mask(crop(soc2005, gadm), gadm)
soc2005 <- as.data.frame(rasterToPoints(soc2005)) 

# Save to file
readr::write_csv(soc2005, "data/2005soc_landuse_pak.csv.xz")

########################################

## Landuse RCP2.6

# Totals & Urban data
rcp26 <- lapply(c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"), function(model){
  # Read data
  data <- append(readISIMIP(path=filedir, type="landuse", scenario="rcp26", 
                            model=model, var="totals", startyear=2006, endyear=2099), 
                 list(readISIMIP(path=filedir, type="landuse", scenario="rcp26", model=model, 
                                 var="urbanareas", startyear=2006, endyear=2099)))
  names(data)[length(data)] <- "urbanareas"
  
  # Crop data
  data <- lapply(data, function(x) mask(crop(x, gadm), gadm))
  # Turn into dataframe
  colname <- names(data)
  data <- lapply(1:length(data), function(x){
    data <- as.data.frame(rasterToPoints(data[[x]])) %>% tidyr::gather(year, value, -c(x,y))
    colnames(data)[4] <- colname[x]
    return(data)
  })
  data <- Reduce(function(...) dplyr::left_join(..., by=c("x","y","year"), all.x=TRUE), data)
  
  # Turn years into numeric
  data$year <- as.numeric(sub("X", "", data$year))
  
  # Add model column
  data$model <- model
  return(data)
})
rcp26 <- do.call("rbind", rcp26)

# Save to file
readr::write_csv(rcp26, "data/rcp26soc_landuse_pak.csv.xz")

########################################

## Landuse RCP6.0

rcp60 <- lapply(c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"), function(model){
  # Read data
  data <- append(readISIMIP(path=filedir, type="landuse", scenario="rcp60", 
                            model=model, var="totals", startyear=2006, endyear=2099), 
                 list(readISIMIP(path=filedir, type="landuse", scenario="rcp60", 
                                 model=model, var="urbanareas", startyear=2006, endyear=2099)))
  names(data)[length(data)] <- "urbanareas"
  
  # Crop data
  data <- lapply(data, function(x) mask(crop(x, gadm), gadm))
  # Turn into dataframe
  colname <- names(data)
  data <- lapply(1:length(data), function(x){
    data <- as.data.frame(rasterToPoints(data[[x]])) %>% tidyr::gather(year, value, -c(x,y))
    colnames(data)[4] <- colname[x]
    return(data)
  })
  data <- Reduce(function(...) dplyr::left_join(..., by=c("x","y","year"), all.x=TRUE), data)
  
  # Turn years into numeric
  data$year <- as.numeric(sub("X", "", data$year))
  
  # Add model column
  data$model <- model
  return(data)
})
rcp60 <- do.call("rbind", rcp60)

# Save to file
readr::write_csv(rcp60, "data/rcp60soc_landuse_pak.csv.xz")

########################################

## Landuse 2100RCP2.6

rcp26_2100 <- lapply(c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"), function(model){
  # Read data
  data <- append(readISIMIP(path=filedir, type="landuse", scenario="rcp26", 
                            model=model, var="totals", startyear=2100, endyear=2299), 
                 list(readISIMIP(path=filedir, type="landuse", scenario="rcp26", 
                                 model=model, var="urbanareas", startyear=2100, endyear=2299)))
  names(data)[length(data)] <- "urbanareas"
  
  # Crop data
  data <- lapply(data, function(x) mask(crop(x, gadm), gadm))
  # Turn into dataframe
  colname <- names(data)
  data <- lapply(1:length(data), function(x){
    data <- as.data.frame(rasterToPoints(data[[x]])) %>% tidyr::gather(year, value, -c(x,y))
    colnames(data)[4] <- colname[x]
    return(data)
  })
  data <- Reduce(function(...) dplyr::left_join(..., by=c("x","y","year"), all.x=TRUE), data)
  
  # Turn years into numeric
  data$year <- as.numeric(sub("X", "", data$year))
  
  # Add model column
  data$model <- model
  return(data)
})
rcp26_2100 <- do.call("rbind", rcp26_2100)

# Save to file
readr::write_csv(rcp26_2100, "data/2100rcp26soc_landuse_pak.csv.xz")
