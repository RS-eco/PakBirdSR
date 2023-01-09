#' ---
#' title: "Analysing trend in temperature for Pakistan"
#' author: "RS-eco"
#' ---

#' Load packages
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr); library(magrittr); library(tidyr)
library(lubridate)
library(readr)

# Set working directory
getwd()
setwd("C:/Users/admin/Dropbox/Bird_SR_Pakistan")

# Files are going to be read from your working directory

# Get GADM of Pakistan
pak <- raster::getData(name="GADM", country="PAK", level=1)

########################################

#' EWEMBI Analysis

# Define variable
var <- "pr"

# Set file directory
filedir <- "/media/mbiber/BioScen1point5_Data"

#' Load EWEMBI Temperature data
ewembi_files <- list.files(paste0(filedir, "/ISIMIP2b/EWEMBI/"), 
                           pattern=paste0(var, "_ewembi1_"), full.names = T)
data_pak <- stack(lapply(ewembi_files, function(x){mask(crop(stack(x), pak), pak)}))

#' Convert temperature into degrees
if(var %in% c("tas", "tasmin", "tasmax")){
  data_pak <- raster::calc(data_pak, fun=function(x){x-273.15})
} else if(var == "pr"){
  data_pak <- raster::calc(data_pak, fun=function(x){x*86400})
}

#' Save temperature data to file
writeRaster(data_pak, paste0("env_data/ewembi_", var, "_pak.tif"), 
            datatype="INT2S", options="COMPRESS=LZW", overwrite=T)

#' Read saved temperature file
data_pak <- stack(paste0("env_data/ewembi_", var, "_pak.tif"))

# Set time information of raster
data_pak <- setZ(data_pak, z=seq(as.Date("1979-01-01"), as.Date("2013-12-31"), by=1), 
                 name="date")

# Extract winter temperature
winter_data <- subset(data_pak, which(month(getZ(data_pak)) %in% c(10,11,12)))

# Extract summer tmeanerature
summer_data <- subset(data_pak, which(month(getZ(data_pak)) %in% c(3,4,5)))
rm(data_pak)

# Calculate annual mean (or sum if var = "pr")
if(var == "pr"){
  an_summer_data <- zApply(summer_data, by=year, fun=sum, name='years')
} else{
  an_summer_data <- zApply(summer_data, by=year, fun=mean, name='years')
}
an_summer_data <- as.data.frame(rasterToPoints(an_summer_data))
colnames(an_summer_data) <- c("x", "y", unique(year(getZ(summer_data))))
an_summer_data %<>% gather(year, an_summer_data, -c(x,y))
write.csv(an_summer_data, paste0("env_data/an_summer_", var, "_pak.csv"), row.names=F)

if(var == "pr"){
  an_winter_data <- zApply(winter_data, by=year, fun=sum, name='years')
} else{
  an_winter_data <- zApply(winter_data, by=year, fun=mean, name='years')
}

# Turn data into data.frame
an_winter_data <- as.data.frame(rasterToPoints(an_winter_data))
colnames(an_winter_data) <- c("x", "y", unique(year(getZ(winter_data))))
an_winter_data %<>% gather(year, an_winter_data, -c(x,y))
write.csv(an_winter_data, paste0("env_data/an_winter_", var, "_pak.csv"), row.names=F)

########################################

#' ISIMIP2b GCM Analysis

# Get GADM of Pakistan
pak <- raster::getData(name="GADM", country="PAK", level=1)

# Set file directory
#filedir <- "/media/mbiber/BioScen1point5_Data"
filedir <- "/work/bb0820/ISIMIP"

#Load the listISIMIP function
source("R/functions/listISIMIP.R")

# Define variable
var <- "pr"

# Run through different scenarios (i <- 1:3)
i <- 1
scenario <- c("historical", "rcp26", "rcp60")[i]
startyear <- c(1861, 2006, 2006)[i]
endyear <- c(2005, 2099, 2099)[i]

# Crop files by extent of Pakistan, convert units and save to file
lapply(c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"), function(model){
  if(!file.exists(paste0("env_data/", scenario, "_", model, "_", var, "_pak.nc"))){
    # List files
    files <- listISIMIP(path=filedir, extent="global", scenario=scenario, model=model, 
                        var=var, startyear=startyear, endyear=endyear)
    
    # Read dat and crop by pak extent
    data <- raster::stack(lapply(files, function(x){raster::mask(raster::crop(raster::stack(x), pak), pak)}))
    
    # Convert temperature into degrees
    if(var %in% c("tasmax", "tasmin", "tas")){
      data <- raster::calc(data, fun=function(x){x-273.15})
    } else if(var == "pr"){
      data <- raster::calc(data, fun=function(x){x*86400})
    }
    
    # Save data to file
    raster::writeRaster(data, paste0("env_data/", scenario, "_", model, "_", var, "_pak.tif"), 
                        datatype="INT2S", options="COMPRESS=LZW", overwrite=T)
  }
  return(NULL)
})

#####

# Define variable
var <- "pr"

# Run through different scenarios (i <- 1:3)
i <- 1
scenario <- c("historical", "rcp26", "rcp60")[i]
starttime <- c("1861-01-01", "2006-01-01", "2006-01-01")[i]
endtime <- c("2005-12-31", "2100-12-31", "2100-12-31")[i]

library(magrittr); library(raster); library(lubridate)
lapply(c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"), function(model){
  if(!file.exists(paste0("env_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"))){
  
  if(model == "GFDL-ESM2M" & scenario != "historical"){endtime <- "2099-12-31"}
  if(scenario == "rcp60"){endtime <- "2099-12-31"}
    
  # Read saved temperature file
  data_pak <- stack(paste0("env_data/", scenario, "_", model, "_", var, "_pak.tif"))
  
  # Set time information of raster
  data_pak <- setZ(data_pak, z=seq(as.Date(starttime), as.Date(endtime), by=1), name="date")
  
  # Extract summer tmeanerature/precipitation
  summer_data <- subset(data_pak, which(month(getZ(data_pak)) %in% c(3,4,5)))
  
  # Extract winter temperature/precipitation
  winter_data <- subset(data_pak, which(month(getZ(data_pak)) %in% c(10,11,12)))
  rm(data_pak)
  
  # Calculate annual mean (or sum if var = "pr")
  if(var == "pr"){
    an_summer_data <- zApply(summer_data, by=year, fun=sum, name='years')
  } else{
    an_summer_data <- zApply(summer_data, by=year, fun=mean, name='years')
  }
  an_summer_data <- as.data.frame(rasterToPoints(an_summer_data))
  colnames(an_summer_data) <- c("x", "y", unique(year(getZ(summer_data))))
  write.csv(an_summer_data, paste0("env_data/an_summer_", scenario, "_", model, "_", var, "_pak.csv"), row.names=F)
  
  if(var == "pr"){
    an_winter_data <- zApply(winter_data, by=year, fun=sum, name='years')
  } else{
    an_winter_data <- zApply(winter_data, by=year, fun=mean, name='years')
  }
  
  # Turn tmean into data.frame
  an_winter_data <- as.data.frame(rasterToPoints(an_winter_data))
  colnames(an_winter_data) <- c("x", "y", unique(year(getZ(winter_data))))
  write.csv(an_winter_data, paste0("env_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"), row.names=F)
 }
})

########################################

#' Old Analysis not relevant

# Calculate 30-average (1980 - 2009) 
temp_1980_2010 <- subset(temp_pak, which(getZ(temp_pak) >= as.Date('1980-01-01') & 
                                           getZ(temp_pak) <= as.Date("2009-12-31")))
temp_1995 <- calc(temp_1980_2010, mean, na.rm=TRUE, filename="tmean_pak_1995.tif", 
                  format="GTiff"); rm(temp_1980_2010)

# Plot map of temp_1995
createMap(data=temp_1995, name="tmean", outline=pak,
          filename="tmean_pak_1995_05deg.tiff", 
          width=8, height=9, units="in", dpi=300)

# Calculate mean temperature value over time
#ts_tmean_pak <- as.data.frame(cellStats(temp_pak, stat='mean', na.rm=TRUE))
#colnames(ts_tmean_pak) <- "tmean"
#ts_tmean_pak$date <- getZ(temp_pak)
#readr::write_csv(ts_tmean_pak, "ts_tmean_pak.csv")

#' The above code takes a considerable time to run, so just load the saved file instead

#' Read CSV file
ts_tmean_pak <- read.csv("ts_tmean_pak.csv")

#' Turn dates into date
ts_tmean_pak$date <- as.Date(ts_tmean_pak$date)

# Calculate monthly mean temperature and plot
ts_tmean_pak$month <- month(ts_tmean_pak$date)
ts_tmean_pak$year <- year(ts_tmean_pak$date)

ts_month <- aggregate(tmean ~ month + year, ts_tmean_pak, mean)
ts_month$date <- as_date(paste("15", ts_month$month, ts_month$year), 
                         format="%d %m %Y")
ts_year <- aggregate(tmean ~ year, ts_tmean_pak, mean)
ts_year$date <- as_date(paste("15", "06", ts_year$year), 
                        format="%d %m %Y")

# Plot daily, monthly and annual mean temperature
p1 <- ggplot(data=ts_tmean_pak, aes(x=date, y=tmean)) + 
  geom_line() + geom_smooth(method="gam", se=TRUE) + 
  scale_x_date(date_breaks="2 years", date_labels = "%Y", expand=c(0.01,0)) + 
  labs(x="Date", y="Mean daily temperature (°C)") + theme_bw()  
p2 <- ggplot(data=ts_month, aes(x=date, y=tmean)) + geom_line() + 
  geom_smooth(method="gam", se=TRUE) + 
  scale_x_date(date_breaks="2 years", date_labels = "%Y", expand=c(0.01,0)) + 
  labs(x="Date", y="Mean monthly temperature (°C)") + theme_bw()
p3 <- ggplot(data=ts_year, aes(x=date, y=tmean)) + geom_point() + 
  geom_path() + geom_smooth(method="gam", se=TRUE) + 
  scale_x_date(date_breaks="2 years", date_labels = "%Y", expand=c(0.01,0),
               limits=as.Date(c("1979-01-01", "2013-12-31"))) + 
  scale_y_continuous(breaks=c(19,20,21,22), limits=c(19,22)) + 
  labs(x="Year", y="Mean annual temperature (°C)") + theme_bw()
g <- gridExtra::grid.arrange(p1,p2,p3)
ggsave("tmean_pak.png", g, width=10, height=8, dpi=300)

# Plot just annual mean temperature
ggplot(data=ts_year, aes(x=year, y=tmean)) + geom_point() + 
  geom_path() + geom_smooth(method="gam", se=TRUE) + 
  scale_x_continuous(breaks=seq(1979,2013, by=2)) + 
  scale_y_continuous(breaks=c(19.5,20.0, 20.5, 21.0, 21.5)) + 
  labs(x="Year", y="Mean annual temperature (°C)") + theme_bw()
ggsave("an_tmean_pak.png", width=12, height=4, dpi=300)

########################################

#' Worldclim data

# Get Worldclim v1.4 data for Pakistan
#bio <- merge(getData(name="worldclim", res=0.5, var="bio", download=TRUE, path=filedir, lon=71, lat=32), 
# getData(name="worldclim", res=0.5, var="bio", download=TRUE, path=filedir, lon=71, lat=25))

# Crop by Pakistan boundaries
#bio <- mask(crop(bio, pak), pak)

# Transform temperature data into degree C
#bio[[c(1:11)]] <- bio[[c(1,2,5,6,7,8,9,10,11)]]/10

# Save to file
#writeRaster(bio, "bioclim_pak_05.tif")

# Read from file
bio <- stack("bioclim_pak_05.tif")

# Plot
plot(bio[[1]])
#plot(pak, add=TRUE)

########################################

#' Worldbank country climate

# Alternatively, use rWBclimate
library(rWBclimate)
pak_temp <- get_historical_temp("PAK", "year")
ggplot(pak_temp,aes(x = year,y = data)) + geom_point() + 
  geom_path() + ylab("Mean annual temperature") + theme_bw()

ggplot(pak_temp,aes(x = year,y = data)) + geom_point() + 
  geom_line(data=ts_year, aes(x=year, y=tmean), colour="red") + 
  geom_path() + ylab("Mean annual temperature") + theme_bw()
# Temperatures diverge by 0.5°C
