#' ---
#' title: "Analysing the Bird SR of Pakistan"
#' author: "RS-eco"
#' ---

#+echo=F
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

#' Load packages
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)
library(magrittr)

#' Set working directory
getwd()
#setwd("C:/Users/admin/Dropbox/Bird_SR_Pakistan")
#' Files are going to be read from and saved to your working directory

#Specify path of file directory
filedir <- "/mnt/sda1/Documents/Wissenschaft/Data/"
#filedir <- "/home/mbiber/"

# Specify country
#country <- "PAK"

#' Get GADM of Pakistan
#pak <- raster::getData(name="GADM", country=country, level=1, path=paste0(filedir, "/GADM"))
pak <- getData(name="GADM", country="PAK", level=1)

# Read species list
species <- readxl::read_xlsx("Birds_Pakistan_list.xlsx")

########################################

#' Create SR data from Hassan Shapefiles

#Load the rasterizeRange function
#+ eval=F
library(rasterSp)

# Rasterize Bird species data from Imran
rasterizeRange(dsn=list.files("All_Hassan_Files", pattern=".shp", full.names=TRUE), 
              resolution=0.5, save=TRUE, extent=c(60, 78, 23, 37), name_split=c(2,3),
              filepath="gridded_Hassan/")

# Rename gridded files
bird_list <- lapply(list.files("gridded_Hassan", pattern=".tif", full.names=TRUE), raster)

# Some bird files have wrong crs
bird_crs <- lapply(bird_list, projection)
wrong_crs <- which(bird_crs == unique(bird_crs)[[2]])
bird_list[wrong_crs] <- lapply(bird_list[wrong_crs], function(x){
  projectRaster(x, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
})

# Stack bird files
bird_stack <- stack(bird_list)

# Raster files also have different extents
bird_ext <- lapply(bird_list, extent)
bird_ext <- lapply(bird_ext, as.vector)

# Calculate Species richness of detailed species ranges
bird_sr <- calc(bird_stack, fun=sum, na.rm=TRUE, 
                filename="bird_sr_pak_hassan_05deg.tif", overwrite=TRUE)

#' Plot Hassan SR data

# Load Hassan SR data
bird_sr <- raster("bird_sr_pak_hassan_05deg.tif")
bird_sr <- crop(mask(bird_sr, pak), pak)

# Plot species richness of Hassan Files
bird_sr_df <- as.data.frame(rasterToPoints(bird_sr))
colnames(bird_sr_df) <- c("x", "y", "bird_sr")
#ggplot() + geom_raster(data=bird_sr_df, aes(x=x,y=y,fill=bird_sr)) + 
#  geom_polygon(data=pak, aes(x=long, y=lat, group=group), colour="black", 
#               fill="transparent") + 
#  scale_fill_gradientn(na.value="transparent", colours=rev(heat.colors(255)))
#ggsave("bird_sr_pak_hassan.png")

# Load ggmap2 function for plotting
library(ggmap2)

# Create map
ggmap2(data=bird_sr, name="SR", outline=pak, filename="SR_Birds_Hassen_PAK_05deg.tiff", 
       save=T, width=8, height=9, units="in", dpi=100)

####################################

#' ## Rasterize BirdLife species data

#' Bird data was obtained from BirdLife. 
#' Currently we use the 2014 version, which comes in form of individual shapefiles for each species. 
#' **Note:** The `rasterizeRange` function can also handle a list of shapefiles.

#' Current bird data comes from Christian, obtained from BirdLife International (All.7z), likely 2014 data. 
#' File from BirdLife also came through (BOTW.7z), version 2018.

#+ eval=F
r_birds_breeding <- rasterizeRange(
  dsn=list.files(paste0(filedir, "/BirdLife/"), pattern=".shp", 
                 full.names=TRUE), resolution=0.5, save=TRUE, seasonal=c(1,2),
  origin=1, presence=c(1,2), path=paste0(filedir, "/SpeciesData/"))

r_birds_wintering <- rasterizeRange(
  dsn=list.files(paste0(filedir, "/BirdLife/"), pattern=".shp", 
                 full.names=TRUE), resolution=0.5, save=TRUE, seasonal=c(1,3),
  origin=1, presence=c(1,2), path=paste0(filedir, "/WinteringBirds/"))

#' BirdLife as well as IUCN shapefiles provide information on a couple of parameters 
#' (e.g. seasonal, origin and presence). 
#' These three parameters are implemented in the `rasterizeRange` function, 
#' which then selects only a specific subset of Polygons for each species. 
#' Infos on the different parameters, can be found here: http://datazone.birdlife.org/species/spcdistPOS.

#' ## Species Data

#+ eval=F
list.files(paste0(filedir, "/SpeciesData"))
list.files(paste0(filedir, "/WinteringBirds"))

#' Plot breeding and wintering range of Accipiter gentilis

# Load range
breeding <- stack(paste0(filedir, "/SpeciesData/Accipiter_gentilis.tif"))
wintering <- stack(paste0(filedir, "/WinteringBirds/Accipiter_gentilis_0.5.tif"))

# Plot
#+fig.width=8, fig.height=6
par(mfrow=c(1,2))
plot(breeding)
plot(wintering)

#' ### Save terrestrial species data to csv file

#' **Note:** The `speciesData` function is internally using 75 % of the number of cores for 
#' parallel computing.

#' Load the speciesData function
#+ eval=F
library(rasterSp)

#+ eval=F
data(ter_birds)
speciesData(species_names=unique(ter_birds$SCINAME), path=paste0(filedir, "/SpeciesData/"), 
            filename="data/ter_birds_dist.csv.xz")

speciesData(species_names=unique(ter_birds$SCINAME), path=paste0(filedir, "/WinteringBirds/"), 
            filename="data/winter_birds_dist.csv.xz")

#' ## Identify bird species that occur in Pakistan

# Read bird data
breeding_birds <- readr::read_csv("ter_birds_dist.csv.xz")
wintering_birds <- readr::read_csv("winter_birds_dist.csv.xz")

#Rasterize country shapefile
data(landseamask_generic, package="rISIMIP")
countries <- raster::rasterize(pak, landseamask_generic)
countries <- data.frame(raster::rasterToPoints(countries))
colnames(countries) <- c("x", "y", "country")

# Identify species that only occur in Pakistan
breeding_birds %<>% right_join(countries)
wintering_birds %<>% right_join(countries)

## Calculate SR per Group

# Create presence column
breeding_birds$presence <- 1
wintering_birds$presence <- 1

# Calulate terrestrial bird SR
breeding_birds %<>% group_by(x, y) %>% summarise(sum = sum(presence))
wintering_birds %<>% group_by(x, y) %>% summarise(sum = sum(presence))

## Plot global map of SR per taxa
ggmap2(data=breeding_birds, name="SR", outline=pak, filename="breeding_birds_sr_PAK.tiff", 
       save=T, width=8, height=9, units="in", dpi=100)
ggmap2(data=wintering_birds, name="SR", outline=pak, filename="wintering_birds_sr_PAK.tiff", 
       save=T, width=8, height=9, units="in", dpi=100)

## Save data to file
write.csv(breeding_birds, "breeding_bird_sr_pak.csv", row.names = F)
write.csv(wintering_birds, "wintering_bird_sr_pak.csv", row.names = F)
