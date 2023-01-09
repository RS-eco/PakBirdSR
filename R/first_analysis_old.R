#' ---
#' title: "Bird species richness in Pakistan"
#' author: "RS-eco"
#' ---

# Load packages
library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)

# Set file directory
#filedir <- "E:/Data"
filedir <- "/home/mabi/Documents/Wissenschaft/Data"

# Get GADM of Pakistan
pak <- getData(name="GADM", country="PAK", level=1, path=paste0(filedir, "/GADM"))

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

# Load EWEMBI Temperature data
#ewembi_files <- list.files(paste0(filedir, "/ISIMIP2b/InputData/EWEMBI/"), 
#                           pattern="tas_ewembi1_", full.names = T)
#temp_pak <- stack(lapply(ewembi_files, function(x){
#  temp <- stack(x)
#  mask(crop(temp, pak), pak)
#  }))

# Convert temperature into degrees
#temp_pak <- raster::calc(temp_pak, fun=function(x){x-273.15})

# Save temperature data to file
#writeRaster(temp_pak, "ewembi_tmean_pak.tif")
temp_pak <- stack("ewembi_tmean_pak.tif")

# Set time information of raster
temp_pak <- setZ(temp_pak, z=seq(as.Date("1979-01-01"), as.Date("2013-12-31"), by=1), 
                 name="date")

# Calculate 30-average (1980 - 2009) 
temp_1980_2010 <- subset(temp_pak, which(getZ(temp_pak) >= as.Date('1980-01-01') & 
                                           getZ(temp_pak) <= as.Date("2009-12-31")))
temp_1995 <- calc(temp_1980_2010, mean, na.rm=TRUE, filename="tmean_pak_1995.tif", format="GTiff"); rm(temp_1980_2010)

# Plot map of temp_1995
library(ggmap2)
createMap(data=temp_1995, name="tmean", outline=pak,
          filename="tmean_pak_1995_05deg.tiff", 
          width=8, height=9, units="in", dpi=300)

# Calculate mean temperature value over time
#ts_tmean_pak <- as.data.frame(cellStats(temp_pak, stat='mean', na.rm=TRUE))
#colnames(ts_tmean_pak) <- "tmean"
#ts_tmean_pak$date <- getZ(temp_pak)
#readr::write_csv(ts_tmean_pak, "ts_tmean_pak.csv")
ts_tmean_pak <- read.csv("ts_tmean_pak.csv")
ts_tmean_pak$date <- as.Date(ts_tmean_pak$date)

# Calculate monthly mean temperature and plot
library(lubridate)
ts_tmean_pak$month <- month(ts_tmean_pak$date)
ts_tmean_pak$year <- year(ts_tmean_pak$date)

ts_month <- aggregate(tmean ~ month + year, ts_tmean_pak, mean)
ts_month$date <- as_date(paste("15", ts_month$month, ts_month$year), format="%d %m %Y")
ts_year <- aggregate(tmean ~ year, ts_tmean_pak, mean)
ts_year$date <- as_date(paste("15", "06", ts_year$year), format="%d %m %Y")

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
library(gridExtra)
g <- grid.arrange(p1,p2,p3)
ggsave("tmean_pak.png", g, width=10, height=8, dpi=300)

ggplot(data=ts_year, aes(x=year, y=tmean)) + geom_point() + 
  geom_path() + geom_smooth(method="gam", se=TRUE) + 
  scale_x_continuous(breaks=seq(1979,2013, by=2)) + 
  scale_y_continuous(breaks=c(19.5,20.0, 20.5, 21.0, 21.5)) + 
  labs(x="Year", y="Mean annual temperature (°C)") + theme_bw()
ggsave("an_tmean_pak.png", width=12, height=4, dpi=300)

# Alternatively, use rWBclimate
library(rWBclimate)
pak_temp <- get_historical_temp("PAK", "year")
ggplot(pak_temp,aes(x = year,y = data)) + geom_point() + 
  geom_path() + ylab("Mean annual temperature") + theme_bw()

ggplot(pak_temp,aes(x = year,y = data)) + geom_point() + 
  geom_line(data=ts_year, aes(x=year, y=tmean), colour="red") + 
  geom_path() + ylab("Mean annual temperature") + theme_bw()
# Temperatures diverge by 0.5°C

# Get Hydrosheds Lake Data
#hydro_lakes <- rgdal::readOGR(paste0(filedir, "/Hydrosheds/HydroLAKES_polys_v10.shp"))
#lakes_pak <- hydro_lakes[hydro_lakes$Country == "Pakistan",]

# Save to file
#writeOGR(lakes_pak, dsn=paste0(filedir, "/lakes_pak.shp"), 
#         layer="lakes_pak", driver="ESRI Shapefile")
lakes_pak <- readOGR(paste0(filedir, "/lakes_pak.shp"))

# Load Pakistan Water Data obtained from the Diva-GIS Website (http://www.diva-gis.org/gdata)
water_areas_pak <- rgdal::readOGR(paste0(filedir, "/PAK_water_areas_dcw.shp"))
water_lines_pak <- rgdal::readOGR(paste0(filedir, "/PAK_water_lines_dcw.shp"))
# Water lines is similar to the hydrosheds data

# Read bird species richness
bird_data_05deg <- read.csv("bird_data_05deg.csv")

# Create presence column
bird_data_05deg$presence <- 1

# Calulate terrestrial bird SR
library(dplyr)
sr_ter_birds <- bird_data_05deg %>% group_by(x,y) %>%
  summarise(sum = sum(presence))

# Turn bird data to raster
coordinates(sr_ter_birds) <- ~x+y
projection(sr_ter_birds) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
gridded(sr_ter_birds) <- TRUE
sr_ter_birds <- raster(sr_ter_birds)

# Crop bird species richness by pakistan outline
bird_sr_pak <- mask(crop(sr_ter_birds, pak), pak)

#writeRaster(bird_sr_pak, "bird_sr_pak_05deg.tif")
bird_sr_pak <- raster("bird_sr_pak_05deg.tif")

#Create plot of Pakistan bird richness
library(ggmap2)
createMap(data=bird_sr_pak, name="SR", outline=pak,
          filename="SR_Birds_PKA_05deg.tiff", 
          width=8, height=9, units="in", dpi=100)

# Read species list
library(readxl)
species <- read_xlsx("Birds_Pakistan_list.xlsx")

# Get path of PAK bird species
species_all <- list.files(paste0(filedir, "/BirdLife/"), pattern=".shp", full.names=TRUE)
species_names_all <- lapply(species_all, function(x) 
  paste(strsplit(basename(x), split="_")[[1]][1], 
        strsplit(basename(x), split="_")[[1]][2], sep="_"))
species_pak <- species_all[species_names_all %in% species$species]

# Rasterize old Birdlife species data for Pakistan species, 
# but without removing non-breeding ranges
library(rasterSp)
rasterizeRange(dsn=species_pak, resolution=0.5, save=TRUE, 
              origin=1, presence=c(1,2), filepath=paste0(filedir, "/gridded_BL_noseason/"))

# Create Species Richness Map without removing non-breeding ranges
bird_data_pak <- speciesData(species_names=species_pak, 
                             path=paste0(filedir, "/gridded_BL_noseason/"), 
                             filename="birds_pak_noseason_05deg.csv")

# Turn ter bird data to dataframe
bird_data_pak <- as.data.frame(bird_data_pak)

# Create presence column
bird_data_pak$presence <- 1

# Calulate bird SR
library(dplyr)
sr_bird_pak <- bird_data_pak %>% 
  group_by(x, y) %>% 
  summarise(sum = sum(presence))
write_csv(sr_bird_pak, "sr_bird_pak_noseason.csv")

# Rasterize new Birdlife species data for Pakistan species
library(rasterSp)
rasterizeRange(dsn=list.files(paste0(filedir, "/BirdLife_2017/"), pattern=".shp", 
                             full.names=TRUE), id="SCINAME", 
              resolution=0.5, save=TRUE, seasonal=c(1,2), 
              origin=1, presence=c(1,2), filepath=paste0(filedir, "/gridded_BL_2017/"))

# Rasterize Bird species data from Imran
library(rasterSp)
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
bird_sr <- calc(bird_stack, fun=sum, na.rm=TRUE)
