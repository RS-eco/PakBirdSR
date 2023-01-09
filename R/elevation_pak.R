# Altitute and terrain data

# SRTM 3ArcSec Data was downloaded from: https://gdex.cr.usgs.gov/gdex/

library(raster)

# Get files
files <- list.files("extdata/NASA_SRTM_v3.0_PAK", pattern=".tif", full.names=T)

# Load data
data <- lapply(files, raster)

# Merge to one layer
alt <- do.call(merge, data)

# Load PAK outline
pak <- getData("GADM", country="PAK", level=1)

#Crop/Mask data
alt <- mask(crop(alt, pak), pak)

# Save to file
writeRaster(alt, "srtmv3.0_pak.tif", datatype="INT2S", options="COMPRESS=LZW")
alt <- raster("srtmv3.0_pak.tif")

# Calculate terrain variables
slope <- terrain(alt, opt='slope')
aspect <- terrain(alt, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)

# Terrain Ruggedness Index (TRI)
tri <-terrain(alt, opt="TRI")
#TRI <- focal(x, w=f, fun=function(x, ...) sum(abs(x[-5]-x[5]))/8, pad=TRUE, padValue=NA)

# Topographic Position Index (TPI)
tpi <- terrain(altx, opt="TPI")
#TPI <- focal(x, w=f, fun=function(x, ...) x[5] - mean(x[-5]), pad=TRUE, padValue=NA)

# Roughness
roughness <- terrain(alt, opt="roughness")
#rough <- focal(x, w=f, fun=function(x, ...) max(x) - min(x), pad=TRUE, padValue=NA, na.rm=TRUE)

# flowdir
flowdir <- terrain(alt, opt="flowdir")

# Turn into raster stack
terrain <- stack(alt, aspect, slope, hill, tri, tpi, roughness, flowdir)

#Save terrain data to file
writeRaster(terrain, "terrain_pak.tif", datatype="INT2S", options="COMPRESS=LZW")
terrain <- stack("terrain_pak.tif")

# Turn terrain data into 0.5 deg grid
terrain <- stack(aggregate(terrain, fact=0.5/0.0008333333, fun=mean),
                 aggregate(terrain, fact=0.5/0.0008333333, fun=sd),
                 aggregate(terrain, fact=0.5/0.0008333333, fun=min),
                 aggregate(terrain, fact=0.5/0.0008333333, fun=max))
terrain
vars <- c("elevation", "aspect", "slope", "hillshade", "tri", "tpi", "roughness", "flowdir")
names(terrain) <- c(paste0(vars, "_mn"), paste0(vars, "_sd"), paste0(vars, "_min"), paste0(vars, "_max"))

dat <- stack("bird_sr_pak_05deg.tif")
terrain <- resample(terrain, dat)

# Save as data.frame
terrain <- as.data.frame(rasterToPoints(terrain))
library(dplyr)
terrain <- terrain %>% select(-matches("hillshade"))
readr::write_csv(terrain, "terrain_pak_05deg.csv")

