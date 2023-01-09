#' ---
#' title: "MODIS land cover for Pakistan"
#' author: "RS-eco"
#' ---

# MODIS High-resolution Country-wise Landcover

#' MCD12Q1 the Yearly Tile Land Cover Type product from MODIS
#' Combined with a ground resolution of 500m

#' Download link to MCD12Q1 Product
#https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.051/2001.01.01/
#' You need to have a EarthData account to download the file

#' Identify required tiles
library(MODIS)
tiles <- getTile("Pakistan")

country <- "PAK"
gadm <- getData(name="GADM", country=country, level=0, path="C:/Users/admin/Documents/Data/GADM")

# Set file directory
filedir <- "C:/Users/admin/Documents/Data/MODIS/MCD12Q1/"

# Specify years
years <- 2001:2013

#' List correct files and turn into raster stacks
library(gdalUtils); library(raster)
lc_years <- lapply(years, function(y){
  file <- unlist(lapply(tiles@tile, function(t)
    list.files(filedir, pattern=paste0("MCD12Q1.A", y, "001.", t), full.names=T)))
  file <- file[!grepl(file, pattern=".hdf.xml")]
  
  #' Convert HDF File to raster stack
  data <- lapply(file, get_subdatasets)
  data <- lapply(data, function(dat){
    sub <- lapply(1:length(dat), function(x){
      tmp <- rasterTmpFile()
      extension(tmp) <- "tif"
      gdal_translate(dat[x], dst_dataset = tmp)
      raster(tmp)
    })
    proj <- projection(sub[[1]])
    gadm <- spTransform(gadm, proj)
    sub <- stack(sub)
    sub <- mask(crop(sub, gadm))
  })
  
  # Merge to one layer
  data <- do.call(merge, data)
  
  # Set names
  names(data) <- c("Land_Cover_Type_1", "Land_Cover_Type_2", "Land_Cover_Type_3",
                   "Land_Cover_Type_4", "Land_Cover_Type_5", "Land_Cover_Type_1_Assessment",
                   "Land_Cover_Type_2_Assessment", "Land_Cover_Type_3_Assessment",
                   "Land_Cover_Type_4_Assessment", "Land_Cover_Type_5_Assessment",
                   "Land_Cover_Type_QC", "Land_Cover_Type_1_Secondary",
                   "Land_Cover_Type_1_Secondary_Percent", "LC_Property_1", "LC_Property_2",
                   "LC_Property_3")
  return(data)
})

# Save to files
saveRDS(lc_years, "data/lc_years_PAK.rds", compress="xz")

# Extract Landcover for 2001 
lc_pak_2001 <- lc_years_PAK[[1]][[1]]
writeRaster(lc_pak_2001, "data/landcover_2001_pak.tif", format="GTiff")

# Read landcover data
lc_pak_2001 <- raster::stack("landcover_2001_pak.tif")

# Re-project data
lc_pak_2001 <- projectRaster(lc_pak_2001, crs="+init=epsg:4326")
lc_pak_2001 <- round(lc_pak_2001)

# Turn into data.frame
lc_pak_2001 <- as.data.frame(rasterToPoints(lc_pak_2001))

# Land cover classes
colnames(lc_pak_2001) <- c("x", "y", "ID")
classes <- data.frame(ID = c(0:16, 254, 255), 
                      Class = factor(c("Water", "Evergreen Needleleaf forest", 
                                       "Evergreen Broadleaf forest", 
                                       "Deciduous Needleleaf forest", 
                                       "Deciduous Broadleaf forest", 
                                       "Mixed forest", "Closed shrublands", 
                                       "Open shrublands", "Woody savannas", 
                                       "Savannas", "Grasslands", 
                                       "Permanent wetlands", "Croplands", 
                                       "Urban and built-up", "Cropland/Natural vegetation mosaic", 
                                       "Snow and ice", "Barren or sparsely vegetated", 
                                       "Unclassified", "Fill Value"), ordered=TRUE), 
                      colours=factor(c("blue", "palegreen", "darkgreen", "tomato3", 
                                       "green2", "green3", "brown", 
                                       "coral1", "lightseagreen", 
                                       "seagreen2", "green", "lightblue", 
                                       "yellow", "grey", "orange", "white", 
                                       "brown2", "transparent", "transparent"), ordered=TRUE))
lc_pak_2001 <- dplyr::left_join(lc_pak_2001, classes)
lc_pak_2001$ID <- 1
lc_pak_2001 <- lc_pak_2001 %>% spread(Class, ID) %>% dplyr::select(-colours)
names <- colnames(lc_pak_2001)

# Change resolution of raster and calculate % cover of each land cover class
lc_pak_2001 <- rasterFromXYZ(lc_pak_2001)
projection(lc_pak_2001) <- "+init=epsg:4326"
lc_area <- area(lc_pak_2001)
lc_pak_2001 <- lc_pak_2001*lc_area
names(lc_pak_2001) <- 
lc_pak_2001 <- aggregate(lc_pak_2001, fact=c(0.5/0.00483, 0.5/0.00417), fun=sum)
dat <- stack("bird_sr_pak_05deg.tif")
lc_pak_2001 <- resample(lc_pak_2001, dat)
lc_area <- area(lc_pak_2001)
lc_pak_2001 <- lc_pak_2001/lc_area*100
names(lc_pak_2001) <- names[3:19]
lc_pak_05 <- as.data.frame(rasterToPoints(lc_pak_2001))
readr::write_csv(lc_pak_05, "landcover_pak_2001_05deg.csv")

#lc_pak_05 <- readr::read_csv("landcover_pak_2001_05deg.csv")
#lc_pak_05 <- rasterFromXYZ(lc_pak_05)
#plot(lc_pak_05[[1]])
