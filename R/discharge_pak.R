#' ---
#' title: "Extract discharge data for Pakistan"
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

# Files are going to be read from your working directory

# Get GADM of Pakistan
pak <- getData(name="GADM", country="PAK", level=1)

########################################

#' ISIMIP2b Discharge data

#' Several groups within the ISIMIP provide discharge data, 
#' however at the moment (August 2018) only mpi-hm provides discharge data for the past 
#' and future using three models.

# Set variable
var <- "maxdis"

# Set file directory
filedir <- "/mnt/sda1/Documents/Wissenschaft/Data"

# Run through different scenarios (i <- 1:3)
i <- 3
scenario <- c("historical", "rcp26", "rcp60")[i]
startyear <- c(1861, 2006, 2006)[i]
endyear <- c(2005, 2099, 2099)[i]

# Crop files by extent of Pakistan, convert units and save to file
lapply(c("gfdl-esm2m", "ipsl-cm5a-lr", "miroc5"), function(model){
  if(!file.exists(paste0("env_data/", scenario, "_", model, "_", var, "_pak.nc"))){
    # List files
    files <- list.files(path=filedir, pattern=glob2rx(paste0("*mpi-hm*", model, "*", scenario, "*", var, "_global*")), 
                        full.names=T, recursive=T)
    
    # Read dat and crop by pak extent
    data <- raster::mask(raster::crop(raster::stack(files), pak), pak)
    
    # Save data to file
    raster::writeRaster(data, paste0("discharge_data/", scenario, "_", model, "_", var, "_pak.tif"), 
                        datatype="INT2S", options="COMPRESS=LZW", overwrite=T)
  }
  return(NULL)
})

########################################

# Currently we use monthly data
# If we use daila data, change Water model (other models are available for all 4 GCMs)
# To calculate summary (summer vs. winter) we currently calculate 

#' Set variable
var <- "maxdis"

#' Summarise discharge data
for(i in 1:3){
  scenario <- c("historical", "rcp26", "rcp60")[i]
  starttime <- c("1861-01-01", "2006-01-01", "2006-01-01")[i]
  endtime <- c("2005-12-31", "2099-12-31", "2099-12-31")[i]
  
  library(magrittr); library(raster); library(lubridate)
  lapply(c("gfdl-esm2m", "ipsl-cm5a-lr", "miroc5"), function(model){
    data_pak <- stack(paste0("discharge_data/", scenario, "_", model, "_", var, "_pak.tif"))
    
    # Set time information of raster
    data_pak <- setZ(data_pak, z=unique(zoo::as.yearmon(seq(as.Date(starttime), as.Date(endtime), by=1))), name="date")
    
    # Extract winter temperature
    winter_data <- subset(data_pak, which(month(getZ(data_pak)) %in% c(10,11,12)))
    
    # Extract summer tmeanerature
    summer_data <- subset(data_pak, which(month(getZ(data_pak)) %in% c(3,4,5)))
    rm(data_pak)
    
    # Calculate annual min/max
    if(var == "mindis"){
      an_summer_data <- zApply(summer_data, by=year, fun=min, name='years')
    } else if(var=="maxdis"){
      an_summer_data <- zApply(summer_data, by=year, fun=max, name='years')
    }
    
    # Turn data into data.frame
    an_summer_data <- as.data.frame(rasterToPoints(an_summer_data))
    colnames(an_summer_data) <- c("x", "y", unique(year(getZ(summer_data))))
    an_summer_data %<>% tidyr::gather(year, an_summer_data, -c(x,y))
    write.csv(an_summer_data, paste0("discharge_data/an_summer_", scenario, "_", model, "_", var, "_pak.csv"), row.names=F)
    
    if(var == "mindis"){
      an_winter_data <- zApply(winter_data, by=year, fun=min, name='years')
    } else if(var=="maxdis"){
      an_winter_data <- zApply(winter_data, by=year, fun=max, name='years')
    }
    
    # Turn data into data.frame
    an_winter_data <- as.data.frame(rasterToPoints(an_winter_data))
    colnames(an_winter_data) <- c("x", "y", unique(year(getZ(winter_data))))
    an_winter_data %<>% tidyr::gather(year, an_winter_data, -c(x,y))
    
    save(an_winter_data, paste0("discharge_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"), row.names=F)
  })
}

########################################
########################################
########################################

###
#' Old discharge code, not relevant anymore!!!
###

# Process discharge data
library(processNC)
models <- c("gfdl-esm2m", "ipsl-cm5a-lr")

dist_list_pak <- lapply(models, FUN=function(x){
  #' Load ISIMIP2b Discharge data
  dis_global <- list.files(paste0("E:/Data/ISIMIP2b/OutputData/mpi-hm/", x),
                           pattern="dis_global", full.names = T)
  dis_picontrol_2005soc_2050 <- summariseNC(files=dis_global[1:4], startyear=2036, extent=gadm, endyear=2065, filename1=paste0("mpi-hm_", x, "_picontrol_2005soc_co2_dis_", country, "_monthly_2050.grd"), format="raster")
  dis_picontrol_2005soc_2080 <- summariseNC(files=dis_global[4:7], startyear=2066, extent=gadm, endyear=2095, filename1=paste0("mpi-hm_", x, "_picontrol_2005soc_co2_dis_", country, "_monthly_2080.grd"), format="raster")
  dis_picontrol_histsoc_1985 <- summariseNC(files=dis_global[8:11], startyear=1970, extent=gadm, endyear=1999, filename1=paste0("mpi-hm_", x, "_picontrol_histsoc_co2_dis_", country, "_monthly_1985.grd"), format="raster")
  dis_rcp26_2005soc_2050 <- summariseNC(files=dis_global[12:15], startyear=2036, extent=gadm, endyear=2065, filename1=paste0("mpi-hm_", x, "_rcp26_2005soc_co2_dis_", country, "_monthly_2050.grd"), format="raster")
  dis_rcp26_2005soc_2080 <- summariseNC(files=dis_global[15:18], startyear=2066, extent=gadm, endyear=2095, filename1=paste0("mpi-hm_", x, "_rcp26_2005soc_co2_dis_", country, "_monthly_2080.grd"), format="raster")
  dis_rcp60_2005soc_2050 <- summariseNC(files=dis_global[19:22], startyear=2036, extent=gadm, endyear=2065, filename1=paste0("mpi-hm_", x, "_rcp60_2005soc_co2_dis_", country, "_monthly_2050.grd"), format="raster")
  dis_rcp60_2005soc_2080 <- summariseNC(files=dis_global[22:25], startyear=2066, extent=gadm, endyear=2095, filename1=paste0("mpi-hm_", x, "_rcp60_2005soc_co2_dis_", country, "_monthly_2080.grd"), format="raster")
  
  # Create list of files
  dis_list <- list(dis_picontrol_histsoc_1985, dis_picontrol_2005soc_2050, 
                   dis_picontrol_2005soc_2080, dis_rcp26_2005soc_2050, 
                   dis_rcp26_2005soc_2080, dis_rcp60_2005soc_2050, 
                   dis_rcp60_2005soc_2080)
  
  # Calculate annual sum
  dis_list_gadm <- stack(lapply(dis_list, function(x) calc(x, sum)))
  names(dis_list_gadm) <- c("picontrol_histsoc_1985", "picontrol_2005soc_2050", 
                            "picontrol_2005soc_2080", "rcp26_2005soc_2050", 
                            "rcp26_2005soc_2080", "rcp60_2005soc_2050", 
                            "rcp60_2005soc_2080")
  rasterVis::levelplot(dis_list_gadm, par.settings=rasterVis::rasterTheme(region=rev(hexbin::BTC(n=9))), 
                       layout=c(4,2), main="Discharge")
  return(dis_list_gadm)
})

# Process discharge data
lapply(models, FUN=function(x){
  #' List ISIMIP2b monthly discharge data
  dis_list <- list.files(filedir, pattern=glob2rx(paste0("mpi-hm_", x, "*.grd")), full.names = T)
  
  #' Load data into R
  dis_data <- lapply(dis_list, stack)
  
  # Crop files by extent of Pakistan
  dis_list_pak <- lapply(dis_data, function(x) mask(crop(x, pak),pak))
  
  # Calculate annual sum
  dis_list_pak <- stack(lapply(dis_list_pak, function(x) calc(x, sum)))
  names(dis_list_pak) <- c("picontrol_2005soc_2050", "picontrol_2005soc_2080", 
                           "picontrol_histsoc_1985", "rcp26_2005soc_2050", 
                           "rcp26_2005soc_2080", "rcp60_2005soc_2050", 
                           "rcp60_2005soc_2080")
  rasterVis::levelplot(dis_list_pak, par.settings=rasterVis::rasterTheme(region=rev(hexbin::BTC(n=9))), 
                       layout=c(2,2), main="Discharge")
})

library(ggplot2)
discharge_files <- list.files("E:/Data/ISIMIP2b/DerivedOutputData/", 
                              pattern=".grd", full.names=TRUE)
lapply(1:length(discharge_files), function(i){
  discharge <- stack(discharge_files[i])
  discharge_df <- as.data.frame(rasterToPoints(discharge))
  colnames(discharge_df) <- c("x", "y", month.abb)
  ggplot() + geom_raster(data=discharge_df, aes(x=x, y=y, fill=Jul)) + 
    scale_fill_gradientn(colours=
                           colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                              "#7FFF7F", "yellow", 
                                              "#FF7F00", "red", "#7F0000"))(255))
  ggsave(sub(pattern=".grd", ".png", discharge_files[i]), width=8, height=6, dpi=300)
  readr::write_csv(discharge_df, sub(pattern=".grd", ".csv", discharge_files[i]))
})
file.remove(discharge_files)
file.remove(sub(pattern=".grd", ".gri", discharge_files))

########################################

#' Hydrosheds & Diva-GIS data

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
