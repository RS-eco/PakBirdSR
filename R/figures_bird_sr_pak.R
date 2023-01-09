#' ---
#' title: "Data Analysis of Bird SR in Pakistan"
#' author: "RS-eco"
#' ---

#' Load packages
library(ggplot2)
library(dplyr)
library(magrittr)
library(readr)

#' Load patchwork package

# Install patchwork package from Github
#+ eval=F
# install.packages("devtools")
#devtools::install_github("thomasp85/patchwork")

# Load package
library(patchwork)

#' Set working directory
#setwd("C:\\Users\\Imran Khaliq\\Dropbox\\Bird_SR_Pakistan")
#getwd()

#' Your working directory should be the Dropbox folder (Bird_SR_Pakistan)

#' Get outline of Pakistan

# Load GADM of Pakistan
pak <- raster::getData(name="GADM", country="PAK", level=1)

#' Define several plot options

# Define colour theme for plotting
red_blue <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                                  "#7FFF7F", "yellow", 
                                  "#FF7F00", "red", "#7F0000"))(255)

# Define map theme
theme_map <- function(base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = element_blank())
}

########################################

#' IUCN breeding and wintering bird richness

# Read data
breeding_birds <- read.csv("data/breeding_bird_sr_pak.csv.xz")
wintering_birds <- read.csv("data/wintering_bird_sr_pak.csv.xz")

## Create maps of SR
breeding_birds$season <- "Breeding"
wintering_birds$season <- "Wintering"

bird_sr <- bind_rows(breeding_birds, wintering_birds)

ggplot() + geom_tile(data=bird_sr, aes(x=x,y=y,fill=sum)) + 
  geom_polygon(data=pak, aes(x=long, y=lat, group=group), 
               colour="black", fill=NA) + facet_wrap(~ season) + 
  theme_map() + coord_map() + 
  scale_fill_gradientn(name="SR", colours=red_blue, 
                       limits=c(40,312), breaks=seq(50,300,length=6)) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14, face="bold"))
ggsave("figures/iucn_birds_sr_pak.png", 
       dpi=600, width=9, height=4)

########################################

#' Breeding and wintering bird SR quartiles

# Calculate richness quartiles
quantile(breeding_birds$sum)
quantile(wintering_birds$sum)

# Cut richness into quartiles
breeding_birds$quartile <- cut(breeding_birds$sum, 
                               breaks = quantile(breeding_birds$sum), 
                                          include.lowest = T)
wintering_birds$quartile <- cut(wintering_birds$sum, 
                                breaks = quantile(wintering_birds$sum), 
                                include.lowest = T)

# Turn quartiles into 1:4
breeding_birds$quartile <- factor(breeding_birds$quartile, labels=c("Low", "Medium", "Moderate", "High"))
wintering_birds$quartile <- factor(wintering_birds$quartile, labels=c("Low", "Medium", "Moderate", "High"))

# Plot histograms
p1 <- ggplot() + 
  geom_histogram(data=breeding_birds, aes(sum), 
                 colour="black", fill="transparent", 
                 bins=25, boundary=0.5) + 
  scale_y_continuous(expand=c(0,0), limits=c(0,50)) + 
  theme_bw() + labs(x="Breeding Bird SR", y="Number of grid cells") + 
  geom_vline(xintercept=quantile(breeding_birds$sum), col="red")
p2 <- ggplot() + 
  geom_histogram(data=wintering_birds, aes(sum), 
                 colour="black", fill="transparent", 
                 bins=30, boundary=0.5) + 
  scale_y_continuous(expand=c(0,0), limits=c(0,50)) + 
  theme_bw() + labs(x="Wintering Bird SR", y="") + 
  geom_vline(xintercept=quantile(wintering_birds$sum), col="red")

# Display joint map
p1 + p2

#Save to file
ggsave("figures/iucn_birds_sr_histogram_pak.png", dpi=600, width=10,height=6)

breeding_birds$season <- "Breeding"
wintering_birds$season <- "Wintering"

bird_sr <- bind_rows(breeding_birds, wintering_birds)

# Plot quartile maps
ggplot() + geom_tile(data=bird_sr, aes(x=x,y=y,fill=quartile)) + 
  geom_polygon(data=pak, aes(x=long, y=lat, group=group), 
               colour="black", fill=NA) + facet_wrap(~ season) + 
  theme_map() + coord_map() + 
  scale_fill_manual(values=rev(ggsci::pal_locuszoom("default")(4))) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size=14, face="bold"))
ggsave("figures/iucn_birds_sr_quartiles_pak.png", 
       dpi=600, width=9, height=4)
ggplot() + geom_tile(data=bird_sr, aes(x=x,y=y,fill=quartile)) + 
  geom_polygon(data=pak, aes(x=long, y=lat, group=group), 
               colour="black", fill=NA) + 
  facet_wrap(season ~ ., ncol=1) + 
  scale_fill_manual(values=rev(pal_locuszoom("default")(4))) + 
  theme_map() + coord_map() + 
  theme(strip.background = element_blank(), 
        legend.position = "bottom",
        strip.text = element_text(size=14, face="bold"))
ggsave("figures/iucn_birds_sr_quartiles_pak_high.png", 
       dpi=600, width=5, height=8)