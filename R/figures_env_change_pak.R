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

#' Observed temperature & precipitation change

# Define variable
var <- "tas" # Can be one of tas, tmin, tmax and pr
# Need to adapt colnames according to var!!!

# Read data
an_summer <- read.csv(paste0("env_data/an_summer_", var, "_pak.csv"))
an_winter <- read.csv(paste0("env_data/an_winter_", var, "_pak.csv"))

# Merge data with birds
an_summer %<>% left_join(breeding_birds)
an_winter %<>% left_join(wintering_birds)

# Create plot of data per quartile
an_summer %>% ggplot(aes(x=year, y=an_summer_tmean)) + geom_point() + 
  geom_smooth(method="lm") + facet_wrap(.~quartile) + 
  labs(y="Annual summer mean temperature")

an_summer %>% ggplot(aes(x=year, y=an_summer_tmean, colour=quartile)) +  
  geom_smooth(method="lm") + labs(y="Annual summer mean temperature")

# Create plot of distribution per quartile
an_summer %>% ggplot(aes(x=quartile, y=an_summer_tmean, fill=quartile)) + geom_boxplot() + 
  labs(y="Annual summer mean temperature") + theme(legend.position = "none")

#Create plot of change distribution per quartile

#Turn data to raster
an_summer %<>% select(x,y,year,an_summer_tmean) %>% 
  tidyr::spread(year, an_summer_tmean) %>% rasterFromXYZ %>% stack()
an_winter %<>% select(x,y,year,an_winter_tmean) %>% 
  tidyr::spread(year, an_winter_tmean) %>% rasterFromXYZ %>% stack()

# Calculate slope of each grid cell (see ?calc examples)
time <- 1:nlayers(an_summer)
X <- cbind(1, time)
invXtX <- solve(t(X) %*% X) %*% t(X)
quickfun <- function(y) (invXtX %*% y)[2]
summer_change <- calc(an_summer, quickfun)*35
winter_change <- calc(an_winter, quickfun)*35

# Add quartile information
summer_change %<>% rasterToPoints %>% as.data.frame %>% left_join(breeding_birds)
colnames(summer_change) <- c("x","y","delta_tas","sum","quartile")
winter_change %<>% rasterToPoints %>% as.data.frame %>% left_join(wintering_birds)
colnames(winter_change) <- c("x","y","delta_tas","sum","quartile")

# Create alternative plot
summer_change$season <- "Summer"
summer_change$quartile <- as.numeric(summer_change$quartile)
winter_change$season <- "Winter"
winter_change$quartile <- as.numeric(winter_change$quartile)
all_change <- bind_rows(summer_change, winter_change)
all_change %>% ggplot(aes(x=factor(quartile), y=delta_tas, fill=factor(season))) + 
  geom_boxplot(notch=T) + scale_fill_discrete(name="") + 
  labs(y=paste0("Observed annual", var, " change"))
ggsave(paste0("figures/obs_quartile_", var, "_change_pak.png"), dpi=600, width=8,height=6)

########################################

#' Temperature & Precipitation trend

# Define variable
var <-"tas" # One of tas, tmin, tmax, pr

# Need to adjust plot labels accordingly!!!

library(magrittr)
# Read data, turn into long format and add model, scenario and season information
dat <- lapply(c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"), function(model){
  dat <- lapply(c("historical", "rcp26", "rcp60"), function(scenario){
    an_summer <- readr::read_csv(paste0("data/an_winter_", scenario, "_", model, "_", var, "_pak.csv.xz"))
    an_summer %<>% tidyr::gather(year, value, -c(x,y))
    an_summer$season <- "summer"
    an_winter <- readr::read_csv(paste0("data/an_winter_", scenario, "_", model, "_", var, "_pak.csv.xz"))
    an_winter %<>% tidyr::gather(year, value, -c(x,y))
    an_winter$season <- "winter"
    dat <- dplyr::bind_rows(an_summer, an_winter)
    dat$Scenario <- scenario
    dat$model <- model
    return(dat)
  })
  dplyr::bind_rows(dat)
})
dat <- dplyr::bind_rows(dat)
dat$year <- as.numeric(dat$year)

library(dplyr)

# Calculate mean across models and create overall envdata time series
std <- function(x) sd(x)/sqrt(length(x))
ci <- function(x) qnorm(0.975)*sd(x)/sqrt(length(x))
library(zoo)
dat %>% filter(Scenario != "rcp60") %>% 
  group_by(x,y, season) %>% arrange(year) %>%
  mutate(value=rollapply(value,30, mean,align='right',fill="NA")) %>%
  group_by(Scenario, season, year) %>% 
  summarise(mean=mean(value, na.rm=T), se=ci(value), 
            min=min(value, na.rm=T), max=max(value, na.rm=T)) %>% 
  ggplot() + geom_ribbon(aes(x=year, ymin = mean-se, 
                             ymax = mean+se, fill=Scenario), alpha=0.3) + 
  geom_line(aes(x=year, y=mean, linetype=Scenario)) + 
  labs(x="Year", y="30-yr mean temperature") + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050, 2080), 
                     limits=c(1900,2065), expand = c(0,0)) +
  theme_bw() + theme(legend.position=c(0.2,0.8),
                     text = element_text(size=12))
ggsave(paste0("figures/", var, "_pak.png"), dpi=600, width=6, height=4)

# Merge data with bird quantiles
an_summer <- dat %>% filter(season=="summer") %>% 
  inner_join(breeding_birds)
an_winter <- dat %>% filter(season=="winter") %>% 
  inner_join(wintering_birds)
dat <- bind_rows(an_summer, an_winter)

# Create envdata time series split by bird quartiles
library(ggsci)
dat %>% 
  mutate(season = factor(season, labels=c("Breeding", "Wintering"))) %>%
  filter(model %in% c("GFDL-ESM2M", "IPSL-CM5A-LR", "MIROC5"),
         Scenario %in% c("historical", "rcp60")) %>% 
  group_by(x, y, model, season, quartile) %>% arrange(year) %>%
  mutate(value=rollapply(value,30, mean, align='right',fill="NA")) %>%
  group_by(Scenario, year, season, quartile) %>% 
  summarise(mean=mean(value, na.rm=T)) %>% 
  ggplot() + geom_line(aes(x=year, y=mean, linetype=Scenario, 
                           colour=quartile), size=1) + 
  labs(x="Year", y="30-yr mean temperature") + 
  facet_grid(.~season) + 
  scale_colour_manual(values=rev(pal_locuszoom("default")(4))) + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050, 2080), 
                     limits=c(1900,2065), expand = c(0,0)) +
  theme_bw() + theme(strip.background = element_blank(),
                     legend.position="none",
                     panel.spacing.x = unit(1, "lines"),
                     strip.text = element_text(size=12, face="bold"))
ggsave(paste0("figures/quartile_", var, "_pak.png"), 
       dpi=600, width=6, height=2.5)

########################################

#' Discharge trend

var <- "mindis"

# Read data, turn into long format and add model, scenario and season information
dat <- lapply(c("gfdl-esm2m", "ipsl-cm5a-lr", "miroc5"), function(model){
  dat <- lapply(c("historical", "rcp26", "rcp60"), function(scenario){
    an_summer <- readr::read_csv(paste0("discharge_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"))
    an_summer$season <- "summer"
    an_winter <- readr::read_csv(paste0("discharge_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"))
    an_winter$season <- "winter"
    dat <- dplyr::bind_rows(an_summer, an_winter)
    dat$Scenario <- scenario
    dat$model <- model
    return(dat)
  })
  dplyr::bind_rows(dat)
})
dat <- dplyr::bind_rows(dat)
dat$year <- as.numeric(dat$year)
colnames(dat) <- c("x", "y", "year", "value", "season", "Scenario", "model")

library(dplyr); library(ggplot2)
# Calculate mean across models and create overall envdata time series
dat %>% group_by(x,y, season) %>% arrange(year) %>%
  mutate(value=rollapply(value,30, mean,align='right',fill="NA")) %>%
  group_by(Scenario, year, season) %>% 
  summarise(mean=mean(value, na.rm=T), sd=ci(value), 
            min=min(value, na.rm=T), max=max(value, na.rm=T)) %>% 
  ggplot() + geom_ribbon(aes(x=year, ymin = mean-sd, 
                             ymax = mean+sd, fill=Scenario), alpha=0.3) + 
  geom_line(aes(x=year, y=mean, linetype=Scenario)) + theme_bw() + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050, 2080), 
                     limits=c(1900,2065), expand = c(0,0)) +
  labs(y="30-yr mean minimum discharge")
ggsave(paste0("figures/", var, "_pak.png"), dpi=300, width=10, height=6)

# Merge data with bird quantiles
an_summer <- dat %>% filter(season=="summer") %>% 
  inner_join(breeding_birds)
an_winter <- dat %>% filter(season=="winter") %>% 
  inner_join(wintering_birds)
dat <- bind_rows(an_summer, an_winter)

#computation of the standard error of the mean
#x <- rnorm(1:10)
#sem<-sd(x)/sqrt(length(x))
#95% confidence intervals of the mean
#c(mean(x)-2*sem,mean(x)+2*sem)
# If data is not normally distributed, use bootstrapping for calculating confidence intervals!!!

# Create envdata time series split by bird quartiles
dat %>% 
  mutate(season = factor(season, labels=c("Breeding", "Wintering"))) %>%
  filter(model %in% c("gfdl-esm2m", "ipsl-cm5a-lr", "miroc5"),
         Scenario %in% c("historical", "rcp60")) %>% 
  group_by(x, y, model, season, quartile) %>% arrange(year) %>%
  mutate(value=rollapply(value,30, mean,align='right',fill="NA")) %>%
  group_by(Scenario, year, season, quartile) %>% 
  summarise(mean=mean(value, na.rm=T)) %>% 
  ggplot() + 
  geom_line(aes(x=year, y=mean, linetype=Scenario, 
                colour=quartile), size=1) + 
  labs(x="Year", y="30-yr mean minimum discharge") + 
  facet_grid(.~season) + 
  scale_colour_manual(values=rev(pal_locuszoom("default")(4))) + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050, 2080), 
                     limits=c(1900,2065), expand = c(0,0)) +
  theme_bw() + theme(strip.background = element_blank(),
                     legend.position="none",
                     panel.spacing.x = unit(1, "lines"),
                     strip.text = element_text(size=12, face="bold"))
ggsave(paste0("figures/quartile_", var, "_pak.png"), dpi=600, 
       width=6, height=2.5)

########################################

#' Land use trend

# Read data
histsoc <- read_csv("landuse_data/histsoc_landuse_pak.csv")
histsoc$scenario <- "historical"
histsoc$model <- "GFDL-ESM2M"
rcp26soc <- read_csv("landuse_data/rcp26soc_landuse_pak.csv")
rcp26soc$scenario <- "rcp26"
rcp60soc <- read_csv("landuse_data/rcp60soc_landuse_pak.csv")
rcp60soc$scenario <- "rcp60"

# Merge data
landuse <- bind_rows(list(histsoc, rcp26soc, rcp60soc))

# Add area to data
data(landseamask_generic, package="rISIMIP")
r <- raster::rasterize(pak, landseamask_generic)
r_df <- data.frame(raster::rasterToPoints(raster::area(r)))
colnames(r_df) <- c("x", "y", "area")
landuse %<>% left_join(r_df)

# Add wintering and breeding bird quartiles
lu_breeding <- landuse %>% left_join(breeding_birds) 
lu_breeding$season <- "Breeding"
lu_wintering <- landuse %>% left_join(wintering_birds)
lu_wintering$season <- "Wintering"
landuse <- bind_rows(lu_breeding, lu_wintering)

# Calculate sum of each land-use type
landuse_sum <- landuse %>% ungroup() %>% 
  mutate_at(c("cropland_irrigated", "cropland_rainfed", 
              "cropland_total", "pastures", 
              "urbanareas", "biofuel_cropland_irrigated", 
              "biofuel_cropland_rainfed"), funs(.*area))

# Calculate mean across models
landuse_mean <- landuse_sum %>% 
  dplyr::select(c("scenario", "year", "model", 
           "cropland_irrigated", "cropland_rainfed", "pastures")) %>% 
  group_by(scenario, year, model) %>% summarise_all(sum) %>% 
  tidyr::gather(var, value, -c(year, scenario, model)) %>% 
  dplyr::select(-model) %>% group_by(scenario, year, var) %>% 
  summarise(mean=mean(value, na.rm=T), sd = ci(value), 
            min=min(value, na.rm=T), max=max(value, na.rm=T))

# Create overall landuse time series
landuse_mean %>% ggplot() + 
  geom_ribbon(aes(x=year, ymin = mean-sd, 
                  ymax = mean+sd, fill=scenario), alpha=0.3) + 
  geom_line(aes(x=year, y=mean, colour=var, linetype=scenario)) + 
  facet_wrap(.~var) + theme_bw() + 
  labs(x="Year", y="Area (ha)") + 
  theme(legend.position = c(0.08,0.77), 
        strip.background = element_blank())
ggsave("figures/landuse_pak.png", dpi=600, width=12, height=6)

# Create landuse time series split by bird quartiles
landuse_sum %>% 
  dplyr::select(c("x", "y", "scenario", "year", "model", "quartile", "season",
                  "cropland_irrigated", "cropland_rainfed", "pastures")) %>% 
  filter(model %in% c("GFDL-ESM2M", "IPSL-CM5A-LR", "MIROC5"),
         scenario %in% c("historical", "rcp60")) %>% 
  tidyr::gather(var, value, -c(x,y,year, scenario, model, quartile, season)) %>% ungroup() %>%
  group_by(scenario, year, var, season, model, quartile) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup() %>%
  mutate(var = factor(var, labels=c("cropland rainfed", "cropland irrigated",
                                    "pastures"))) %>%
  group_by(scenario, year, var, quartile, season) %>% 
  summarise(mean=mean(value, na.rm=T)) %>% 
  ggplot() + geom_line(aes(x=year, y=mean, linetype=scenario, 
                         colour=quartile), size=1) + 
  facet_grid(var~season, scales="free", switch="y") + 
  scale_colour_manual(values=rev(pal_locuszoom("default")(4))) + 
  labs(x="Year", y="") + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050, 2080), 
                     limits=c(1900,2065), expand = c(0,0)) +
  theme_bw() + theme(strip.background = element_blank(),
                     panel.spacing.x = unit(1, "lines"),
                     strip.placement = "outside",
                     strip.text.x = element_text(size=12, face="bold"),
                     strip.text.y = element_text(size=10))
ggsave("figures/quartile_landuse_pak.png", dpi=600, width=6, height=4)

########################################

#' Population trend

# Read data
histsoc <- read_csv("population_data/histsoc_population_pak.csv")
histsoc$Scenario <- "historical"
ssp2soc <- read_csv("population_data/ssp2soc_population_pak.csv")
ssp2soc$Scenario <- "ssp2"

# Bind data
population <- bind_rows(histsoc, ssp2soc)

# Add area to data
data(landseamask_generic, package="rISIMIP")
r <- raster::rasterize(pak, landseamask_generic)
r_df <- data.frame(raster::rasterToPoints(raster::area(r)))
colnames(r_df) <- c("x", "y", "area")
population %<>% left_join(r_df)

# Add wintering and breeding bird quartiles
pop_breeding <- population %>% left_join(breeding_birds) 
pop_breeding$season <- "Breeding"
pop_wintering <- population %>% left_join(wintering_birds)
pop_wintering$season <- "Wintering"
population <- bind_rows(pop_breeding, pop_wintering)

# Calculate sum
#population %<>% ungroup() %>% dplyr::select(-c(x,y)) %>% 
#  mutate(pop=value*area)

# Create overall population time series
population %>% dplyr::select(Scenario, year, value) %>% 
  group_by(Scenario, year) %>% 
  summarise_all(sum) %>% ggplot() + 
  geom_line(aes(x=year, y=value/1000000, linetype=Scenario)) + 
  labs(x="Year", y="Population size (Mio)") + theme_bw() + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050, 2080), 
                     limits=c(1900,2065), expand = c(0,0)) +
  theme(legend.position = c(0.2,0.8),
        text = element_text(size=12))
ggsave("figures/population_pak.png", dpi=600, width=6, height=4)

# Create population time series split by bird quartiles
population %>% 
  dplyr::select(c("year", "quartile", "season", "value", "Scenario")) %>% 
  group_by(season, year, quartile, Scenario) %>% summarise_all(sum) %>% 
  ggplot() + geom_line(aes(x=year, y=value/1000000, colour=quartile, 
                           linetype=Scenario), size=1) + facet_wrap(.~season) + 
  scale_colour_manual(values=rev(pal_locuszoom("default")(4))) + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050, 2080), 
                     limits=c(1900,2065), expand = c(0,0)) +
  theme_bw() + theme(strip.background = element_blank(),
                     panel.spacing.x = unit(1, "lines"),
                     strip.placement = "outside",
                     legend.position="none",
                     strip.text.x = element_text(size=12, face="bold"),
                     strip.text.y = element_text(size=10)) + 
  labs(x="Year", y="Population (Mio)")
ggsave("figures/quartile_population_pak.png", dpi=600, 
       width=6,height=2.5)
