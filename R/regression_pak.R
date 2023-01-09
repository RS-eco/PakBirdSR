#' ---
#' title: "Regression analysis of bird species richnes in Pakistan"
#' author: "RS-eco"
#' ---

#' Load packages
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(readr)

########################################

#' Load and adjust EWEMBI Data

# Define variables
vars <- c("tas", "tasmin", "tasmax", "pr")

# Load data
summerdata <- lapply(vars, function(x) read.csv(paste0("env_data/an_summer_", x, "_pak.csv")))
winterdata <- lapply(vars, function(x) read.csv(paste0("env_data/an_winter_", x, "_pak.csv")))

# Subset data to 30-year time period
summerdata <- lapply(summerdata, function(x) x %>% filter(year >= 1980 & year <= 2009))
winterdata <- lapply(winterdata, function(x) x %>% filter(year >= 1980 & year <= 2009))

# Calculate 30-average
summerdata <- lapply(summerdata, function(x) x %>% group_by(x,y) %>% 
                       dplyr::select(starts_with("an_summer")) %>% 
                       summarise_all(funs("mean" = mean)))
winterdata <- lapply(winterdata, function(x) x %>% group_by(x,y) %>% 
                       dplyr::select(starts_with("an_winter")) %>% 
                       summarise_all(funs("mean" = mean)))

# Turn into data.frame
summerdata <- Reduce(function(...) dplyr::left_join(..., by=c("x","y"), all.x=TRUE), summerdata)
colnames(summerdata) <- c("x", "y", vars)
winterdata <- Reduce(function(...) dplyr::left_join(..., by=c("x","y"), all.x=TRUE), winterdata)
colnames(winterdata) <- c("x", "y", vars); rm(vars)

########################################

#' Load and adjust ISIMIP2b Discharge data

# Process summer data
summerdischarge <- lapply(c("mindis", "maxdis"), function(x){
  
  # Loop through global circulation models
  summerdata <- lapply( c("gfdl-esm2m", "ipsl-cm5a-lr", "miroc5"), function(y){
    
  # Load data
  summerdata <- bind_rows(read.csv(paste0("discharge_data/an_summer_historical_", y, "_", x, "_pak.csv")), 
                          read.csv(paste0("discharge_data/an_summer_rcp26_", y, "_", x, "_pak.csv")), 
                          read.csv(paste0("discharge_data/an_summer_rcp60_", y, "_", x, "_pak.csv")))
  
  # Subset data to 30-year time period
  summerdata <- summerdata %>% filter(year >= 1980 & year <= 2009) %>% 
    group_by(x,y,year) %>% select(starts_with("an_summer")) %>% 
    summarise(an_summer_data=mean(an_summer_data))
    
  # Calculate 30-average
  summerdata %>% group_by(x,y) %>% 
                         select(starts_with("an_summer")) %>% 
                         summarise_all(funs("mean" = mean))
  
  })
  # Turn into data.frame
  bind_rows(summerdata) %>% group_by(x,y) %>% summarise(mean=mean(mean))
})
# Turn into data.frame
summerdischarge <- Reduce(function(...) dplyr::left_join(..., by=c("x","y"), all.x=TRUE), summerdischarge)

colnames(summerdischarge) <- c("x", "y", "mindis", "maxdis")


# Process winter data
winterdischarge <- lapply(c("mindis", "maxdis"), function(x){
  
  # Loop through global circulation models
  winterdata <- lapply(c("gfdl-esm2m", "ipsl-cm5a-lr", "miroc5"), function(y){
    
  winterdata <- bind_rows(read.csv(paste0("discharge_data/an_winter_historical_", y, "_", x, "_pak.csv")), 
                          read.csv(paste0("discharge_data/an_winter_rcp26_", y, "_", x, "_pak.csv")), 
                          read.csv(paste0("discharge_data/an_winter_rcp60_", y, "_", x, "_pak.csv")))
  
  # Subset data to 30-year time period
  winterdata <- winterdata %>% filter(year >= 1980 & year <= 2009) %>% 
    group_by(x,y,year) %>% select(starts_with("an_winter")) %>% 
    summarise(an_winter_data=mean(an_winter_data))
  
  winterdata <- winterdata %>% group_by(x,y) %>% 
                         select(starts_with("an_winter")) %>% 
                         summarise_all(funs("mean" = mean))
  })
  # Turn into data.frame
  bind_rows(winterdata) %>% group_by(x,y) %>% summarise(mean=mean(mean))
})
# Turn into data.frame
winterdischarge <- Reduce(function(...) dplyr::left_join(..., by=c("x","y"), all.x=TRUE), winterdischarge)
colnames(winterdischarge) <- c("x", "y", "mindis", "maxdis")

########################################

#' Load and adjust ISIMIP2b population data

# Read file
histsoc <- read.csv("population_data/histsoc_population_pak.csv")
ssp2soc <- read.csv("population_data/ssp2soc_population_pak.csv")

# Summarise file
population <- bind_rows(histsoc, ssp2soc) %>% filter(year >= 1980 & year <= 2009) %>% 
  group_by(x,y) %>% summarise(population=mean(value)); rm(histsoc, ssp2soc)

########################################

#' Load and adjust ISIMIP2b land-use data

# Read files
histsoc_landuse <- read.csv("landuse_data/histsoc_landuse_pak.csv") %>% filter(year >= 1980 & year <= 2009)
rcp26soc_landuse <- read.csv("landuse_data/rcp26soc_landuse_pak.csv") %>% 
  filter(year >= 1980 & year <= 2009) %>% group_by(x,y,year) %>% select(-model) %>% summarise_all(mean)
rcp60soc_landuse <- read.csv("landuse_data/rcp60soc_landuse_pak.csv") %>% 
  filter(year >= 1980 & year <= 2009) %>% group_by(x,y,year) %>% select(-model) %>% summarise_all(mean)

# Summarise data
landuse <- bind_rows(histsoc_landuse, rcp26soc_landuse, rcp60soc_landuse) %>% 
  group_by(x,y) %>% select(-year) %>% summarise_all(mean, na.rm=T)
rm(histsoc_landuse, rcp26soc_landuse, rcp60soc_landuse)

########################################

#' Load species data

breeding_birds <- read.csv("breeding_bird_sr_pak.csv")
wintering_birds <- read.csv("wintering_bird_sr_pak.csv")

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
breeding_birds$quartile <- factor(breeding_birds$quartile, labels=c(1:4))
wintering_birds$quartile <- factor(wintering_birds$quartile, labels=c(1:4))

#' Merge with envdata
breeding_birds <- left_join(breeding_birds, summerdata); rm(summerdata)
wintering_birds <- left_join(wintering_birds, winterdata); rm(winterdata)

#' Merge with discharge data
breeding_birds <- left_join(breeding_birds, summerdischarge); rm(summerdischarge)
wintering_birds <- left_join(wintering_birds, winterdischarge); rm(winterdischarge)

#' Merge with population data
breeding_birds <- left_join(breeding_birds, population)
wintering_birds <- left_join(wintering_birds, population); rm(population)

#' Merge with land-use data
breeding_birds <- left_join(breeding_birds, landuse)
wintering_birds <- left_join(wintering_birds, landuse); rm(landuse)

#-#-# Correlation matrix #-#-#

library(corrplot)
br_data <- breeding_birds[,c("tas", "tasmin", "tasmax", "pr","maxdis", "mindis", "population",
                              "cropland_irrigated", "cropland_rainfed", 
                              "pastures", "urbanareas")]
win_data <- wintering_birds[,c("tas", "tasmin", "tasmax", "pr", "maxdis", "mindis", "population",
                               "cropland_irrigated", "cropland_rainfed", 
                               "pastures", "urbanareas")]

CorD <- cor(br_data)

png("figures/cor_mat_breeding.png", width = 8, height = 7, units="in", res = 600)
corrplot(CorD, method="circle", bg = "white",
         addgrid.col = "gray10", tl.col = "black",
         tl.cex = 0.8, sig.level = 0.7)
dev.off()

breeding_birds$season <- "summer"
wintering_birds$season <- "winter"
mod_data <- bind_rows(breeding_birds, wintering_birds)

#' Run models per season and Quartile
models1 <- mod_data %>% 
  group_by(season, quartile) %>% 
  do(mod = lm(sum ~ tas + pr + population + mindis + 
                cropland_irrigated + cropland_rainfed + pastures, 
              data = .))
models2 <- mod_data %>% 
  group_by(season, quartile) %>% 
  do(mod = lm(sum ~ poly(tas, 2) + poly(pr, 2) + poly(population, 2) + 
                poly(mindis, 2) + poly(cropland_irrigated, 2) + 
                poly(cropland_rainfed, 2) + poly(pastures, 2), 
              data = .))
summary(models1$mod[[1]])
summary(models2$mod[[1]])

#' Do step-wise selection procedure
library(MASS)
step <- mod_data %>% 
  group_by(season, quartile) %>% 
  do(mod = stepAIC(lm(sum ~ tas + pr + population + mindis + 
                cropland_irrigated + cropland_rainfed + 
                pastures, data = .), direction="both"))
fin_mod <- step
fin_mod$mod <- sapply(step$mod, function(x) x$terms)
knitr::kable(fin_mod)

#' Extract coefficients of linear model
coef_mod <- step %>% do(data.frame(season= .$season, 
                                   quartile = .$quartile,
                                     var = names(coef(.$mod)), 
                                     coef(summary(.$mod))))

#' Extract R2 values and create table
step %>% do(data.frame(season= .$season, quartile=.$quartile,
                         R2 = summary(.$mod)$r.squared))

library(broom)
mod_res <- step %>% do(data.frame(season= .$season,
                                  quartile = .$quartile,
                                  tidy(.$mod)))
mod_res %>% filter(term != "(Intercept)") %>% 
  ggplot() + 
  geom_pointrange(aes(x=quartile, y=estimate, ymin=estimate-std.error,
                                 ymax=estimate+std.error, colour=term)) + 
  labs(x="Richness quartile", y="Coefficient", 
       colour="Variable") + 
  facet_grid(. ~ season, scales="free") + 
  scale_fill_manual(values=c("#00468BFF", "#0099B4FF", "#42B540FF",
                             "#925E9FFF", "#ADB6B6FF", "#FDAF91FF",
                             "#AD002AFF")) + 
  theme_bw() + theme(strip.background = element_blank(),
                     strip.text = element_text(size=12, face="bold"))
ggsave("figures/coefficient.png", width=8, height=4, dpi=600)

#' Look at relative importance of variables
library(relaimpo); library(ggsci)
rel <- step %>% 
  do(data.frame(season= .$season,
                quartile = .$quartile,
                var = names(calc.relimp(.$mod, type="lmg")$lmg),
                imp = calc.relimp(.$mod, type="lmg")$lmg))
rel %>% ungroup() %>%
  mutate(season = factor(season, labels=c("Breeding", "Wintering")),
         var = factor(var, labels=c("cropland irrigated", "cropland rainfed", "minimum discharge",
                      "pastures", "population", "prec", "temp")),
         quartile = factor(quartile, labels=c("Low", "Medium", "Moderate", "High"))) %>%
  ggplot() + geom_bar(aes(x=quartile, y=imp, fill=var),
                      stat="identity", position="stack") + 
  labs(x="Richness quartile", y="% Variance explained", fill="Variable") + 
  facet_grid(. ~ season) + 
  scale_fill_manual(values=c("#00468BFF", "#0099B4FF", "#42B540FF",
                             "#925E9FFF", "#ADB6B6FF", "#FDAF91FF",
                             "#AD002AFF")) + 
  scale_y_continuous(limits=c(0,0.601), expand=c(0,0)) + 
  theme_bw() + theme(strip.background = element_blank(),
                     strip.text = element_text(size=12, face="bold"))
ggsave("figures/var_imp.png", width=8, height=4, dpi=600)

library(ggpmisc)
mod_data %>% dplyr::select(sum, tas, pr, maxdis,
                    population, `cropland_irrigated`,
                    `cropland_rainfed`, pastures, season, quartile) %>%
  gather(var, value, -c(sum, season, quartile)) %>% 
  group_by(season, quartile) %>% 
  ggplot(aes(x=value, y=sum, colour=quartile)) + geom_point() + 
  facet_grid(season ~ var, scales="free_x", switch="x") + 
  geom_smooth(method="lm") + 
  #stat_poly_eq(formula = x~y, 
  #             aes(label =  paste(stat(eq.label), 
  #                                stat(adj.rr.label), sep = "~~~~")),
  #             parse = TRUE) + 
  labs(x="", y="Species richness") + theme_bw() + 
  theme(strip.background = element_blank(),
        strip.placement = "outside")
ggsave("figures/mod_fit_linear.png", width=10, height=5, dpi=600)

#' We can do the same using a quadratic term

mod_data %>% dplyr::select(sum, tas, pr, maxdis,
                           population, `cropland_irrigated`,
                           `cropland_rainfed`, pastures, 
                           season, quartile) %>%
  gather(var, value, -c(sum, season, quartile)) %>% 
  group_by(season) %>% 
  ggplot(aes(x=value, y=sum, colour=quartile)) + geom_point() + 
  facet_grid(season ~ var, scales="free_x", switch="x") + 
  geom_smooth(method="lm", formula= y ~ poly(x, 2)) + 
  #stat_poly_eq(formula = y ~ poly(x, 2), 
  #             aes(label =  paste(stat(eq.label), 
  #                                stat(adj.rr.label), sep = "~~~~")),
  #             parse = TRUE) + 
  labs(x="", y="Species richness") + theme_bw() + 
  theme(strip.background = element_blank(),
        strip.placement = "outside")
ggsave("figures/mod_fit_quad.png", width=10, height=5, dpi=600)

########################################

# Define variable
var <- "tas"

# Read data, turn into long format and add model, scenario and season information
tas <- lapply(c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"), function(model){
  dat <- lapply(c("historical", "rcp26", "rcp60"), function(scenario){
    an_summer <- readr::read_csv(paste0("env_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"))
    an_summer %<>% tidyr::gather(year, value, -c(x,y))
    an_summer$season <- "summer"
    an_winter <- readr::read_csv(paste0("env_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"))
    an_winter %<>% tidyr::gather(year, value, -c(x,y))
    an_winter$season <- "winter"
    dat <- dplyr::bind_rows(an_summer, an_winter)
    dat$Scenario <- scenario
    dat$model <- model
    return(dat)
  })
  dplyr::bind_rows(dat)
})
tas <- dplyr::bind_rows(tas)
tas$year <- as.numeric(tas$year)
colnames(tas)[4] <- var

# Define variable
var <- "pr"

# Read data, turn into long format and add model, scenario and season information
pr <- lapply(c("GFDL-ESM2M", "HadGEM2-ES", "IPSL-CM5A-LR", "MIROC5"), function(model){
  dat <- lapply(c("historical", "rcp26", "rcp60"), function(scenario){
    an_summer <- readr::read_csv(paste0("env_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"))
    an_summer %<>% tidyr::gather(year, value, -c(x,y))
    an_summer$season <- "summer"
    an_winter <- readr::read_csv(paste0("env_data/an_winter_", scenario, "_", model, "_", var, "_pak.csv"))
    an_winter %<>% tidyr::gather(year, value, -c(x,y))
    an_winter$season <- "winter"
    dat <- dplyr::bind_rows(an_summer, an_winter)
    dat$Scenario <- scenario
    dat$model <- model
    return(dat)
  })
  dplyr::bind_rows(dat)
})
pr <- dplyr::bind_rows(pr)
pr$year <- as.numeric(pr$year)
colnames(pr)[4] <- var

# Read data, turn into long format and add model, scenario and season information
var <- "mindis"
dis <- lapply(c("gfdl-esm2m", "ipsl-cm5a-lr", "miroc5"), function(model){
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
dis <- dplyr::bind_rows(dis)
dis$year <- as.numeric(dis$year)
dis$model <- factor(dis$model, labels=c("GFDL-ESM2M", "IPSL-CM5A-LR", "MIROC5"))
colnames(dis)[4] <- var

#' Land use trend

# Read data
histsoc <- read_csv("landuse_data/histsoc_landuse_pak.csv")
histsoc$Scenario <- "historical"
rcp26soc <- read_csv("landuse_data/rcp26soc_landuse_pak.csv")
rcp26soc$Scenario <- "rcp26"
rcp60soc <- read_csv("landuse_data/rcp60soc_landuse_pak.csv")
rcp60soc$Scenario <- "rcp60"

# Merge data
landuse <- bind_rows(list(histsoc, rcp26soc, rcp60soc))

#' Population trend

# Read data
histsoc <- read_csv("population_data/histsoc_population_pak.csv")
#histsoc$Scenario <- "historical"
ssp2soc <- read_csv("population_data/ssp2soc_population_pak.csv")
#ssp2soc$Scenario <- "ssp2"

# Bind data
population <- bind_rows(histsoc, ssp2soc)
colnames(population)[4] <- "population"

# Read data
breeding_birds <- read.csv("breeding_bird_sr_pak.csv")
wintering_birds <- read.csv("wintering_bird_sr_pak.csv")

breeding_birds$quartile <- cut(breeding_birds$sum, 
                               breaks = quantile(breeding_birds$sum), 
                               include.lowest = T)
wintering_birds$quartile <- cut(wintering_birds$sum, 
                                breaks = quantile(wintering_birds$sum), 
                                include.lowest = T)

# Turn quartiles into factor
breeding_birds$quartile <- factor(breeding_birds$quartile, labels=c(1:4))
wintering_birds$quartile <- factor(wintering_birds$quartile, labels=c(1:4))

# Merge data with bird quantiles
an_summer <- tas %>% full_join(pr) %>% full_join(dis) %>% 
  full_join(landuse) %>% full_join(population) %>%
  filter(season=="summer") %>% inner_join(breeding_birds)
an_winter <- tas %>% full_join(pr) %>% full_join(dis) %>% 
  full_join(landuse) %>% full_join(population) %>%
  filter(season=="winter") %>% inner_join(wintering_birds)
pred_data <- bind_rows(an_summer, an_winter)

# Predict model to future
sum_pred <- mod_data %>% filter(season == "summer") %>%
  group_by(quartile) %>% 
  do(stepAIC(lm(sum ~ tas + pr + population + mindis + 
                        cropland_irrigated + cropland_rainfed + 
                        pastures, data = .), direction="both") %>% 
       predict(pred_data) %>% data.frame(prediction = .))
sum_pred$x <- rep(pred_data$x, 4)
sum_pred$y <- rep(pred_data$y, 4)
sum_pred <- tidyr::drop_na(sum_pred)
sum_pred_all <- pred_data %>% filter(season == "summer") %>%
  left_join(fut_pred, by=c("x", "y", "season", "quartile"))

fut_pred %>% dplyr::select(x,y,season, year, prediction, sum, Scenario) %>%
  filter(season %in% c("summer", "winter"),
         Scenario %in% c("rcp26", "rcp60")) %>% 
  group_by(season, year, Scenario, x, y) %>%
  summarise(sum=mean(sum,na.rm=T), imp=mean(prediction,na.rm=T)) %>%
  group_by(season,year,Scenario) %>% 
  summarise(sum=mean(sum,na.rm=T), imp=mean(prediction, na.rm=T)) %>%
  ggplot(aes(x=year, y=imp-sum, colour=Scenario)) + geom_line() + 
  facet_grid(season ~ ., scales="free_x", switch="x") + 
  labs(x="", y="Combined impact") + theme_bw() + 
  scale_x_continuous(limits=c(2005, 2100), expand=c(0,0)) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside")
ggsave("figures/comb_impact.png", width=10, height=5, dpi=600)

# Predict model to future
models <- mod_data %>% 
  gather(var, value, c(tas, pr, population, dis, cropland_irrigated,
                       cropland_rainfed)) %>%
  group_by(season, var) %>% 
  do(mod = lm(sum ~ poly(value, 2), data = .))

sum_pred <- data.frame(imp1=predict(models$mod1[[1]], an_summer),
                       imp2=predict(models$mod2[[1]], an_summer),
                       imp3=predict(models$mod3[[1]], an_summer),
                       imp4=predict(models$mod4[[1]], an_summer),
                       imp5=predict(models$mod5[[1]], an_summer),
                       imp6=predict(models$mod6[[1]], an_summer),
                       imp7=predict(models$mod7[[1]], an_summer))
win_pred <- data.frame(imp1=predict(models$mod1[[2]], an_winter),
                       imp2=predict(models$mod2[[2]], an_winter),
                       imp3=predict(models$mod3[[2]], an_winter),
                       imp4=predict(models$mod4[[2]], an_winter),
                       imp5=predict(models$mod5[[2]], an_winter),
                       imp6=predict(models$mod6[[2]], an_winter),
                       imp7=predict(models$mod7[[2]], an_winter))
sum_pred <- bind_cols(an_summer, sum_pred)
win_pred <- bind_cols(an_winter, win_pred)
fut_pred <- bind_rows(sum_pred, win_pred)
fut_pred %>% dplyr::select(x,y,season, year, sum, Scenario,
                           imp1,imp2,imp3,imp4,imp5,imp6,imp7) %>%
  filter(season %in% c("summer", "winter"),
         Scenario %in% c("rcp26", "rcp60")) %>% 
  tidyr::gather(var, imp, c(imp1,imp2,imp3,imp4,imp5,imp6,imp7)) %>% 
  mutate(var = factor(var, labels=c("tas", "pr", "population",
                                    "dis", "cropland_irrigated",
                                    "cropland_rainfed", "pastures"))) %>% 
  group_by(season, year, Scenario, x, y, var) %>%
  summarise(sum=mean(sum,na.rm=T), imp=mean(imp,na.rm=T)) %>%
  group_by(season,year,Scenario, var) %>% 
  summarise(sum=mean(sum,na.rm=T), imp=mean(imp, na.rm=T)) %>%
  ggplot(aes(x=year, y=imp-sum, colour=Scenario)) + geom_line() + 
  facet_grid(season ~ var, switch="x") + 
  labs(x="", y="Individual impact") + theme_bw() + 
  scale_x_continuous(limits=c(2005, 2100), expand=c(0,0)) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside")
ggsave("figures/ind_impact.png", width=8, height=4, dpi=600)

