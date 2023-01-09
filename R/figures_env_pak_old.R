data("yearmean_tas")
data("runmean31_tas")

# Change scenario names of mixed scenario data
runmean31_tas$scenario[runmean31_tas$scenario %in% c("piControl-historical-rcp26", "piControl-historical-rcp60") & runmean31_tas$year < 1860] <- "piControl"
runmean31_tas$scenario[runmean31_tas$scenario %in% c("piControl-historical-rcp26", "piControl-historical-rcp60") & runmean31_tas$year >= 1860 & runmean31_tas$year <= 2005] <- "historical"
runmean31_tas$scenario[runmean31_tas$scenario == "piControl-historical-rcp26" & runmean31_tas$year > 2005] <- "rcp26"
runmean31_tas$scenario[runmean31_tas$scenario == "piControl-historical-rcp60" & runmean31_tas$year > 2005] <- "rcp60"

# Merge data for plotting
all_data <- dplyr::left_join(yearmean_tas, runmean31_tas, by=c("year", "model", "scenario"))

# Add missing data to all_data
all_data$model <- factor(all_data$model, levels=c("IPSL-CM5A-LR", "GFDL-ESM2M", "MIROC5", "HadGEM2-ES"))
all_data$`tas(K).x` <- all_data$`tas(K).x`-273.15
all_data$`tas(K).y` <- all_data$`tas(K).y`-273.15

# Calculate deltaT from 1661-1860 piControl mean temperature
library(dplyr)
baseline_data <- all_data %>% group_by(model) %>% filter(scenario == "piControl", year >= 1661, year <= 1860) %>% summarise(baseline = mean(`tas(K).x`))
all_data <- left_join(all_data, baseline_data)
all_data$deltaT <- all_data$`tas(K).y` - all_data$baseline

all_data_long <- tidyr::gather(all_data, "var", "tas", -c(year, model, scenario, baseline))
all_data_long$var <- factor(all_data_long$var, labels=c("1"=paste("31-year running mean ", '\U0394', "T (?C)"), "2"="annual T (?C)", "3"="31-year running mean T (?C)"))
all_data_long$var <- factor(all_data_long$var, levels=levels(all_data_long$var)[c(2,3,1)])

library(ggplot2)
all_data_long %>% filter(year >= 1900 & year <= 2005) %>% 
  filter(var == "31-year running mean  Δ T (?C)") %>%
  filter(scenario %in% c("historical", "rcp26", "rcp60")) %>%
  group_by(year, scenario, var) %>% 
  summarise(mean=mean(tas, na.rm=T), 
            min=min(tas, na.rm=T), 
            max=max(tas, na.rm=T)) %>% 
  ggplot() + 
  geom_ribbon(aes(x=year, ymin = min, 
                  ymax = max, fill=scenario), alpha=0.3) + 
  geom_line(aes(x=year, y=mean, linetype=scenario)) + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050, 2080), 
                     limits=c(1900,2065), expand = c(0,0)) +
  scale_y_continuous(breaks=c(0,1,2,3), limits=c(-0.5,3)) + 
  labs(x="Year", y="30-yr mean temperature change") + 
  theme_bw() + theme(strip.background= element_blank(),
                     strip.placement= "outside",
                     legend.position = c(0.1,0.85),
                     legend.title = element_blank(),
                     text=element_text(size=12),
                     legend.background = element_rect(fill = NA),
                     panel.spacing.x=unit(0.25, "lines"),
                     panel.spacing.y=unit(0.25, "lines"))
ggsave("temp_change_hist.png", width=6, height=4)

all_data_long %>% filter(year >= 1900 & year <= 2065) %>% 
  filter(var == "31-year running mean  Δ T (?C)") %>%
  filter(scenario %in% c("historical", "rcp26", "rcp60")) %>%
  group_by(year, scenario, var) %>% 
  summarise(mean=mean(tas, na.rm=T), 
            min=min(tas, na.rm=T), 
            max=max(tas, na.rm=T)) %>% 
  ggplot() + 
  geom_ribbon(aes(x=year, ymin = min, 
                  ymax = max, fill=scenario), alpha=0.3) + 
  geom_line(aes(x=year, y=mean, linetype=scenario)) + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050), 
                     limits=c(1900,2065), expand = c(0,0)) + 
  scale_y_continuous(breaks=c(0,1,2,3), limits=c(-0.5,3)) + 
  labs(x="Year", y="30-yr mean temperature change") + 
  theme_bw() + theme(strip.background= element_blank(),
                     strip.placement= "outside",
                     legend.position = c(0.1,0.85),
                     legend.title = element_blank(),
                     text = element_text(size=12),
                     legend.background = element_rect(fill = NA),
                     panel.spacing.x=unit(0.25, "lines"),
                     panel.spacing.y=unit(0.25, "lines"))
ggsave("temp_change_fut.png", width=6, height=4)

data_ts <- readr::read_csv("data/crop_area_ts.csv")
data_ts$scenario[data_ts$scenario == "rcp26soc"] <- "rcp26"
data_ts$scenario[data_ts$scenario == "rcp60soc"] <- "rcp60"
data_ind <- data_ts[data_ts$var != "cropland_total",]
data_ts %>% filter(var != "cropland_total",
                   scenario %in% c("histsoc", "rcp26", "rcp60"),
                   year >= 1900 & year <= 2005) %>%
ggplot(aes(x = year, y = area/1000000, colour = factor(var), 
                            linetype=factor(scenario))) + 
  scale_y_continuous(limits=c(-0.5,35),expand=c(0,0)) + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050),
                     limits=c(1900,2065), expand = c(0,0)) + 
  scale_colour_discrete(name="Landuse type", 
                        labels=c("Biofuel cropland irrigated", 
                                 "Biofuel cropland rainfed", 
                                 "Cropland irrigated", 
                                 "Cropland rainfed", 
                                 "Pastures", 
                                 "Urban areas")) + 
  labs(x= "Year", y="Area (km² x 1,000,000)") + 
  scale_linetype_discrete(name="Scenario") + 
  geom_line(size=1) + theme_bw() + 
  theme(strip.background= element_blank(),
        text=element_text(size=12))
ggsave(paste0("lu_hist.png"), width=6, height=4, dpi=600)

data_ts %>% filter(var != "cropland_total",
                   scenario %in% c("histsoc", "rcp26", "rcp60"),
                   year >= 1900 & year <= 2065) %>%
  ggplot(aes(x = year, y = area/1000000, colour = factor(var), 
             linetype=factor(scenario))) + 
  scale_x_continuous(breaks=c(1900, 1930, 1960, 1990, 2020, 2050),
                     limits=c(1900,2065), expand = c(0,0)) + 
  scale_y_continuous(limits=c(-0.5,35),expand=c(0,0)) + 
  scale_colour_discrete(name="Landuse type", 
                        labels=c("Biofuel cropland irrigated", 
                                 "Biofuel cropland rainfed", 
                                 "Cropland irrigated", 
                                 "Cropland rainfed", 
                                 "Pastures", 
                                 "Urban areas")) + 
  labs(x= "Year", y="Area (km² x 1,000,000)") + 
  scale_linetype_discrete(name="Scenario") + 
  geom_line(size=1) + theme_bw() + 
  theme(strip.background= element_blank(),
        text=element_text(size=12))
ggsave("lu_fut.png", width=6, height=4, dpi=600)

data("yearmean_tas")
data("runmean31_tas")

# Change scenario names of mixed scenario data
runmean31_tas$scenario[runmean31_tas$scenario %in% c("piControl-historical-rcp26", "piControl-historical-rcp60") & runmean31_tas$year < 1860] <- "piControl"
runmean31_tas$scenario[runmean31_tas$scenario %in% c("piControl-historical-rcp26", "piControl-historical-rcp60") & runmean31_tas$year >= 1860 & runmean31_tas$year <= 2005] <- "historical"
runmean31_tas$scenario[runmean31_tas$scenario == "piControl-historical-rcp26" & runmean31_tas$year > 2005] <- "rcp26"
runmean31_tas$scenario[runmean31_tas$scenario == "piControl-historical-rcp60" & runmean31_tas$year > 2005] <- "rcp60"

# Merge data for plotting
all_data <- dplyr::left_join(yearmean_tas, runmean31_tas, by=c("year", "model", "scenario"))

# Add missing data to all_data
all_data$model <- factor(all_data$model, levels=c("IPSL-CM5A-LR", "GFDL-ESM2M", "MIROC5", "HadGEM2-ES"))
all_data$`tas(K).x` <- all_data$`tas(K).x`-273.15
all_data$`tas(K).y` <- all_data$`tas(K).y`-273.15

# Calculate deltaT from 1661-1860 piControl mean temperature
library(dplyr)
baseline_data <- all_data %>% group_by(model) %>% filter(scenario == "piControl", year >= 1661, year <= 1860) %>% summarise(baseline = mean(`tas(K).x`))
all_data <- left_join(all_data, baseline_data)
all_data$deltaT <- all_data$`tas(K).y` - all_data$baseline

all_data_long <- tidyr::gather(all_data, "var", "tas", -c(year, model, scenario, baseline))
all_data_long$var <- factor(all_data_long$var, labels=c("1"=paste("31-year running mean ", '\U0394', "T (?C)"), "2"="annual T (?C)", "3"="31-year running mean T (?C)"))
all_data_long$var <- factor(all_data_long$var, levels=levels(all_data_long$var)[c(2,3,1)])

library(ggplot2)
all_data_long %>% filter(var == "annual T (?C)") %>%
  ggplot(aes(x=year, y=tas, colour=scenario)) + geom_line() + 
  facet_grid(.~model, scales="free_y", switch="y") + labs(x="", y="") + 
  scale_x_continuous(breaks=c(1700, 1800, 1900, 2000, 2100, 2200), 
                     limits=c(1650, 2310), expand = c(0,0)) +
  ylab("Annual Temperature (C)") + 
  theme_bw() + theme(strip.background= element_blank(),
                     strip.placement= "outside",
                     legend.position = c(0.06,0.85),
                     legend.title = element_blank(),
                     legend.background = element_rect(fill = NA),
                     panel.spacing.x=unit(0.25, "lines"),
                     panel.spacing.y=unit(0.25, "lines"))
ggsave("annual_temp_gcm.png", width=12, height=5)
