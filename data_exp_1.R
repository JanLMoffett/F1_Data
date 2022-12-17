
#load packages
library(devtools)
library(lubridate)

library(maps)
library(mapdata)
library(usmap)
library(rworldmap)
library(sf)

library(tidyverse)

#dataviz code from my github
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#setwd("~/Projects/GithubRepositories/F1_Data")

#write.csv(lap_times.ndv, "datasets/ndv_lap_times.csv")
#write.csv(drivers.ndv, "datasets/ndv_drivers.csv")
#write.csv(pit_stops.ndv, "datasets/ndv_pit_stops.csv")
#write.csv(qualifying.ndv, "datasets/ndv_qualifying.csv")
#write.csv(circuits.mnz, "datasets/ndv_circuits.csv")
#write.csv(results.ndv, "datasets/ndv_results.csv")
#write.csv(results.mnz, "datasets/ndv_monza_results.csv")
#write.csv(qualifying.mnz, "datasets/ndv_monza_qualifying.csv")