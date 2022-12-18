
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

lap_times.ndv <- read.csv("datasets/ndv_lap_times.csv")
drivers.ndv <- read.csv("datasets/ndv_drivers.csv")
pit_stops.ndv <- read.csv("datasets/ndv_pit_stops.csv")
qualifying.ndv <- read.csv("datasets/ndv_qualifying.csv")
results.ndv <- read.csv("datasets/ndv_results.csv")

results.mnz <- read.csv("datasets/ndv_monza_results.csv")
qualifying.mnz <- read.csv("datasets/ndv_monza_qualifying.csv")
lap_times.mnz <- read.csv("datasets/ndv_monza_lap_times.csv")
circuits.mnz <- read.csv("datasets/ndv_circuits.csv")

drivers <- tbl_list[["drivers"]]

names(lap_times.mnz)
lt <- left_join(lap_times.mnz, drivers, by = "driverId")

names(lt)
#[1] "X"            "raceId"       "driverId"     "lap"         
#[5] "position"     "time"         "milliseconds" "driverRef"   
#[9] "number"       "code"         "forename"     "surname"     
#[13] "dob"          "nationality"  "url"

#each driver's best lap in the race, sorted
best_laps <- lt %>% group_by(driverId) %>% arrange(driverId, milliseconds) %>%
  slice_head(n = 1) %>% arrange(milliseconds)

#write.csv(best_laps, "datasets/ndv_monza_best_laps.csv")

lt.ndv <- lap_times.ndv %>% arrange(milliseconds)

fl <- lap_times.ndv %>% filter(lap == 41)

ggplot(lt.ndv) + juiceGlass +
  geom_segment(aes(x = 0, xend = milliseconds,
                   y = lap, yend = lap)) + 
  coord_cartesian(xlim = c(75000, 125000),
                  ylim = c(53, 1)) + 
  labs(title = "Nyck De Vries Lap Times in Chronological Order",
       x = "Milliseconds") + 
  annotate("segment", x = 0, 
           xend = lt.ndv %>% filter(lap == 41) %>% pull(milliseconds),
           y = 41, yend = 41, color = oj["orange3"]) + 
  annotate("text", x = fl$milliseconds + 200, y = 41, label = paste0("Fastest Lap ", fl$time))

                  