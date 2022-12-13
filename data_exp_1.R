
library(baseballr)
library(tidyverse)
library(maps)
library(mapdata)
library(usmap)
library(rworldmap)
library(sf)
library(devtools)
library(lubridate)

source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#setwd("~/Projects/GithubRepositories/F1_Data")

drv <- read.csv("datasets/drivers.csv")
View(drv)

drv %>% pull(dob)

drv <- drv %>% filter(year(dob) > 1980)

drv <- drv %>% arrange(dob)

drv %>% pull(code)

keep_codes <- c("ALO", "HAM", "HUL", "RIC", "BOT", 
                "VER", "MAG", "GIO", "SAI", "DEV", 
                "GAS", "ALB", "FIT", "OCO", "LEC", 
                "RUS", "STR", "MSC", "ZHO", "NOR", 
                "TSU")

drv %>% filter(code %in% keep_codes) %>% arrange(month(dob))
