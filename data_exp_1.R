
#load packages
library(tidyverse)
library(devtools)
library(lubridate)

#dataviz code from my github
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

#setwd("~/Projects/GithubRepositories/F1_Data")

#names of tables in dataset:
tbl_names <- c("circuits", "constructor_results", "constructor_standings", 
                "constructors", "driver_standings", "drivers", "lap_times",
                "pit_stops", "qualifying", "races", "results", "seasons", 
                "sprint_results", "status")

#make into filepaths
tbl_names <- paste0("datasets/", tbl_names, ".csv")

#list to hold tables
tbl_list <- list(circuits, 
     constructor_results = NA,
     constructor_standings = NA,
     constructors = NA,
     driver_standings = NA,
     drivers = NA,
     lap_times = NA,
     pit_stops = NA,
     qualifying = NA,
     races = NA,
     results = NA,
     seasons = NA,
     sprint_results = NA,
     status = NA)



drv %>% pull(dob)

drv <- drv %>% filter(year(dob) > 1980)

drv <- drv %>% arrange(dob)

drv %>% pull(code) %>% unique()

keep_codes <- c("ALO", "HAM", "HUL", "RIC", "BOT", 
                "VER", "MAG", "GIO", "SAI", "DEV", 
                "GAS", "ALB", "FIT", "OCO", "LEC", 
                "RUS", "STR", "MSC", "ZHO", "NOR", 
                "TSU")

drv %>% filter(code %in% keep_codes) %>% arrange(month(dob))
