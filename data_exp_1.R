
#load packages
library(tidyverse)
library(devtools)
library(lubridate)

library(maps)
library(mapdata)
library(usmap)
library(rworldmap)
library(sf)

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
tbl_paths <- paste0("datasets/", tbl_names, ".csv")

#list to hold tables
tbl_list <- list()
#read in data tables and save in list
tbl_list <- sapply(tbl_paths, read.csv)

#make table of metadata for data tables, to get an overview of the variables available 
#and also which variables are primary keys
names(tbl_list) <- tbl_names




