
#load packages
library(devtools)
library(lubridate)
library(tidyverse)

#setwd("~/Projects/GithubRepositories/F1_Data")

#data from the ergast api via kaggle
# # # # # # # # # # # # # # # # # # #
 # # # # # # # # # # # # # # # # # # #

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
newlist <- tbl_list

for(i in seq_along(tbl_names)){
  
  x <- tbl_list[[i]]
  
  #variables in table
  x.df <- x %>% summarize(across(.cols = everything(), .fns = class)) %>%
    t() %>% data.frame()
  names(x.df) <- "datatype"
  
  x.df$var_name <- names(x)
  
  x2 <- x %>% summarize(across(.cols = everything(), .fns = n_distinct)) %>%
    t() %>% data.frame()
  
  x.df$unq_values <- x2[[1]]
  
  x.df <- x.df %>% mutate(table_name = names(tbl_list)[[i]])
  
  newlist[[i]] <- x.df
  
}

names(newlist) <- tbl_names

#now i have a list of metadata tables for each table

tbl_names
newlist[["circuits"]]
newlist[["driver_standings"]]
newlist[["drivers"]]
newlist[["lap_times"]]
newlist[["pit_stops"]]
newlist[["qualifying"]]
newlist[["races"]]
newlist[["results"]]

#i'm making a christmas present for nyck using his data from monza
#want to look for nyck's results first

circuits.mnz <- tbl_list[["circuits"]] %>% filter(location == "Monza")

drivers.ndv <- tbl_list[["drivers"]] %>% filter(str_detect(surname, "Vries"))
t(drivers.ndv)
#nyck's driverId = 856

lap_times.ndv <- tbl_list[["lap_times"]] %>% filter(driverId == "856")
#the raceId for Monza = 1089

pit_stops.ndv <- tbl_list[["pit_stops"]] %>% filter(driverId == "856")
#he just had one

qualifying.ndv <- tbl_list[["qualifying"]] %>% filter(driverId == "856")

results.ndv <- tbl_list[["results"]] %>% filter(driverId == "856")

races.mnz <- tbl_list[["races"]] %>% filter(raceId == "1089")
results.mnz <- tbl_list[["results"]] %>% filter(raceId == "1089")
quali.mnz <- tbl_list[["qualifying"]] %>% filter(raceId == "1089")

#everyone's lap times
lap_times.mnz <- tbl_list[["lap_times"]] %>% filter(raceId == "1089")

#write.csv(lap_times.ndv, "datasets/ndv_lap_times.csv")
#write.csv(drivers.ndv, "datasets/ndv_drivers.csv")
#write.csv(pit_stops.ndv, "datasets/ndv_pit_stops.csv")
#write.csv(qualifying.ndv, "datasets/ndv_qualifying.csv")
#write.csv(circuits.mnz, "datasets/ndv_circuits.csv")
#write.csv(results.ndv, "datasets/ndv_results.csv")
#write.csv(results.mnz, "datasets/ndv_monza_results.csv")
#write.csv(qualifying.mnz, "datasets/ndv_monza_qualifying.csv")
#write.csv(lap_times.mnz, "datasets/ndv_monza_lap_times.csv")


#each driver's best lap at Monza, ranked
drivers <- read.csv("datasets/drivers.csv")

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



