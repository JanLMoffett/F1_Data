
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
best_laps <- read.csv("datasets/ndv_monza_best_laps.csv")

lt.ndv <- lap_times.ndv %>% arrange(lap)

fl <- lap_times.ndv %>% filter(lap == 41)

#reference:
#https://www.formula1.com/en/latest/article.live-coverage-formula-1-pirelli-gran-premio-d-italia-2022.6pvMkOQrUQNnPE3LXxlP9H.html

#new theme to use for these plots

#theme(axis.text = element_blank(),
#      axis.ticks = element_blank(),
#      axis.title = element_blank())
#----

time_string = "1:26.624"
time_to_milliseconds <- function(time_string){
  
  a <- as.numeric(str_sub(time_string, start = 1, end = 1))*60
  b <- as.numeric(str_sub(time_string, start = 3, end = 4))
  
  seconds = a + b + as.numeric(str_sub(time_string, start = 5, end = 8))
  
  seconds * 1000  
  
  
}

millisecs = 86624
millisecs = 25000
milliseconds_to_time <- function(millisecs){
  
  min <- floor(millisecs/60/1000)
  sec <- round((millisecs/60/1000) - min, digits = 3)
  
  paste0("", min, ":", str_pad(sec, width = 6, pad = '0', side = "left"))
  
}

#lap 12 yellow flag (vettel stopped), virtual safety car 
#lap 19 nyck got flagged for track limit
#lap 20 nyck pits, soft to med
#lap 40, nyck defends again zhou
#lap 44, nyck locks up
#lap 45, nyck let max through
#lap 47 ricciardo stopped, full safety car

fl
time_to_milliseconds("1:26.624")
milliseconds_to_time(86624)
milliseconds_to_time(25000)

monza_theme2 <- theme(
  
  line = element_line(lineend = "square"),
  rect = element_rect(fill = "white"),
  text = element_text(family = "sans", color = oj["blueK"], size = 12, hjust = 0.5, vjust = 0.5),
  title = element_text(family = "sans", color = "black", size = 12, hjust = 0.5, vjust = 0),
  
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  panel.background = element_rect(fill = "white"),
  panel.border = element_blank(),#element_rect(fill = NA, color = "gray70", size = 1),
  panel.spacing = unit(10, "pt"),
  panel.grid.major = element_blank(),#element_line(color = "gray90", size = 0.5),
  panel.grid.minor = element_blank(),#element_line(color = "white", size = 0.5),
  
  plot.background = element_rect(fill = "white"),
  plot.title = element_text(family = "sans", face = "bold", size = 18, color = "black", hjust = 0.5),
  plot.subtitle = element_text(family = "sans", face = "italic", size = 12, color = "gray15", hjust = 0.5),
  #plot.caption = element_text(),
  plot.margin = margin(10,10,40,10),
  plot.tag = element_text(color = "black"),
  plot.tag.position = c(0.95,1),
  
  strip.background = element_rect(fill = "gray90", color = "gray70", size = 1),
  strip.text = element_text(color = oj["blueK"]),
  
  complete = FALSE,
  validate = TRUE
)

time_labels <- c("0:00.000",
                 "0:15.000",
                 "0:30.000",
                 "0:45.000",
                 "1:00.000",
                 "1:15.000",
                 "1:30.000",
                 "1:45.000",
                 "2:00.000",
                 "2:15.000",
                 "2:30.000")

time_label_x_values <- c(time_to_milliseconds("0:00.000"),
                         time_to_milliseconds("0:15.000"),
                         time_to_milliseconds("0:30.000"),
                         time_to_milliseconds("0:45.000"),
                         time_to_milliseconds("1:00.000"),
                         time_to_milliseconds("1:15.000"),
                         time_to_milliseconds("1:30.000"),
                         time_to_milliseconds("1:45.000"),
                         time_to_milliseconds("2:00.000"),
                         time_to_milliseconds("2:15.000"),
                         time_to_milliseconds("2:30.000"))

ggplot(lt.ndv) + monza_theme2 +
  geom_segment(aes(x = 0, xend = milliseconds,
                   y = lap, yend = lap)) + 
  
  coord_cartesian(xlim = c(-10000, time_to_milliseconds("2:30.000")),
                  ylim = c(60, 1),
                  expand = 1) + 
  
  #"Lap"
  annotate("text", x = -3000, y = -1, hjust = 1, label = "Lap") +
  
  #lap ticks
  annotate("segment", y = seq(1,53,2), yend = seq(1,53,2), 
           x = -1800, xend = 0, color = oj["blue5"]) + 
  #lap labels
  annotate("text", x = rep.int(-3000, 27), y = seq(1,53,2), label = seq(1,53,2), hjust = 1) + 
  
  #Fastest lap horizontal line segment
  annotate("segment", x = 0, 
           xend = lt.ndv %>% filter(lap == 41) %>% pull(milliseconds),
           y = 41, yend = 41, color = oj["orange5"]) +
  #Fastest lap vertical line marker
  geom_vline(xintercept = fl$milliseconds, 
             color = oj["orange5"], 
             linetype = 1) + 
  #Fastest lap label
  annotate("text", x = fl$milliseconds + 2500, y = 41, 
           label = paste0("Fastest Lap: ", fl$time), hjust = 0, 
           color = oj["orange5"]) + 
  
  #virtual safety car horizontal line segment
  annotate("segment", x = 0, xend = lt.ndv$milliseconds[which(lt.ndv$lap == 12)],
           y = 12, yend = 12, color = oj["blue3"]) + 
  
  
  #track limits warning horizontal line segment
  annotate("segment", x = 0, xend = lt.ndv$milliseconds[which(lt.ndv$lap == 20)],
           y = 12, yend = 12, color = oj["blue3"]) + 
  
  
  #pit stop horizontal line segment
  annotate("segment", x = 0, xend = lt.ndv$milliseconds[which(lt.ndv$lap == 21)],
           y = 12, yend = 12, color = oj["blue3"]) + 
  
  
  #Safety car horizontal line segment
  annotate("segment", x = rep.int(0, length(47:53)), 
           xend = lt.ndv$milliseconds[which(lt.ndv$lap %in% 47:53)],
           y = 47:53, yend = 47:53, color = oj["blue2"]) +
  #Fastest lap vertical line marker
  geom_vline(xintercept = fl$milliseconds, 
             color = oj["orange5"], 
             linetype = 1) + 
  #Fastest lap label
  annotate("text", x = fl$milliseconds + 2500, y = 41, 
           label = paste0("Fastest Lap: ", fl$time), hjust = 0, 
           color = oj["orange5"]) + 

  #vertical time markers
  geom_vline(xintercept = time_label_x_values, color = oj["blue5"]) + 
  #bottom axis
  geom_hline(yintercept = 55, color = oj["blue5"]) + 
  annotate("text", x = time_label_x_values + 1000, y = rep.int(57,11), 
           label = time_labels, 
           color = "black",
           hjust = 0, size = 3)



                  