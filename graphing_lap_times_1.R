
#load packages
library(devtools)
library(lubridate)
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

best_laps <- read.csv("datasets/ndv_monza_best_laps.csv")

drivers <- read.csv("datasets/drivers.csv")

#want cumulative lap times
lt.ndv <- lap_times.ndv %>% arrange(lap) %>% 
  mutate(cum_ms = cumsum(milliseconds))

lt.mnz <- lap_times.mnz %>% group_by(driverId) %>% 
  arrange(driverId, lap) %>%
  mutate(cum_ms = cumsum(milliseconds)) %>% 
  left_join(drivers, by = "driverId")

#write.csv(lt.ndv, "datasets/ndv_lt_cum_ms.csv")
#write.csv(lt.mnz, "datasets/ndv_mnz_lt_cum_ms.csv")

lt.ndv <- read.csv("datasets/ndv_lt_cum_ms.csv")
lt.mnz <- read.csv("datasets/ndv_mnz_lt_cum_ms.csv")

#reference:
#https://www.formula1.com/en/latest/article.live-coverage-formula-1-pirelli-gran-premio-d-italia-2022.6pvMkOQrUQNnPE3LXxlP9H.html

#function for converting timestamp string into milliseconds (numeric)
time_to_milliseconds <- function(time_string){
  
  a <- as.numeric(str_sub(time_string, start = 1, end = 1))*60
  b <- as.numeric(str_sub(time_string, start = 3, end = 4))
  
  seconds = a + b + as.numeric(str_sub(time_string, start = 5, end = 8))
  
  seconds * 1000  
}

#function to convert milliseconds (numeric) to a timestamp string
milliseconds_to_time <- function(ms){
  
  #convert to seconds
  sec <- ms/1000
  #separate minutes and remaining seconds
  min <- floor(sec / 60)
  sec2 <- sec %% 60
  
  sec_wh <- str_pad(as.character(floor(sec2)), 2, side = "left", pad = "0")
  sec_fr <- as.character(round(sec2 %% 1, digits = 3))
  sec_fr <- str_pad(str_sub(sec_fr, start = 3, end = str_length(sec_fr)), 3, side = "right", pad = "0")
  
  paste0("", min, ":", sec_wh, ".", sec_fr)
  
}

#notes from watching the race again:

#lap 12 yellow flag (vettel stopped), virtual safety car 
#lap 19 nyck flagged for track limits
#lap 20 nyck pits, soft to med
#lap 39 ~ nyck in DRS train
#lap 40, nyck defends against zhou
#lap 44, nyck locks up, says something on team radio about his brake pedal
#lap 45, nyck let max through
#lap 47 ricciardo stopped, full safety car

#sometime after safety car, nyck hit the brakes, zhou says wtf on team radio
#this is the incident where nyck was supposedly driving erratically and had to
#talk to stewards after the race

#luck:
#nyck previously drove alex's car
#nyck was already at the gp that weekend
#alex got sick and required nyck to drive
#the numerous grid penalties that let nyck start in 8th position
#monza being an optimal track for the williams
#ricciardo retiring
#finishing under the safety car

class(lt.ndv$lap)
#creating a variable to easily filter laps where time is affected
lt.ndv <- lt.ndv %>% 
  mutate(lap_type = case_when(
    lap == 1 ~ "Starting Lap",
    lap == 12 ~ "Virtual Safety Car",
    lap == 19 ~ "Pit Stop In",
    lap == 20 ~ "Pit Stop Out",
    lap >= 47 ~ "Safety Car",
    TRUE ~ "Regular"
  ))


monza_theme <- theme(
  
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
  plot.margin = margin(10,30,20,10),
  plot.tag = element_text(color = "black"),
  plot.tag.position = c(0.95,1),
  
  strip.background = element_rect(fill = "gray90", color = "gray70", size = 1),
  strip.text = element_text(color = oj["blueK"]),
  
  complete = FALSE,
  validate = TRUE
)

time_labels <- c("0:00.0",
                 "0:15.0",
                 "0:30.0",
                 "0:45.0",
                 "1:00.0",
                 "1:15.0",
                 "1:30.0",
                 "1:45.0",
                 "2:00.0",
                 "2:15.0")

time_label_x_values <- c(time_to_milliseconds("0:00.000"),
                         time_to_milliseconds("0:15.000"),
                         time_to_milliseconds("0:30.000"),
                         time_to_milliseconds("0:45.000"),
                         time_to_milliseconds("1:00.000"),
                         time_to_milliseconds("1:15.000"),
                         time_to_milliseconds("1:30.000"),
                         time_to_milliseconds("1:45.000"),
                         time_to_milliseconds("2:00.000"),
                         time_to_milliseconds("2:15.000"))



ggplot(lt.ndv) + monza_theme +
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
  #vsc label
  annotate("text", x = lap_virtual_safety_car$milliseconds + 2500, 
           y = 12, color = oj["blue5"], 
           label = "Virtual Safety Car", hjust = 0) + 
  
  
  #track limits warning horizontal line segment
  annotate("segment", x = 0, xend = lt.ndv$milliseconds[which(lt.ndv$lap == 19)],
           y = 19, yend = 19, color = oj["blue3"]) + 
  
  
  
  #pit stop horizontal line segment
  annotate("segment", x = 0, xend = lt.ndv$milliseconds[which(lt.ndv$lap == 20)],
           y = 20, yend = 20, color = oj["blue3"]) + 
  #pit stop lap label
  annotate("text", 
           x = lap_pit_stop$milliseconds + 2500, 
           y = 20, 
           label = paste0("Pit Stop: ", pit_stops.ndv$duration), 
           hjust = 0, 
           color = oj["orange5"]) +
  
  
  #Safety car horizontal line segments
  annotate("segment", x = rep.int(0, length(47:53)), 
           xend = lt.ndv$milliseconds[which(lt.ndv$lap %in% 47:53)],
           y = 47:53, yend = 47:53, color = oj["blue2"]) +
  #Safety car label
  annotate("text", x = lt.ndv$milliseconds[which(lt.ndv$lap == 50)] + 2500,
           y = 50, label = "Safety Car",
           hjust = 0) +
  
  #Fastest lap label
  annotate("text", 
           x = fl$milliseconds + 2500, 
           y = 41, 
           label = paste0("Fastest Lap: ", fl$time), 
           hjust = 0, 
           color = oj["orange5"]) + 

  #vertical time markers
  geom_vline(xintercept = time_label_x_values, color = oj["blue1"]) + 
  #bottom axis
  geom_hline(yintercept = 55, color = oj["blue5"]) + 
  annotate("text", x = time_label_x_values + 1000, y = rep.int(57,10), 
           label = time_labels, 
           color = "black",
           hjust = 0, size = 3)



#detail of lap times, regular laps only

#range of times in regular laps:
summary(lt.ndv %>% filter(lap_type == "Regular") %>% pull(milliseconds))
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#86624   87019   87326   87328   87535   88416 

milliseconds_to_time(86624)
#"1:26.624"
milliseconds_to_time(88416)
#"1:28.416"

time_labels <- c("1:26.500",
                 "1:26.750",
                 "1:27.000",
                 "1:27.250",
                 "1:27.500",
                 "1:27.750",
                 "1:28.000",
                 "1:28.250",
                 "1:28.500")

time_label_x_values <- c(time_to_milliseconds("1:26.500"),
                         time_to_milliseconds("1:26.750"),
                         time_to_milliseconds("1:27.000"),
                         time_to_milliseconds("1:27.250"),
                         time_to_milliseconds("1:27.500"),
                         time_to_milliseconds("1:27.750"),
                         time_to_milliseconds("1:28.000"),
                         time_to_milliseconds("1:28.250"),
                         time_to_milliseconds("1:28.500"))

fl <- lt.ndv %>% filter(lap == 41)

#changing non-regular laps to NA values in new column to
#make line breaks in graph
lt.ndv <- lt.ndv %>% 
  mutate(regular_milliseconds = ifelse(
    lap_type == "Regular",
    milliseconds,
    NA
  ))

#adding a tire type variable to add color to lines
lt.ndv <- lt.ndv %>% 
  mutate(tire_type = ifelse(lap > 19, "medium", "soft"))


monza_dk_theme <- theme(
  
  line = element_line(lineend = "square"),
  rect = element_rect(fill = jmbn["hunter"]),
  text = element_text(family = "sans", color = oj["blue1"], size = 3, hjust = 0.5, vjust = 0.5),
  title = element_text(family = "sans", color = oj["blue1"], size = 10, hjust = 0.5, vjust = 0),
  
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  legend.position = "none",
  
  panel.background = element_rect(fill = jmbn["hunter"]),
  panel.border = element_blank(),#element_rect(fill = NA, color = "gray70", size = 1),
  panel.spacing = unit(10, "pt"),
  panel.grid.major = element_blank(),#element_line(color = "gray90", size = 0.5),
  panel.grid.minor = element_blank(),#element_line(color = "white", size = 0.5),
  
  plot.background = element_rect(fill = jmbn["hunter"]),
  plot.title = element_text(family = "sans", face = "bold", size = 18, color = "white", hjust = 0.5),
  plot.subtitle = element_text(family = "sans", face = "italic", size = 12, color = "gray85", hjust = 0.5),
  #plot.caption = element_text(),
  plot.margin = margin(10,30,20,10),
  plot.tag = element_text(color = "white"),
  plot.tag.position = c(0.95,1),
  
  strip.background = element_rect(fill = "gray90", color = "gray70", size = 1),
  strip.text = element_text(color = oj["blueK"]),
  
  complete = FALSE,
  validate = TRUE
)

monza_subtitle <- "Monza Circuit, Gran Premio D'Italia, 9/11/22"
my_f1_caption <- "Jan Moffett | github.com/JanLMoffett | Data Source: Ergast API"

special_lap_label_x = 88650

ggplot(lt.ndv) + monza_dk_theme +
  coord_cartesian(xlim = c(time_to_milliseconds("1:26.4"), time_to_milliseconds("1:28.6")),
                  ylim = c(56, 1),
                  expand = 1) +
  labs(title = "Nyck De Vries Lap Times (Detail)",
       subtitle = monza_subtitle,
       caption = my_f1_caption) + 
  
  #vertical time markers
  geom_vline(xintercept = time_label_x_values, color = jmbn["turquoise"], alpha = 0.5) +
  
  #horizontal lap markers
  geom_hline(yintercept = 1:53, color = jmbn["turf"]) +
  #special lap markers
  geom_hline(yintercept = 1, color = "grey", linetype = 3) +
  geom_hline(yintercept = c(12,47:53), color = jmbn["rose"], linetype = 3) +
  geom_hline(yintercept = 19:20, color = jmbn["periwinkle"], linetype = 3) +
  
  #special lap labels 
  annotate("text", x = special_lap_label_x, y = 1, color = "grey",
           label = "Starting Lap", hjust = 1, size = 3.5) + 
  annotate("text", x = special_lap_label_x, y = 12, color = jmbn["rose"],
           label = "Virtual Safety Car", hjust = 1, size = 3.5) +
  annotate("text", x = rep.int(special_lap_label_x, 2), y = c(19,20),
           color = jmbn["periwinkle"], label = c("Pit In", "Pit Out"),
           hjust = 1, size = 3.5) +
  annotate("text", x = special_lap_label_x, y = 41, color = jmbn["highlighter"],
           label = "Fastest Lap", hjust = 1, size = 3.5) +
  annotate("text", x = special_lap_label_x, y = 47, color = jmbn["rose"],
           label = "Safety Car", hjust = 1, size = 3.5) +  
  
  #rec under lap number labels
  annotate("rect", xmin = 86350, xmax = 86500,
           ymin = -2, ymax = 54, fill = jmbn["hunter"], alpha = 0.5) + 
  
  #"Lap"
  annotate("text", x = 86455, y = -1, label = "Lap", 
           hjust = 1, color = jmbn["turquoise"], size = 3.5) +
  #lap number labels
  annotate("text", x = rep.int(86455, 27), y = seq(1,53,2), 
           label = seq(1,53,2), hjust = 1, color = "white", size = 3.5) + 
  
  #bottom axis
  geom_hline(yintercept = 55, color = jmbn["turquoise"]) + 
  annotate("text", x = time_label_x_values, y = rep.int(57,9), 
           label = time_labels, 
           color = "white",
           hjust = 0, size = 2.8) + 
  
  #left axis
  geom_vline(xintercept = 86500, color = jmbn["turquoise"]) +
  
  #lap time points
  geom_point(aes(x = regular_milliseconds, y = lap, color = tire_type)) + 
  #lap time lines
  geom_line(aes(x = regular_milliseconds, y = lap, color = tire_type), 
            orientation = "y") + 
  scale_color_manual(values = c("soft" = "red", "medium" = "yellow")) + 
  #Fastest lap marker
  geom_hline(yintercept = 41, color = jmbn["highlighter"], linetype = 2) +
  geom_vline(xintercept = fl$milliseconds, 
             color = jmbn["highlighter"], 
             linetype = 2)


names(lt.mnz)

#[1] "raceId"       "driverId"     "lap"          "position"    
#[5] "time"         "milliseconds" "cum_ms"       "driverRef"   
#[9] "number"       "code"         "forename"     "surname"     
#[13] "dob"          "nationality"  "url"          "timestamp"

#want to make a plot where each driver's whole race is one horizontal line,
#with vertical markers for each lap

#chronological timestamps for end of each lap
lt.mnz <- lt.mnz %>% select(-all_of(c("X.1", "X"))) %>%
  mutate(timestamp = milliseconds_to_time(cum_ms))
  
#need a table of drivers in the race
dr.mnz <- lt.mnz %>% group_by(driverId) %>%
  summarize(
    
    driver_code = first(code),
    driver_name = paste0(first(forename), " ", first(surname)),
    total_milliseconds = last(cum_ms),
    finish_timestamp = last(timestamp)
    )

z <- lt.mnz %>% select(driverId, lap, milliseconds)
z <- pivot_wider(z, names_from = lap, names_prefix = "lap_", values_from = milliseconds)

dr.mnz <- dr.mnz %>% left_join(z, by = "driverId") %>%
  left_join(results.mnz, by = "driverId")

dr.mnz <- dr.mnz %>% arrange(positionOrder)
dr.mnz <- dr.mnz %>% mutate(finish_position = as.numeric(position))
dr.mnz <- dr.mnz %>% mutate(finish_position2 = ifelse(is.na(finish_position), "DNF", as.character(finish_position)))
  
names(dr.mnz)
#[1] "driverId"           "driver_code"        "driver_name"       
#[4] "total_milliseconds" "finish_timestamp"   "lap_1" ... 
#[57] "lap_52"             "lap_53"             "X"                  "resultId"          
#[61] "raceId"             "constructorId"      "number"             "grid"              
#[65] "position"           "positionText"       "positionOrder"      "points"            
#[69] "laps"               "time"               "milliseconds"       "fastestLap"        
#[73] "rank"               "fastestLapTime"     "fastestLapSpeed"    "statusId"

ggplot(dr.mnz) + #monza_dk_theme +
  coord_cartesian(xlim = c(-300000, 5000000), ylim = c(20,1)) +
  #horizontal race timelines
  geom_segment(aes(x = 0, xend = total_milliseconds, 
                   y = positionOrder, yend = positionOrder)) + 
  #vertical lap time markers
  geom_segment(aes(x = lap_1, xend = lap_1,
                   y = positionOrder-.25, yend = positionOrder +0.25)) +
  #driver code labels
  annotate("text", x = -2500, y = dr.mnz$positionOrder, hjust = 1,
           label = paste0(dr.mnz$finish_position2, " ", dr.mnz$driver_code),
           size = 3)

#this isn't quite how the data should be arranged

names(lt.mnz)
#i just need to join results to lap times
lt2 <- left_join(lt.mnz, results.mnz, by = "driverId")

names(lt2)
lt2 <- lt2 %>% rename(raceId = raceId.x,
                      lap_position = position.x,
                      lap_time = time.x,
                      lap_milliseconds = milliseconds.x,
                      driver_number = number.x,
                      finish_position = position.y,
                      finish_order = positionOrder,
                      total_laps = laps,
                      finish_time = time.y,
                      finish_milliseconds = milliseconds.y,
                      fastest_lap_rank = rank) %>%
  select(-all_of(c("X", "raceId.y", "number.y")))

#assign colors for each team/driver
team_colors <- c(
  alp_pink <- hex("#ff85dc"),
  ryl_blue <- hex("#0000FF"),
  rb_navy <- hex("#001861"),
  rb_orange <- hex("#ff4400"),
  mer_cyan <- hex("#00ffff"),
  mer_slvr <- hex("#a6a6a6"),
  fer_red <- hex("#e80000"),
  fer_ylw <- hex("#e6ff00"),
  mcl_org <- hex("#ffa200"),
  ast_grn <- hex("#00a5a8"))

names(lt2)

lt2 <- lt2 %>% mutate(
  tm_color_1 = case_when(
    constructorId == 131 ~ mer_slvr,
    constructorId == 214 ~ alp_pink,
    constructorId == 117 ~ ast_grn,
    constructorId == 9 ~ rb_navy,
    constructorId == 1 ~ mcl_org,
    constructorId == 51 ~ "white",
    constructorId == 210 ~ "white",
    constructorId == 6 ~ fer_red,
    constructorId == 213 ~ rb_navy,
    constructorId == 3 ~ ryl_blue,
    TRUE ~ "white")) %>%
  mutate(
    tm_color_2 = case_when(
      constructorId == 131 ~ mer_cyan,
      constructorId == 214 ~ ryl_blue,
      constructorId == 117 ~ "black",
      constructorId == 9 ~ rb_navy,
      constructorId == 1 ~ "white",
      constructorId == 51 ~ "black",
      constructorId == 210 ~ fer_red,
      constructorId == 6 ~ fer_ylw,
      constructorId == 213 ~ "white",
      constructorId == 3 ~ mer_cyan,
      TRUE ~ "black"
    ))

tm_color_1 = c(
  "131" = mer_slvr,
  "214" = alp_pink,
  "117" = ast_grn,
  "9" = rb_navy,
  "1" = mcl_org,
  "51" = "white",
  "210" = "white",
  "6" = fer_red,
  "213" = rb_navy,
  "3" = ryl_blue)

tm_color_2 = c(
  "131" = mer_cyan,
  "214" = ryl_blue,
  "117" = "black",
  "9" = rb_orange,
  "1" = "white",
  "51" = "black",
  "210" = fer_red,
  "6" = fer_ylw,
  "213" = "white",
  "3" = mer_cyan)

ggplot(lt2) + monza_dk_theme + 
  coord_cartesian(xlim = c(0,54), ylim = c(21,0), expand = 0) +
  
  geom_rect(aes(xmin = lap - 0.5, xmax = lap + 0.5,
                ymin = lap_position - 0.5, ymax = lap_position + 0.5,
                fill = factor(constructorId),
                color = factor(constructorId))) + 
  scale_fill_manual(values = tm_color_1) + 
  scale_color_manual(values = tm_color_2) + 
  geom_text(aes(x = lap, 
                y = lap_position, 
                label = driver_number,
                color = factor(constructorId)),
            size = 2)



