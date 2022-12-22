
#There is data on the F1 website that I want to put into tables

#using this tutorial
#https://towardsdatascience.com/tidy-web-scraping-in-r-tutorial-and-resources-ac9f72b4fe47

library(rvest)
library(dplyr)
library(devtools)
library(xml2)
library(stringr)

f1_monza_pits_URL <- "https://www.formula1.com/en/results.html/2022/races/1120/italy/pit-stop-summary.html"
f1_monza_pits_page <- read_html(f1_monza_pits_URL)

pits <- f1_monza_pits_page %>% html_nodes("body") %>% html_text()
pss_start <- str_locate_all(pits, "PIT STOP SUMMARY")[[1]][1,1]

pits2 <- str_sub(pits, start = pss_start, end = str_length(pits))
pss_end <- str_locate(pits2, "Download")[1,1]

pits3 <- str_sub(pits2, start = 1, end = pss_end)

#there are a lot of newline and tab characters i need to remove
str_remove_all(pits3, "\\n") %>% str_remove_all("\\t")








this.tr <- wiki_page %>% html_nodes("body") %>% html_nodes("tr") %>%
  xml2::xml_find_all("//tr[contains(@class, 'vevent')]")


this.tr %>% html_nodes("th") %>% html_text()
#episode overall, this is just 1:40

this.td <- this.tr %>% html_nodes("td") %>% html_text() 
#episode of season
ep_of_season = this.td[seq(1, 160, 4)]
#episode title
episode_title = this.td[seq(2, 160, 4)]

air_date = this.td[seq(3, 160, 4)]

viewers = this.td[seq(4, 160, 4)]

eps <- data.frame(
  ep_of_series = 1:40,
  ep_of_season = as.numeric(ep_of_season),
  episode_title = str_remove_all(episode_title, "\""),
  air_date = str_sub(air_date, start = 1, end = -14),
  viewers = as.numeric(str_sub(viewers, start = 1, end = 4))
  
)

#write.csv(eps, "data/RHOSLC_episodes_v1.csv")

#~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|
# Scraping the imdb episode guides
#~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|

imdb_s1_URL <- "https://www.imdb.com/title/tt11363282/episodes?season=1&ref_=ttep_ep_sn_pv"
imdb_s2_URL <- "https://www.imdb.com/title/tt11363282/episodes?season=2&ref_=ttep_ep_sn_nx"

imdb_s1_page <- read_html(imdb_s1_URL)
imdb_s2_page <- read_html(imdb_s2_URL)

bn1 <- imdb_s1_page %>% html_nodes("body")
bn2 <- imdb_s2_page %>% html_nodes("body")

s1_ep_descriptions = bn1 %>% html_nodes("div") %>%
  xml_find_all("//div[contains(@class, 'item_description')]") %>%
  html_text() %>%
  as.character() %>%
  str_remove_all('\n') %>%
  str_remove_all('\  ')

s2_ep_descriptions = bn2 %>% html_nodes("div") %>%
  xml_find_all("//div[contains(@class, 'item_description')]") %>%
  html_text() %>%
  as.character() %>%
  str_remove_all('\n') %>%
  str_remove_all('\  ')

episode_descriptions = c(s1_ep_descriptions, s2_ep_descriptions)
episode_descriptions
class(episode_descriptions)

eps$episode_descriptions = episode_descriptions
#write.csv(eps, "data/RHOSLC_episodes_v1.csv")

#~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|
# 
#~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|~|=|









