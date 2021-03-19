######## COVID Mapping ########## ----- Data creation for demo 
#####################03-18-21####

library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)

# load dataset
covid <- read_csv("data/us-counties.csv")

covid_selected <- 
  covid %>%
  filter(date == "2021-03-14")

#write_csv(covid_selected, "data/covid_selected.csv")


us_counties <- read_csv("data/census_us_counties_check.csv")

colnames(covid_selected)[4] <- "GEOID"

county_check <- full_join(covid_selected, us_counties, by = "GEOID")

unknown <- covid_selected[which(covid_selected$county == "Unknown"),]  # 26 obs 

duplicated <- covid_selected[duplicated(covid_selected$GEOID),]

############
# NYC Data #
############

nyc_covid <- read_csv("data/nyc_covid_zip.csv")

nyc_boroughs_covid <-  
  nyc_covid %>%  
  group_by(BOROUGH_GROUP) %>% 
  summarise(cases = sum(COVID_CASE_COUNT), deaths = sum(COVID_DEATH_COUNT))

nyc_boroughs_covid$GEOID <- ifelse(nyc_boroughs_covid$BOROUGH_GROUP == "Bronx", "36005",
                                   ifelse(nyc_boroughs_covid$BOROUGH_GROUP == "Brooklyn", "36047",
                                   ifelse(nyc_boroughs_covid$BOROUGH_GROUP == "Manhattan", "36061",
                                   ifelse(nyc_boroughs_covid$BOROUGH_GROUP == "Staten Island", "36085",
                                   ifelse(nyc_boroughs_covid$BOROUGH_GROUP == "Queens", "36081", NA)))))

nyc_boroughs_covid$date <- as.Date("2021-03-14")
nyc_boroughs_covid$state <- paste("New York")

colnames(nyc_boroughs_covid)[1] <- "county"

# reorder columns 
nyc_boroughs_covid <- nyc_boroughs_covid[, c(5, 1, 6, 4, 2, 3)]

# combine two datasets
covid_selected <- rbind(nyc_boroughs_covid, covid_selected) # 3252 counties

covid_selected <- covid_selected[!duplicated(covid_selected$GEOID),] # 3224 counties

#####################
# Joplin and Kansas # 
#####################

newton_county <- 
  duplicated %>% 
  filter(county == "Joplin") %>% 
  mutate(cases = floor(cases/2), deaths = floor(deaths/2)) 

newton_ms <- 
  covid_selected %>% 
  filter(county == "Newton" & state == "Missouri")  

newton_ms <- cbind(newton_ms[c(1, 2, 3, 4)], newton_ms[5] + newton_county[5], newton_ms[6] + newton_county[6])
  
jasper_county <- 
  duplicated %>% 
  filter(county == "Joplin") %>% 
  mutate(cases = floor(cases/2), deaths = floor(deaths/2))

jasper_ms <- 
  covid_selected %>% 
  filter(county == "Jasper" & state == "Missouri")

jasper_ms <- cbind(jasper_ms[c(1, 2, 3, 4)], jasper_ms[5] + jasper_county[5], jasper_ms[6] + jasper_county[6])

###############
# Kansas City #
###############

jackson_county <- 
  duplicated %>% 
  filter(county == "Kansas City") %>% 
  mutate(cases = floor(cases/2), deaths = floor(deaths/2))

jackson_ms <- 
  covid_selected %>% 
  filter(county == "Jackson" & state == "Missouri")

jakson_ms <- cbind(jackson_ms[c(1, 2, 3, 4)], jackson_ms[5] + jackson_county[5], jackson_ms[6] + jackson_county[6])

covid_selected <- rbind(newton_ms, jasper_ms, jackson_ms, covid_selected)

covid_selected <- covid_selected[!duplicated(covid_selected$GEOID),]

#write_csv(covid_selected, "covid_selected_CLN.csv")


