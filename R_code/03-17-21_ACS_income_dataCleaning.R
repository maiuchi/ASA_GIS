#### ACS income data cleaning ####
#######################03-17-21###


library(dplyr)
library(tidyr)
library(readr)

# selet only the columns we need 
poverty <- read_csv("data/ACSST5Y2019.S1701_data_with_overlays_2021-03-17T214827.csv")[-1, c(1:4)]

colnames(poverty) <- c("AFFGEOID", "name", "pop_total", "pov_count")

poverty <- 
  poverty %>% 
  mutate(rate = (as.numeric(pov_count)/ as.numeric(pop_total)*100))

#write_csv(poverty, "ACS_poverty_CLN.csv")
