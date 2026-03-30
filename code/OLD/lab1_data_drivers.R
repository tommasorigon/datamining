# Additional information

# Source of the data: https://dati.mit.gov.it/hfs/patenti_Lombardia.csv
# Documentation: http://dati.mit.gov.it/catalog/dataset/patenti

# Author:	Direzione generale per la motorizzazione - Div7 - Centro elaborazione dati motorizzazione
# Last update:	21 December 2022, 17:16 (UTC+01:00)
# Created:	20 February 2022, 18:21 (UTC+01:00)
# Temporal extension (end)	31 December 2019

library(readr)
library(tidyverse)
drivers <- read_csv("https://dati.mit.gov.it/hfs/patenti_Lombardia.csv")

# Change the name of the columns
colnames(drivers) <- c("id", "birth", "town", "city", "region", "state", "gender", "category", "date", "habilit", "date_habilit", "expiration", "date_foreigners", "points")

# Change the format of the date
drivers <- drivers %>% mutate(experience = 2019 - year(date), 
                              age = 2019 - birth)

# Select patent B and other things
drivers <- drivers %>%
  filter(category == "B", is.na(state), !is.na(points)) %>%
  filter(experience > 0)

# Remove irrelevant columns
drivers <- drivers %>% select(-c(id, category, state, date_foreigners, expiration, date_habilit, birth, date, region))
drivers <- drivers %>% mutate(hazard = sqrt(-(points - 30)))
drivers <- na.omit(drivers)

glimpse(drivers)
summary(drivers)
write_csv(drivers, "../data/drivers2.csv")