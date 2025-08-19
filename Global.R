library(tidyverse)
library(readxl)
library(janitor)
library(bslib)
library(ggthemes)
library(lubridate)
library(thematic)
library(plotly)
library(patchwork)
library(scales)
library(DT)
library(gt)
library(gtExtras)
library(withr)
library(ggpp)
library(calendR)
library(ggimage)
library(keyring)
library(digest)


####
#FORCE PLATES ----
####

#Load and wrangle Force Plate Data - currently coming from excel
FPDatabase_full <- read_excel("sample_data/FP-Database.xlsx",
                              sheet = "Database",
                              range = cell_cols("A:BQ"),
                              na = c("","N/A","NaN")
) %>%
  clean_names() %>%
  mutate(date_ddmmyear = as.Date(date_ddmmyear),
         date_ddmmyear2 =  (format(date_ddmmyear, "%b %d, %Y")))


#Recent Five ----
#Select athlete name and jump type to create a dataframe of their last five test dates
recent_five <- function(athlete1, activity1) {
  FPDatabase_full %>%
    filter(athlete == athlete1, activity == activity1) %>%
    arrange(desc(date_ddmmyear)) %>%
    filter(date_ddmmyear %in% unique(date_ddmmyear)[1:5])
}


####
#REHAB DETAILS ----
####

Rehab_Info <- read_excel(
  "sample_data/ACL-Intake-Form.xlsx"
) %>% clean_names()

#Show the day number of the rehab, calculated as days since surgery
Days <- as.numeric(difftime(today(), Rehab_Info$date_of_surgery, units = "days"))
injury_date <- Rehab_Info$date_of_injury
surgery_date <- wday(Rehab_Info$date_of_surgery, label = TRUE) %>% as.character()










####
#WELLNESS MONITORING ----
####


acl_rsi <- read_excel(
  "sample_data/mental_perform.xlsx",
  sheet = "acl_rsi"
) %>% clean_names() %>%
  mutate(date_ddmmyear = as.Date(date),
         date_ddmmyear2 =  (format(date_ddmmyear, "%b %d, %Y")))


acl_rsi_score <- acl_rsi %>%
  select(date, date_ddmmyear2, score) %>%
  mutate(score = score*100)


wellness <- read_excel(
  "sample_data/mental_perform.xlsx",
  sheet = "pain"
) %>% clean_names() %>%
  mutate(date_ddmmyear = as.Date(date),
         date_ddmmyear2 =  (format(date_ddmmyear, "%b %d, %Y")))



####
#ISO Testing LSI ----
####


iso_joint <- read_excel(
  "sample_data/mental_perform.xlsx",
  sheet = "ISO"
) %>% clean_names() %>%
  mutate(date_ddmmyear = as.Date(date),
         date_ddmmyear2 =  (format(date_ddmmyear, "%b %d, %Y")))





