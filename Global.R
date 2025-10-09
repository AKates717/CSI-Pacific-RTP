#Where everything happens behind the scenes
  #loading packages, reading data into R, re-useable functions

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
library(here)
library(htmltools)
library(rmarkdown)


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

inj_side <- Rehab_Info$limb

#Show the day number of the rehab, calculated as days since surgery
days <- as.numeric(difftime(today(), Rehab_Info$date_of_surgery, units = "days"))
injury_date <- format(Rehab_Info$date_of_injury, "%b %d, %Y")
surgery_date <- format(Rehab_Info$date_of_surgery, "%b %d, %Y")
surgery_wday <- wday(Rehab_Info$date_of_surgery, label = TRUE) %>% as.character()

#find the week of rehab starting from first monday after surgery
monday_on_or_after <- function(date) {
  date <- as.Date(date)
  wd <- as.integer(format(date, "%u"))
  date + ((8 - wd) %% 7)
}
first_monday <- monday_on_or_after(Rehab_Info$date_of_surgery)
week <- floor(as.numeric(difftime(Sys.Date(), as.Date(first_monday), units = "days"))/7)


prehab <- as.numeric(difftime(Rehab_Info$date_of_surgery, Rehab_Info$date_of_injury, units = "days"))



  




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

phase0 <- read_xlsx("sample_data/outcome_data.xlsx", sheet = "Phase0_data") %>%
  clean_names() %>%
  mutate(date_ddmmyear = (format(date, "%b %d, %Y")))



####
#ISO Testing LSI ----
####


iso_joint <- read_excel(
  "sample_data/mental_perform.xlsx",
  sheet = "ISO"
) %>% clean_names() %>%
  mutate(date_ddmmyear = as.Date(date),
         date_ddmmyear2 =  (format(date_ddmmyear, "%b %d, %Y")))





