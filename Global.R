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
#1. REHAB DETAILS ----
####

Rehab_Info <- read_excel(
  "sample_data/ACL-Intake-Form.xlsx"
  ) %>% 
  clean_names()

#Injured Limb
inj_side <- Rehab_Info$limb
#Date of Injury (nice format)
injury_date <- format(Rehab_Info$date_of_injury, "%b %d, %Y")
#Date of surgery if scheduled (nice format)
surgery_date <- ifelse(
  is.na(Rehab_Info$date_of_surgery),
  "TBD",
  format(Rehab_Info$date_of_surgery, "%b %d, %Y")
)

#Show the day number of the rehab, calculated as days since surgery
rehab_day_num <- ifelse(
  is.na(Rehab_Info$date_of_surgery),
  "TBD",
  as.numeric(difftime(today(), Rehab_Info$date_of_surgery, units = "days"))
)

#What day of the week was the surgery?
surgery_wday <- ifelse(
  is.na(Rehab_Info$date_of_surgery),
  "TBD",
  wday(Rehab_Info$date_of_surgery, label = TRUE) %>% as.character()
)


#Find the week of rehab starting from first monday after surgery
monday_post_surgery <- function(date) {
  date <- as.Date(date)
  wd <- as.integer(format(date, "%u"))
  date + ((8 - wd) %% 7)
}
first_monday <- ifelse(
  is.na(Rehab_Info$date_of_surgery),
  "TBD",
  monday_post_surgery(Rehab_Info$date_of_surgery)
)
week <-  ifelse(
  is.na(Rehab_Info$date_of_surgery),
  "TBD",
  floor(as.numeric(difftime(Sys.Date(), as.Date(first_monday), units = "days"))/7)
)

phase0_length <- ifelse(
  is.na(Rehab_Info$date_of_surgery),
  "TBD",
  as.numeric(difftime(Rehab_Info$date_of_surgery, Rehab_Info$date_of_injury, units = "days"))
)






####
#2. FORCE PLATES ----
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
#3. WELLNESS MONITORING ----
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

phase0 <- read_xlsx("sample_data/outcome_data.xlsx", sheet = "Phase_data") %>%
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






####
#CRITERIA ----
####

#Helpers

# --- Evaluate pass/fail using the operator from `criteria` ---
compare <- function(x, op, y) {
  dplyr::case_when(
    op == ">=" ~ x >= y,
    op == ">"  ~ x >  y,
    op == "<=" ~ x <= y,
    op == "<"  ~ x <  y,
    op == "==" ~ x == y,
    TRUE ~ NA
  )
}


#maybe out in global
pretty_var <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%     # underscores -> spaces
    str_squish() %>%
    str_to_title() %>%                # title case
    # fix common acronyms after title-casing
    str_replace_all("\\bLsi\\b", "LSI") %>%
    str_replace_all("\\bRpe\\b", "RPE") %>%
    str_replace_all("\\bHrv\\b", "HRV") %>%
    str_replace_all("\\bVo2\\b", "VO2")
}





####
#PHASE 0 CRITERIA ----
####

# 1 Prepare empty dataframe with all p0 criteria
# 1a) Read Phase 0 criteria
criteria_phase0 <- read_xlsx("sample_data/acl-protocol-criteria-2025.xlsx", sheet = "criteria_full") %>%
  clean_names() %>%
  filter(phase == 0) %>%
  filter(outcome_measure != "Single Leg Hop Test") %>% # Leaving this out for now
  #select(outcome_measure, operator_pretty, operator_code, goal, goal_pretty) %>%
  mutate(score = NA)

# 2) Criteria Data
# Read all phase 0 criteria data
outcomes_raw_phase0 <- read_xlsx("sample_data/outcome_data.xlsx", sheet = "Phase_data") %>%
  clean_names() %>%
  filter(phase == 0)

#Individual Criteria
# 2a) Swelling
p0_swelling <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Swelling") %>%
  filter(side == "Left")
p0_swelling_best <- p0_swelling %>%
  slice_min(value, n = 1, with_ties = FALSE)

# 2b) Passive Knee Extension
p0_extension <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Passive Knee Extension") %>%
  filter(side == "Left")
p0_extension_best <- p0_extension %>%
  slice_min(value, n = 1, with_ties = FALSE)

# 2c) Passive Knee Flexion
p0_flexion <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Passive Knee Flexion") %>%
  filter(side == "Left")
p0_flexion_best <- p0_flexion %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2d) Quad Strength
p0_quads <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Quad Strength") %>%
  group_by(date) %>%
  summarise(
    L = if (any(side == "Left",  na.rm = TRUE))  max(value[side == "Left"],  na.rm = TRUE) else NA_real_,
    R = if (any(side == "Right", na.rm = TRUE))  max(value[side == "Right"], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(L) & !is.na(R) & R != 0, 100 * L / R, NA_real_),
    lsi = round(lsi, 1)
  ) %>%
  arrange(date)

p0_quads_best <- p0_quads %>%
  slice_max(lsi, n = 1, with_ties = FALSE)

# 2e) Hamstring Strength
p0_hams <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Hamstring Strength") %>%
  group_by(date) %>%
  summarise(
    L = if (any(side == "Left",  na.rm = TRUE))  max(value[side == "Left"],  na.rm = TRUE) else NA_real_,
    R = if (any(side == "Right", na.rm = TRUE))  max(value[side == "Right"], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(L) & !is.na(R) & R != 0, 100 * L / R, NA_real_),
    lsi = round(lsi, 1)
  ) %>%
  arrange(date)

p0_hams_best <- p0_hams %>%
  slice_max(lsi, n = 1, with_ties = FALSE)

# 3 Build Phase 0 Crieria Table

#Add current best scores into the table
swelling_val <- suppressWarnings(as.numeric(if (exists("p0_swelling_best") && nrow(p0_swelling_best) > 0) p0_swelling_best$value[1] else NA_real_))
extension_val<- suppressWarnings(as.numeric(if (exists("p0_extension_best") && nrow(p0_extension_best) > 0) p0_extension_best$value[1] else NA_real_))
flexion_val  <- suppressWarnings(as.numeric(if (exists("p0_flexion_best")   && nrow(p0_flexion_best)   > 0) p0_flexion_best$value[1]  else NA_real_))
hams_val     <- suppressWarnings(as.numeric(if (exists("p0_hams_best")     && nrow(p0_hams_best)     > 0) p0_hams_best$lsi[1]     else NA_real_))
quads_val    <- suppressWarnings(as.numeric(if (exists("p0_quads_best")    && nrow(p0_quads_best)    > 0) p0_quads_best$lsi[1]    else NA_real_))

# Populate: only fills when a value exists; stays NA (empty) otherwise
criteria_phase0 <- criteria_phase0 %>%
  mutate(
    score = case_when(
      outcome_measure == "Swelling"            ~ swelling_val,
      outcome_measure == "Passive Knee Extension" ~ extension_val,
      outcome_measure == "Passive Knee Flexion"   ~ flexion_val,
      outcome_measure == "Hamstring Strength"  ~ hams_val,
      outcome_measure == "Quad Strength"       ~ quads_val,
      TRUE                                     ~ score
    )
  )


criteria_phase0 <- criteria_phase0 %>%
  mutate(meets = compare(score, operator_code, goal))





# #Add column for conditional formatting
# criteria_phase0 <- criteria_phase0 %>%
#   mutate(
#     meets = case_when(
#       operator_code == ">=" ~ score >= goal,
#       operator_code == ">"  ~ score >  goal,
#       operator_code == "<=" ~ score <= goal,
#       operator_code == "<"  ~ score <  goal,
#       operator_code == "==" ~ score == goal,
#       TRUE ~ NA
#     )
#   )

