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
library(shiny)

source("plot_functions.R")

options(bslib.bs_version = 5)

####
#1. REHAB DETAILS ----
####

Rehab_Info <- read_excel(
  "sample_data/ACL-Intake-Form.xlsx"
  ) %>% 
  clean_names()

#Injured Limb
inj_side <- Rehab_Info$limb
#Non Injured Limb
non_inj_side <- if_else(Rehab_Info$limb == "Left",
                        "Right",
                        "Left")
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
  ceiling(as.numeric(difftime(as.character(Sys.Date()), as.Date(first_monday), units = "days"))/7)
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


#For filtering 
athlete <- "Frank Reynolds"
#If we need info from this database throughout the site (i.e. body mass)
recent_five_cmj <- recent_five(athlete, "CMJ")
#Most Recent Body Mass (kg)
body_mass <- as.numeric(recent_five_cmj[1,6])





  




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


iso_joint2 <- read_csv(
  "sample_data/tindeq_results.csv"
) %>% clean_names() %>%
  #mutate(date = as.Date(timestamp)) %>%
  mutate(date_ddmmyear = as.Date(timestamp),
         date_ddmmyear2 =  (format(date_ddmmyear, "%b %d, %Y")))




####
#CRITERIA ----
####

#Load Criteria
criteria_all <- read_xlsx("sample_data/acl-protocol-criteria-2025.xlsx", sheet = "criteria_full") %>%
  clean_names()

#Load Outcomes
outcomes_raw_all <- read_xlsx("sample_data/outcome_data.xlsx", sheet = "Phase_data") %>%
  clean_names() %>%
  mutate(date_ddmmyear2 =  (format(date, "%b %d, %Y")))
  

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


#Kind of a reverese of janitor::clean_names()
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
criteria_phase0 <- criteria_all %>%
  filter(phase == 0) %>%
  filter(outcome_measure != "Single Leg Hop Test") %>% # Leaving this out for now
  #select(outcome_measure, operator_pretty, operator_code, goal, goal_pretty) %>%
  mutate(score = NA)

# 2) Criteria Data
# Read all phase 0 criteria data
outcomes_raw_phase0 <- outcomes_raw_all %>%
  filter(phase == 0)

#Individual Criteria
# 2a) Swelling
p0_swelling <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Swelling") %>%
  filter(side == inj_side)
p0_swelling_best <- p0_swelling %>%
  slice_min(value, n = 1, with_ties = FALSE)

# 2b) Passive Knee Extension
p0_extension <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Passive Knee Extension") %>%
  filter(side == inj_side)
p0_extension_best <- p0_extension %>%
  slice_min(value, n = 1, with_ties = FALSE)

# 2c) Passive Knee Flexion
p0_flexion <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Passive Knee Flexion") %>%
  filter(side == inj_side)
p0_flexion_best <- p0_flexion %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2d) Quad Strength
p0_quads <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Quad Strength") %>%
  group_by(date) %>%
  summarise(
    inj = if (any(side == inj_side,  na.rm = TRUE))  max(value[side == inj_side],  na.rm = TRUE) else NA_real_,
    non_inj = if (any(side == non_inj_side, na.rm = TRUE))  max(value[side == non_inj_side], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(inj) & !is.na(non_inj) & non_inj != 0, 100 * inj / non_inj, NA_real_),
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
    inj = if (any(side == inj_side,  na.rm = TRUE))  max(value[side == inj_side],  na.rm = TRUE) else NA_real_,
    non_inj = if (any(side == non_inj_side, na.rm = TRUE))  max(value[side == non_inj_side], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(inj) & !is.na(non_inj) & non_inj != 0, 100 * inj / non_inj, NA_real_),
    lsi = round(lsi, 1)
  ) %>%
  arrange(date)

p0_hams_best <- p0_hams %>%
  slice_max(lsi, n = 1, with_ties = FALSE)

# 3 Build Phase 0 Criteria Table

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

#Use compare helper function (x, op, y)
criteria_phase0 <- criteria_phase0 %>%
  mutate(meets = compare(score, operator_code, goal))


#Build 1 Row summary
#Phase, met/total, percent
progress_p0 <- tibble(
  Phase = 0,
  done = sum(criteria_phase0$meets %in% TRUE, na.rm = TRUE),
  total = nrow(criteria_phase0)
  )  %>%
  mutate(
    Progress = paste0(done, "/", total),
    Percent  = done / total
    ) %>%
  select(Phase, Progress, Percent)
#Will do this for each phase and bind them together for a kind of primary summary table






####
#PHASE 1 CRITERIA ----
####

# 1 Prepare empty dataframe with all p1 criteria
# 1a) Read Phase 1 criteria
criteria_phase1 <- criteria_all %>%
  filter(phase == 1) %>%
  mutate(score = NA)

# 2) Criteria Data
# Read all phase 1 criteria data
outcomes_raw_phase1 <- outcomes_raw_all %>%
  filter(phase == 1)

#Individual Criteria
# 2a) Swelling
p1_swelling <- outcomes_raw_phase1 %>%
  filter(outcome_measure == "Swelling") %>%
  filter(side == inj_side)
p1_swelling_best <- p1_swelling %>%
  slice_min(value, n = 1, with_ties = FALSE)

# 2b) Passive Knee Extension
p1_extension <- outcomes_raw_phase1 %>%
  filter(outcome_measure == "Passive Knee Extension") %>%
  filter(side == inj_side)
p1_extension_best <- p1_extension %>%
  slice_min(value, n = 1, with_ties = FALSE)

# 2c) Passive Knee Flexion
p1_flexion <- outcomes_raw_phase1 %>%
  filter(outcome_measure == "Passive Knee Flexion") %>%
  filter(side == inj_side)
p1_flexion_best <- p1_flexion %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2d) Quads Lag Test
p1_lag <- outcomes_raw_phase1 %>%
  filter(outcome_measure == "Quads Lag Test") %>%
  filter(side == inj_side)
p1_lag_best <- p1_lag %>%
  slice_max(value, n = 1, with_ties = FALSE)



# # 2d) Quad Strength
# p0_quads <- outcomes_raw_phase0 %>%
#   filter(outcome_measure == "Quad Strength") %>%
#   group_by(date) %>%
#   summarise(
#     L = if (any(side == "Left",  na.rm = TRUE))  max(value[side == "Left"],  na.rm = TRUE) else NA_real_,
#     R = if (any(side == "Right", na.rm = TRUE))  max(value[side == "Right"], na.rm = TRUE) else NA_real_,
#     .groups = "drop"
#   ) %>%
#   mutate(
#     lsi = if_else(!is.na(L) & !is.na(R) & R != 0, 100 * L / R, NA_real_),
#     lsi = round(lsi, 1)
#   ) %>%
#   arrange(date)
# 
# p0_quads_best <- p0_quads %>%
#   slice_max(lsi, n = 1, with_ties = FALSE)
# 
# # 2e) Hamstring Strength
# p0_hams <- outcomes_raw_phase0 %>%
#   filter(outcome_measure == "Hamstring Strength") %>%
#   group_by(date) %>%
#   summarise(
#     L = if (any(side == "Left",  na.rm = TRUE))  max(value[side == "Left"],  na.rm = TRUE) else NA_real_,
#     R = if (any(side == "Right", na.rm = TRUE))  max(value[side == "Right"], na.rm = TRUE) else NA_real_,
#     .groups = "drop"
#   ) %>%
#   mutate(
#     lsi = if_else(!is.na(L) & !is.na(R) & R != 0, 100 * L / R, NA_real_),
#     lsi = round(lsi, 1)
#   ) %>%
#   arrange(date)
# 
# p0_hams_best <- p0_hams %>%
#   slice_max(lsi, n = 1, with_ties = FALSE)

# 3 Build Phase 1 Criteria Table

#Add current best scores into the table
swelling_val_p1 <- suppressWarnings(as.numeric(if (exists("p1_swelling_best") && nrow(p1_swelling_best) > 0) p1_swelling_best$value[1] else NA_real_))
extension_val_p1 <- suppressWarnings(as.numeric(if (exists("p1_extension_best") && nrow(p1_extension_best) > 0) p1_extension_best$value[1] else NA_real_))
flexion_val_p1  <- suppressWarnings(as.numeric(if (exists("p1_flexion_best")   && nrow(p1_flexion_best)   > 0) p1_flexion_best$value[1]  else NA_real_))
lag_val_p1  <- suppressWarnings(as.numeric(if (exists("p1_lag_best")   && nrow(p1_lag_best)   > 0) p1_lag_best$value[1]  else NA_real_))
# hams_val     <- suppressWarnings(as.numeric(if (exists("p0_hams_best")     && nrow(p0_hams_best)     > 0) p0_hams_best$lsi[1]     else NA_real_))
# quads_val    <- suppressWarnings(as.numeric(if (exists("p0_quads_best")    && nrow(p0_quads_best)    > 0) p0_quads_best$lsi[1]    else NA_real_))

# Populate: only fills when a value exists; stays NA (empty) otherwise
criteria_phase1 <- criteria_phase1 %>%
  mutate(
    score = case_when(
      outcome_measure == "Swelling"            ~ swelling_val_p1,
      outcome_measure == "Passive Knee Extension" ~ extension_val_p1,
      outcome_measure == "Passive Knee Flexion"   ~ flexion_val_p1,
      outcome_measure == "Quads Lag Test"            ~ lag_val_p1,
      # outcome_measure == "Hamstring Strength"  ~ hams_val,
      # outcome_measure == "Quad Strength"       ~ quads_val,
      TRUE                                     ~ score
    )
  )

#Use compare helper function (x, op, y)
criteria_phase1 <- criteria_phase1 %>%
  mutate(meets = compare(score, operator_code, goal))


#Build 1 Row summary
#Phase, met/total, percent
progress_p1 <- tibble(
  Phase = 1,
  done = sum(criteria_phase1$meets %in% TRUE, na.rm = TRUE),
  total = nrow(criteria_phase1)
)  %>%
  mutate(
    Progress = paste0(done, "/", total),
    Percent  = done / total
  ) %>%
  select(Phase, Progress, Percent)
#Will do this for each phase and bind them together for a kind of primary summary table








####
#PHASE 2 CRITERIA ----
####

# 1 Prepare empty dataframe with all p2 criteria
# 1a) Read Phase 2 criteria
criteria_phase2 <- criteria_all %>%
  filter(phase == 2) %>%
  mutate(score = NA)

# 1b) Phase specific mutations
#Body Mass Multiplier for TBDL ----
criteria_phase2 <- criteria_phase2 %>%
  mutate(goal_pretty = case_when(
    outcome_measure == "TrapBar Deadlift" ~ paste0(round(1.5*body_mass,0), " kg"),
    TRUE                           ~ as.character(goal_pretty)
  )) %>%
  mutate(goal = if_else(outcome_measure == "TrapBar Deadlift", round(1.5*body_mass,0), goal))

# 2) Criteria Data
# Read all phase 2 criteria data
outcomes_raw_phase2 <- outcomes_raw_all %>%
  filter(phase == 2)

#Individual Criteria
# 2a) Swelling
p2_swelling <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Swelling") %>%
  filter(side == inj_side)
p2_swelling_best <- p2_swelling %>%
  slice_min(value, n = 1, with_ties = FALSE)

# 2b) Passive Knee Extension
p2_extension <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Passive Knee Extension") %>%
  filter(side == inj_side)
p2_extension_best <- p2_extension %>%
  slice_min(value, n = 1, with_ties = FALSE)

# 2c) Passive Knee Flexion
p2_flexion <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Passive Knee Flexion") %>%
  filter(side == inj_side)
p2_flexion_best <- p2_flexion %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2d) Single Leg Bridge
p2_slbridge <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Single Leg Bridge") %>%
  filter(side == inj_side)
p2_slbridge_best <- p2_slbridge %>%
  slice_max(value, n = 1, with_ties = FALSE)
  
# 2e) Single Leg Calf Raise
p2_slcalfraise <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Single Leg Calf Raises") %>%
  filter(side == inj_side)
p2_slcalfraise_best <- p2_slcalfraise %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2f) Side Bridge
p2_sidebridge <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Side Bridge") %>%
  filter(side == inj_side)
p2_sidebridge_best <- p2_sidebridge %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2g) Side Bridge
p2_sidebridge <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Side Bridge") %>%
  filter(side == inj_side)
p2_sidebridge_best <- p2_sidebridge %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2h) Single Leg Rise
p2_slrise <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Single Leg Rise") %>%
  filter(side == inj_side)
p2_slrise_best <- p2_slrise %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2i) Single Leg Balance Eyes Open
p2_slbalanceopen <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Single Leg Balance (Eyes Open)") %>%
  filter(side == inj_side)
p2_slbalanceopen_best <- p2_slbalanceopen %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2j) Single Leg Balance Eyes Closed
p2_slbalanceclosed <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Single Leg Balance (Eyes Closed)") %>%
  filter(side == inj_side)
p2_slbalanceclosed_best <- p2_slbalanceclosed %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2k) TrapBar Deadlift
p2_trapbar <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "TrapBar Deadlift")
p2_trapbar_best <- p2_trapbar %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2l) Quad Strength
p2_quads <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Quad Strength") %>%
  group_by(date) %>%
  summarise(
    inj = if (any(side == inj_side,  na.rm = TRUE))  max(value[side == inj_side],  na.rm = TRUE) else NA_real_,
    non_inj = if (any(side == non_inj_side, na.rm = TRUE))  max(value[side == non_inj_side], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(inj) & !is.na(non_inj) & non_inj != 0, 100 * inj / non_inj, NA_real_),
    lsi = round(lsi, 1)
  ) %>%
  arrange(date)

p2_quads_best <- p2_quads %>%
  slice_max(lsi, n = 1, with_ties = FALSE)

# 2m) Hamstring Strength
p2_hams <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Hamstring Strength") %>%
  group_by(date) %>%
  summarise(
    inj = if (any(side == inj_side,  na.rm = TRUE))  max(value[side == inj_side],  na.rm = TRUE) else NA_real_,
    non_inj = if (any(side == non_inj_side, na.rm = TRUE))  max(value[side == non_inj_side], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(inj) & !is.na(non_inj) & non_inj != 0, 100 * inj / non_inj, NA_real_),
    lsi = round(lsi, 1)
  ) %>%
  arrange(date)

p2_hams_best <- p2_hams %>%
  slice_max(lsi, n = 1, with_ties = FALSE)

# 2m) Ankle Dorsiflexion
p2_dorsi <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Ankle Dorsiflexion") %>%
  filter(side == inj_side)
p2_dorsi_best <- p2_dorsi %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2n) Y-Balance (Anterior)
p2_yBalAnt <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Y-Balance (Anterior)") %>%
  group_by(date) %>%
  summarise(
    inj = if (any(side == inj_side,  na.rm = TRUE))  max(value[side == inj_side],  na.rm = TRUE) else NA_real_,
    non_inj = if (any(side == non_inj_side, na.rm = TRUE))  max(value[side == non_inj_side], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(inj) & !is.na(non_inj) & non_inj != 0, 100 * inj / non_inj, NA_real_),
    lsi = round(lsi, 1)
  ) %>%
  arrange(date)
  
p2_yBalAnt_best <- p2_yBalAnt %>%
  slice_max(lsi, n = 1, with_ties = FALSE)

# 2o) Y-Balance (PM)
p2_yBalPM <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Y-Balance (Postero-Medial)") %>%
  group_by(date) %>%
  summarise(
    inj = if (any(side == inj_side,  na.rm = TRUE))  max(value[side == inj_side],  na.rm = TRUE) else NA_real_,
    non_inj = if (any(side == non_inj_side, na.rm = TRUE))  max(value[side == non_inj_side], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(inj) & !is.na(non_inj) & non_inj != 0, 100 * inj / non_inj, NA_real_),
    lsi = round(lsi, 1)
  ) %>%
  arrange(date)

p2_yBalPM_best <- p2_yBalPM %>%
  slice_max(lsi, n = 1, with_ties = FALSE)

# 2p) Y-Balance (Anterior)
p2_yBalPL <- outcomes_raw_phase2 %>%
  filter(outcome_measure == "Y-Balance (Postero-Lateral)") %>%
  group_by(date) %>%
  summarise(
    inj = if (any(side == inj_side,  na.rm = TRUE))  max(value[side == inj_side],  na.rm = TRUE) else NA_real_,
    non_inj = if (any(side == non_inj_side, na.rm = TRUE))  max(value[side == non_inj_side], na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    lsi = if_else(!is.na(inj) & !is.na(non_inj) & non_inj != 0, 100 * inj / non_inj, NA_real_),
    lsi = round(lsi, 1)
  ) %>%
  arrange(date)

p2_yBalPL_best <- p2_yBalPL %>%
  slice_max(lsi, n = 1, with_ties = FALSE)


# 3 Build Phase 2 Criteria Table

#Add current best scores into the table
swelling_val_p2 <- suppressWarnings(as.numeric(if (exists("p2_swelling_best") && nrow(p2_swelling_best) > 0) p2_swelling_best$value[1] else NA_real_))
extension_val_p2 <- suppressWarnings(as.numeric(if (exists("p2_extension_best") && nrow(p2_extension_best) > 0) p2_extension_best$value[1] else NA_real_))
flexion_val_p2  <- suppressWarnings(as.numeric(if (exists("p2_flexion_best")   && nrow(p2_flexion_best)   > 0) p2_flexion_best$value[1]  else NA_real_))
slbridge_val_p2  <- suppressWarnings(as.numeric(if (exists("p2_slbridge_best")   && nrow(p2_slbridge_best)   > 0) p2_slbridge_best$value[1]  else NA_real_))
slcalfraise_val_p2  <- suppressWarnings(as.numeric(if (exists("p2_slcalfraise_best")   && nrow(p2_slcalfraise_best)   > 0) p2_slcalfraise_best$value[1]  else NA_real_))
sidebridge_val_p2  <- suppressWarnings(as.numeric(if (exists("p2_sidebridge_best")   && nrow(p2_sidebridge_best)   > 0) p2_sidebridge_best$value[1]  else NA_real_))
slrise_val_p2  <- suppressWarnings(as.numeric(if (exists("p2_slrise_best")   && nrow(p2_slrise_best)   > 0) p2_slrise_best$value[1]  else NA_real_))
slbalanceopen_val_p2  <- suppressWarnings(as.numeric(if (exists("p2_slbalanceopen_best")   && nrow(p2_slbalanceopen_best)   > 0) p2_slbalanceopen_best$value[1]  else NA_real_))
slbalanceclosed_val_p2  <- suppressWarnings(as.numeric(if (exists("p2_slbalanceclosed_best")   && nrow(p2_slbalanceclosed_best)   > 0) p2_slbalanceclosed_best$value[1]  else NA_real_))
trapbar_val_p2  <- suppressWarnings(as.numeric(if (exists("p2_trapbar_best")   && nrow(p2_trapbar_best)   > 0) p2_trapbar_best$value[1]  else NA_real_))
quads_val_p2    <- suppressWarnings(as.numeric(if (exists("p2_quads_best")    && nrow(p2_quads_best)    > 0) p2_quads_best$lsi[1]    else NA_real_))
hams_val_p2     <- suppressWarnings(as.numeric(if (exists("p2_hams_best")     && nrow(p2_hams_best)     > 0) p2_hams_best$lsi[1]     else NA_real_))
dorsi_val_p2     <- suppressWarnings(as.numeric(if (exists("p2_dorsi_best")     && nrow(p2_dorsi_best)     > 0) p2_dorsi_best$value[1]     else NA_real_))
yBalAnt_val_p2     <- suppressWarnings(as.numeric(if (exists("p2_yBalAnt_best")     && nrow(p2_yBalAnt_best)     > 0) p2_yBalAnt_best$lsi[1]     else NA_real_))
yBalPM_val_p2     <- suppressWarnings(as.numeric(if (exists("p2_yBalPM_best")     && nrow(p2_yBalPM_best)     > 0) p2_yBalPM_best$lsi[1]     else NA_real_))
yBalPL_val_p2     <- suppressWarnings(as.numeric(if (exists("p2_yBalPL_best")     && nrow(p2_yBalPL_best)     > 0) p2_yBalPL_best$lsi[1]     else NA_real_))



# Populate: only fills when a value exists; stays NA (empty) otherwise
criteria_phase2 <- criteria_phase2 %>%
  mutate(
    score = case_when(
      outcome_measure == "Swelling"            ~ swelling_val_p2,
      outcome_measure == "Passive Knee Extension" ~ extension_val_p2,
      outcome_measure == "Passive Knee Flexion"   ~ flexion_val_p2,
      outcome_measure == "Single Leg Bridge"   ~ slbridge_val_p2,
      outcome_measure == "Single Leg Calf Raises"   ~ slcalfraise_val_p2,
      outcome_measure == "Side Bridge"   ~ sidebridge_val_p2,
      outcome_measure == "Single Leg Rise"   ~ slrise_val_p2,
      outcome_measure == "Single Leg Balance (Eyes Open)"   ~ slbalanceopen_val_p2,
      outcome_measure == "Single Leg Balance (Eyes Closed)"   ~ slbalanceclosed_val_p2,
      outcome_measure == "TrapBar Deadlift"   ~ trapbar_val_p2,
      outcome_measure == "Quad Strength"       ~ quads_val_p2,
      outcome_measure == "Hamstring Strength"  ~ hams_val_p2,
      outcome_measure == "Ankle Dorsiflexion"  ~ dorsi_val_p2,
      outcome_measure == "Y-Balance (Anterior)"  ~ yBalAnt_val_p2,
      outcome_measure == "Y-Balance (Postero-Medial)"  ~ yBalPM_val_p2,
      outcome_measure == "Y-Balance (Postero-Lateral)"  ~ yBalPL_val_p2,
      TRUE                                     ~ score
    )
  )

#Use compare helper function (x, op, y)
criteria_phase2 <- criteria_phase2 %>%
  mutate(meets = compare(score, operator_code, goal))


#Build 1 Row summary
#Phase, met/total, percent
progress_p2 <- tibble(
  Phase = 2,
  done = sum(criteria_phase2$meets %in% TRUE, na.rm = TRUE),
  total = nrow(criteria_phase2)
)  %>%
  mutate(
    Progress = paste0(done, "/", total),
    Percent  = done / total
  ) %>%
  select(Phase, Progress, Percent)
#Will do this for each phase and bind them together for a kind of primary summary table







####
#PHASE 3 CRITERIA ----
####

# 1 Prepare empty dataframe with all p3 criteria
# 1a) Read Phase 3 criteria
criteria_phase3 <- criteria_all %>%
  filter(phase == 3) %>%
  mutate(score = NA)

# 1b) Phase specific mutations
#Body Mass Multiplier for TBDL ----
criteria_phase3 <- criteria_phase3 %>%
  mutate(goal_pretty = case_when(
    outcome_measure %in% c("TrapBar Deadlift", "Single Leg Press") ~
      paste0(round(1.8 * body_mass, 0), " kg"),
    TRUE ~ as.character(goal_pretty)
  )) %>%
  mutate(goal = if_else(
    outcome_measure %in% c("TrapBar Deadlift", "Single Leg Press"),
    round(1.8 * body_mass, 0),
    goal
  ))


# 2) Criteria Data
# Read all phase 3 criteria data
outcomes_raw_phase3 <- outcomes_raw_all %>%
  filter(phase == 3)

#Individual Criteria
# 2a) Swelling
p3_slhop <- outcomes_raw_phase3 %>%
  filter(outcome_measure == "Single Leg Hop Test") %>%
  filter(side == inj_side)
p3_slhop_best <- p3_slhop %>%
  slice_max(value, n = 1, with_ties = FALSE)

# 2b)






# 3 Build Phase 3 Criteria Table

#Add current best scores into the table
slhop_val_p3 <- suppressWarnings(as.numeric(if (exists("p3_slhop_best") && nrow(p3_slhop_best) > 0) p3_slhop_best$value[1] else NA_real_))




# Populate: only fills when a value exists; stays NA (empty) otherwise
criteria_phase3 <- criteria_phase3 %>%
  mutate(
    score = case_when(
      outcome_measure == "Single Leg Hop Test"            ~ slhop_val_p3,
      TRUE                                     ~ score
    )
  )


#Use compare helper function (x, op, y)
criteria_phase3 <- criteria_phase3 %>%
  mutate(meets = compare(score, operator_code, goal))


#Build 1 Row summary
#Phase, met/total, percent
progress_p3 <- tibble(
  Phase = 3,
  done = sum(criteria_phase3$meets %in% TRUE, na.rm = TRUE),
  total = nrow(criteria_phase3)
)  %>%
  mutate(
    Progress = paste0(done, "/", total),
    Percent  = done / total
  ) %>%
  select(Phase, Progress, Percent)
#Will do this for each phase and bind them together for a kind of primary summary table









progress_overall <- bind_rows(progress_p0, progress_p1, progress_p2, progress_p3)





