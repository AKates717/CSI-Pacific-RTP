knitr::opts_knit$set(root.dir = normalizePath(".."))

source("Global.R")
source("plot_functions.R")
source("Themes.R")


# Read all daily monitoring data
daily_mon_raw <- read_xlsx("sample_data/outcome_data.xlsx", sheet = "daily") %>%
  clean_names()

daily_mon_most_recent <- lubridate::as_date(max(daily_mon_raw$date))

daily_mon_past <- daily_mon_raw %>%
  filter(date > daily_mon_most_recent-28 & date < daily_mon_most_recent)


#Descriptive stats for all past results
wellness_summary <- daily_mon_past %>%
  summarise(across(
    where(is.numeric),
    list(
      mean = ~ mean(.x, na.rm = TRUE),
      sd   = ~ sd(.x, na.rm = TRUE),
      cv   = ~ ifelse(mean(.x, na.rm = TRUE) == 0, NA_real_,
                      sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

wellness_summary <- wellness_summary %>%
  mutate(upper = mean + (mean * cv),
         lower = mean - (mean * cv))


today <- daily_mon_raw %>%
  filter(as.Date(date) == lubridate::today(tzone = "America/Toronto")) %>%
  select(-timestamp) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value")

today2 <- left_join(today, wellness_summary, by = "variable")

today_wellness <- today2 %>%
  filter(variable != "hours_of_sleep") %>%
  mutate(var_label = pretty_var(variable)) %>%
  mutate(Flag = case_when(
    value < lower ~ "Decrease",
    value > upper ~ "Increase",
    TRUE ~ "Normal"
  )) %>%
  mutate(Colour = case_when(
    Flag == "Normal" ~ "gray40",
    Flag == "Decrease" ~ "#009E73",
    Flag == "Increase" ~ "#D55E00"
  ))

today_sleep <- today2 %>%
  filter(variable == "hours_of_sleep") %>%
  mutate(var_label = pretty_var(variable)) %>%
  mutate(Flag = case_when(
    value < lower ~ "Decrease",
    value > upper ~ "Increase",
    TRUE ~ "Normal"
  )) %>%
  mutate(Colour = case_when(
    Flag == "Normal" ~ "gray40",
    Flag == "Decrease" ~ "#D55E00",
    Flag == "Increase" ~ "#009E73"
  ))



#Two plots
#1) The rows are outcome measures instead of athlete names
#1a) Wellness
p_well <- ggplot(today_wellness) +
  geom_bar(aes(x = var_label, y = value, fill = Flag), stat = "identity", width = 0.5, fill = today_wellness$Colour) +
  geom_errorbar(aes(x = var_label, ymin = lower, ymax = upper), linewidth = 1, width = 0.5) +
  geom_point(aes(x = var_label, y = mean), size = 2, shape = 23, fill = "#0072B2", stroke = 1.5) +
  geom_text(aes(x = var_label, y = value, label = value), position = position_stack(vjust = .5), fontface = "bold", color = "white") +
  scale_y_continuous(
    limits = c(0, 5),
    breaks = 0:5,
    expand = expansion(mult = c(0, 0.05)),
    oob = scales::oob_squish
  ) +
  ak_plot_theme() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", vjust = 0, hjust = 0.5),
        plot.subtitle = element_text(size = 9)) +
  # labs(title = "Daily Monitoring",
  #      subtitle = paste0(format(Sys.Date(), "%b %d, %Y"))) +
  coord_flip()

#1b) Sleep
p_sleep <- ggplot(today_sleep) +
  geom_bar(aes(x = var_label, y = value, fill = Flag), stat = "identity", width = 0.5, fill = today_sleep$Colour) +
  geom_errorbar(aes(x = var_label, ymin = lower, ymax = upper), linewidth = 1, width = 0.5) +
  geom_point(aes(x = var_label, y = mean), size = 2, shape = 23, fill = "#0072B2", stroke = 1.5) +
  geom_text(aes(x = var_label, y = value, label = value), position = position_stack(vjust = .5), fontface = "bold", color = "white") +
  scale_y_continuous(
    limits = c(0, 12),
    breaks = 0:12,
    expand = expansion(mult = c(0, 0.05)),
    oob = scales::oob_squish
  ) +
  ak_plot_theme() +
  theme(axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", vjust = 0, hjust = 0.5),
        plot.subtitle = element_text(size = 9)) +
  # labs(title = "Daily Monitoring",
  #      subtitle = paste0(format(Sys.Date(), "%b %d, %Y"))) +
  coord_flip()

p_sleep/p_well +
  plot_layout(heights = c(1,4))









#2) Outcome measure by date.  Start with knee soreness, eventually make it reactive

v0 <- "knee_soreness"

v1 <- daily_mon_raw %>%
  select(date, timestamp, knee_soreness) %>%
  mutate(avg = lag(cummean(knee_soreness)))
  # select(timestamp, date, all_of(v0)) %>%
  # arrange(timestamp) %>%                      # ensure chronological
  # mutate(
  #   {{ v0 }} := as.numeric(.data[[v0]]),      # coerce in case it's character
  #   avg = lag(cummean(.data[[v0]]))           # running mean up to previous row
  # )

n <- seq_along(v1$knee_soreness) #just creating a sequence, essentially a row number
m <- cumsum(v1$knee_soreness) /n #this one plus the one above just does a cummean I guess
m2 <- cumsum(v1$knee_soreness * v1$knee_soreness) / n #
v <- (m2 - m * m) * (n / (n - 1)) #
s <- sqrt(v) #st dev
CoV <- s/m #Cov

v1$CoV <- CoV

v1 <- v1 %>%
  mutate(Upper = round(avg + (avg * CoV),2),
         Lower = round(avg - (avg * CoV),2)) %>%
  mutate(Flag = case_when(
    knee_soreness < Lower ~ "Decrease",
    knee_soreness > Upper ~ "Increase",
    TRUE ~ "Normal"
  )) %>%
  mutate(Colour = case_when(
    Flag == "Normal" ~ "gray40",
    Flag == "Decrease" ~ "#009E73",
    Flag == "Increase" ~ "#D55E00"
  ))


p_knee_soreness <- v1 %>%
  mutate(date = as.Date(date)) %>%          # or as.Date(timestamp)
  ggplot(aes(x = date, y = knee_soreness)) +
  geom_col(fill = v1$Colour, alpha = 0.9, width = 0.8) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), size = 1, width = 0.5) +
  geom_point(aes(y = avg), size = 2, shape = 23, fill = "#0072B2", stroke = .5) +
  geom_text(aes(label = knee_soreness), position = position_stack(vjust = .5), angle = 90, fontface = "bold", color = "white", size = 3) +
  scale_x_date(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(limits = c(0, 5), oob = oob_squish) +
  ak_plot_theme() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )
  #ylim(0,5)

ggplotly(p_knee_soreness)

































#PHASE 0 CRITERIA (complete) ----

# 1 Prepare empty dataframe with all p0 criteria
# 1a) Read Phase 0 criteria
criteria_phase0_names <- read_xlsx("sample_data/acl-protocol-criteria-2025.xlsx", sheet = "criteria_full") %>%
  clean_names() %>%
  filter(phase == 0) %>%
  filter(outcome_measure != "Single Leg Hop Test") %>% # Leaving this out for now
  select(outcome_measure, operator_pretty, operator_code, goal, goal_pretty) %>%
  mutate(score = NA)

# # 2) Criteria Data
# # Read all phase 0 criteria data
outcomes_raw_phase0 <- read_xlsx("sample_data/outcome_data.xlsx") %>%
  clean_names() %>%
  filter(phase == 0)

#Individual Criteria
# 2a) Swelling
p0_swelling <- outcomes_raw_phase0 %>%
  filter(outcome_measure == "Swelling") %>%
  filter(side == "Left")

p0_swelling_best <- p0_swelling %>%
  slice_min(value, n = 1, with_ties = FALSE)



# # 2b) Passive Knee Extension
# p0_extension <- outcomes_raw_phase0 %>%
#   filter(outcome_measure == "Passive Knee Extension") %>%
#   filter(side == "Left")
#
# p0_extension_best <- p0_extension %>%
#   slice_min(value, n = 1, with_ties = FALSE)
#
#
#
# # 2c) Passive Knee Flexion
# p0_flexion <- outcomes_raw_phase0 %>%
#   filter(outcome_measure == "Passive Knee Flexion") %>%
#   filter(side == "Left")
#
# p0_flexion_best <- p0_flexion %>%
#   slice_max(value, n = 1, with_ties = FALSE)
#
#
#
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
#
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
#
#
#
#
#
#
#
#
# # 3 Build Phase 0 Crieria Table
#
# #Add current best scores into the table
# swelling_val <- suppressWarnings(as.numeric(if (exists("p0_swelling_best") && nrow(p0_swelling_best) > 0) p0_swelling_best$value[1] else NA_real_))
# extension_val<- suppressWarnings(as.numeric(if (exists("p0_extension_best") && nrow(p0_extension_best) > 0) p0_extension_best$value[1] else NA_real_))
# flexion_val  <- suppressWarnings(as.numeric(if (exists("p0_flexion_best")   && nrow(p0_flexion_best)   > 0) p0_flexion_best$value[1]  else NA_real_))
# hams_val     <- suppressWarnings(as.numeric(if (exists("p0_hams_best")     && nrow(p0_hams_best)     > 0) p0_hams_best$lsi[1]     else NA_real_))
# quads_val    <- suppressWarnings(as.numeric(if (exists("p0_quads_best")    && nrow(p0_quads_best)    > 0) p0_quads_best$lsi[1]    else NA_real_))
#
# # Populate: only fills when a value exists; stays NA (empty) otherwise
# criteria_phase0_names <- criteria_phase0_names %>%
#   mutate(
#     score = case_when(
#       outcome_measure == "Swelling"            ~ swelling_val,
#       outcome_measure == "Passive Knee Extension" ~ extension_val,
#       outcome_measure == "Passive Knee Flexion"   ~ flexion_val,
#       outcome_measure == "Hamstring Strength"  ~ hams_val,
#       outcome_measure == "Quad Strength"       ~ quads_val,
#       TRUE                                     ~ score
#     )
#   )
#
#
# criteria_phase0_names <- criteria_phase0_names %>%
#   mutate(meets = compare(score, operator_code, goal))
#
#
#
# # #Add column for conditional formatting
# # criteria_phase0_names <- criteria_phase0_names %>%
# #   mutate(
# #     meets = case_when(
# #       operator_code == ">=" ~ score >= goal,
# #       operator_code == ">"  ~ score >  goal,
# #       operator_code == "<=" ~ score <= goal,
# #       operator_code == "<"  ~ score <  goal,
# #       operator_code == "==" ~ score == goal,
# #       TRUE ~ NA
# #     )
# #   )
#
# #Create the table
# tbl_phase0_min <- criteria_phase0_names %>%
#   transmute(
#     `Outcome Measure` = outcome_measure,
#     `Op` = operator_pretty,
#     Goal              = goal_pretty,   # nice display string from your sheet
#     Score             = score,
#     meets
#   ) %>%
#   gt() %>%
#   fmt_number(
#     columns = Score,
#     rows = `Outcome Measure` %in% c("Hamstring Strength", "Quadriceps Strength"),
#     decimals = 0,
#     pattern = "{x}%"
#   ) %>%
#   sub_missing(columns = Score, missing_text = "") %>%
#   cols_label(Op = "") %>%
#   cols_align(align = "center", columns = c(Goal, Score)) %>%
#   cols_align(align = "right",  columns = Op) %>%
#   tab_style(
#     style = list(cell_fill(color = "#E8F5E9"),
#                  cell_text(color = "#1B5E20", weight = "600")),
#     locations = cells_body(columns = Score, rows = meets %in% TRUE)
#   ) %>%
#   tab_style(
#     style = list(cell_fill(color = "#FFF9C4"),
#                  cell_text(color = "#7A6A00", weight = "600")),
#     locations = cells_body(columns = Score, rows = meets %in% FALSE)
#   )  %>%
#   cols_hide(meets) %>%
#   opt_css("td[data-col='Outcome Measure'] { white-space: nowrap; }") %>%
#   ak_gt_theme3()
#
# tbl_phase0_min
#
#
#
#
#
































































# 
# criteria <- readxl::read_xlsx("sample_data/acl-protocol-criteria-2025.xlsx", sheet="criteria_full") %>%
#   clean_names()
#   #dplyr::filter(!is.na(date)) %>%
#   #dplyr::mutate(date = ymd(date))
# 
# min(phase0$date)  
# #rm(phase0)  
#   
# 
# 
# 
# phase0 <- read_xlsx("sample_data/outcome_data.xlsx", sheet = "Phase0_data") %>%
#   clean_names()
#   
# inj_side <- "Left"
# 
# swelling <- phase0 %>%
#   filter(outcome_measure == "Swelling",
#          side == inj_side)
#  
# p <- ggplot(swelling, aes(x = date, y = value)) +
#   geom_col(fill = "#E1535B", alpha = 0.7 ) +
#   geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
#   ak_plot_theme() +
#   labs(
#     x = NULL,
#     y = "Swelling (cm)"
#   )
# 
# ggplotly(p) 
#   
#   
#   
#   
# 
# # 2) Helpers for parsing numbers and percents
# parse_num <- function(x) {
#   if (is.numeric(x)) return(x)
#   s <- as.character(x)
#   suppressWarnings(as.numeric(str_extract(s, "-?\\d+\\.?\\d*")))
# }
# is_percent <- function(x) grepl("%", as.character(x))
# 
# # 3) Prep data: normalize operators, parse numbers, and decide if goal is met
# phase0_prepped <- phase0_criteria %>%
#   mutate(
#     # Normalize operators like ≥ ≤ to R-friendly >= <=
#     op_clean = trimws(Operator),
#     op_clean = dplyr::recode(op_clean,
#                              "≥" = ">=", "≤" = "<=", "=" = "==", "=>" = ">=", "=<" = "<="
#     ),
#     
#     # Raw values for potential string-equality comparisons
#     score_raw = Score,
#     goal_raw  = Goal,
#     
#     # Numerics (pull first numeric token)
#     score_num = parse_num(Score),
#     goal_num  = parse_num(Goal),
#     
#     # Percent handling: if Goal shows %, compare in 0–100 space
#     score_num = dplyr::case_when(
#       is_percent(goal_raw) & !is.na(score_num) & score_num <= 1 ~ score_num * 100,
#       TRUE ~ score_num
#     ),
#     
#     # For text equality goals (e.g., "Pass"), allow == to compare strings
#     meets_string = dplyr::case_when(
#       op_clean == "==" ~ tolower(trimws(as.character(score_raw))) ==
#         tolower(trimws(as.character(goal_raw))),
#       TRUE ~ NA
#     ),
#     
#     # Final meets_goal logic (numeric first; fall back to string equality)
#     meets_goal = dplyr::case_when(
#       !is.na(score_num) & !is.na(goal_num) & op_clean == ">=" ~ score_num >= goal_num,
#       !is.na(score_num) & !is.na(goal_num) & op_clean == ">"  ~ score_num >  goal_num,
#       !is.na(score_num) & !is.na(goal_num) & op_clean == "<=" ~ score_num <= goal_num,
#       !is.na(score_num) & !is.na(goal_num) & op_clean == "<"  ~ score_num <  goal_num,
#       !is.na(meets_string) ~ meets_string,
#       TRUE ~ NA
#     )
#   )
# 
# phase0_prepped <- phase0_prepped %>%
#   mutate(
#     Goal  = ifelse(grepl("^0?\\.\\d+$", Goal),  paste0(as.numeric(Goal)  * 100, "%"), Goal),
#     Score = ifelse(grepl("^0?\\.\\d+$", Score), paste0(as.numeric(Score) * 100, "%"), Score)
#   )
# 
# 
# # 4) Build gt with Option A (icon tooltip) + Score coloring
# tbl <- phase0_prepped %>%
#   select(
#     `Outcome Measure`,
#     Operator, Goal, Score, meets_goal
#   ) %>%
#   gt() %>%
#   cols_label(
#     `Outcome Measure` = "Outcome Measure",
#     #`Display Description` = "Display Description",
#     #Info = "",
#     Goal = "Goal",
#     Operator = "",
#     Score = "Score"
#   ) %>%
#   cols_align(align = "center", columns = c(Goal, Operator, Score)) %>%
#   ak_gt_theme3() %>%
#   # cols_width(
#   #   `Outcome Measure` ~ px(260),
#   #   Goal ~ px(90),
#   #   Operator ~ px(60),
#   #   Score ~ px(100)
#   # ) %>%
#   #tab_options(table.font.size = px(14), data_row.padding = px(6)) %>%
#   # Color Score cell by pass/fail/NA
#   tab_style(
#     style = list(
#       cell_fill(color = "#E8F5E9"),   # light green
#       cell_text(color = "#1B5E20", weight = "600")
#     ),
#     locations = cells_body(columns = Score, rows = meets_goal %in% TRUE)
#   ) %>%
#   tab_style(
#     style = list(
#       cell_fill(color = "#FFF9C4"),   # light yellow
#       cell_text(color = "#7A6A00", weight = "600")
#     ),
#     locations = cells_body(columns = Score, rows = meets_goal %in% FALSE)
#   ) %>%
#   tab_style(
#     style = list(
#       cell_fill(color = "#ECEFF1"),   # light grey for NA/uncomparable
#       cell_text(color = "#455A64")
#     ),
#     locations = cells_body(columns = Score, rows = is.na(meets_goal))
#   ) %>%
#   # Hide helper column
#   cols_hide(columns = meets_goal)
# 
# 
# 
# tbl
# 
