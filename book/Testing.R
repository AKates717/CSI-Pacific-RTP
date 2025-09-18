
knitr::opts_knit$set(root.dir = normalizePath(".."))

source("Global.R")
source("plot_functions.R")
source("Themes.R")


phase0_length <- tibble(
  'Injury Date ' = injury_date,
  'Surgery Date' = surgery_date,
  'Prehab Window' = paste0(as.numeric(difftime(Rehab_Info$date_of_surgery, Rehab_Info$date_of_injury, units = "days")), " Days")
) %>% gt() %>%
  ak_gt_theme3()

phase0_length









phase0_criteria <- readxl::read_xlsx("sample_data/acl-protocol-criteria-2025.xlsx", sheet="Phase0")




# 2) Helpers for parsing numbers and percents
parse_num <- function(x) {
  if (is.numeric(x)) return(x)
  s <- as.character(x)
  suppressWarnings(as.numeric(str_extract(s, "-?\\d+\\.?\\d*")))
}
is_percent <- function(x) grepl("%", as.character(x))

# 3) Prep data: normalize operators, parse numbers, and decide if goal is met
phase0_prepped <- phase0_criteria %>%
  mutate(
    # Normalize operators like ≥ ≤ to R-friendly >= <=
    op_clean = trimws(Operator),
    op_clean = dplyr::recode(op_clean,
                             "≥" = ">=", "≤" = "<=", "=" = "==", "=>" = ">=", "=<" = "<="
    ),
    
    # Raw values for potential string-equality comparisons
    score_raw = Score,
    goal_raw  = Goal,
    
    # Numerics (pull first numeric token)
    score_num = parse_num(Score),
    goal_num  = parse_num(Goal),
    
    # Percent handling: if Goal shows %, compare in 0–100 space
    score_num = dplyr::case_when(
      is_percent(goal_raw) & !is.na(score_num) & score_num <= 1 ~ score_num * 100,
      TRUE ~ score_num
    ),
    
    # For text equality goals (e.g., "Pass"), allow == to compare strings
    meets_string = dplyr::case_when(
      op_clean == "==" ~ tolower(trimws(as.character(score_raw))) ==
        tolower(trimws(as.character(goal_raw))),
      TRUE ~ NA
    ),
    
    # Final meets_goal logic (numeric first; fall back to string equality)
    meets_goal = dplyr::case_when(
      !is.na(score_num) & !is.na(goal_num) & op_clean == ">=" ~ score_num >= goal_num,
      !is.na(score_num) & !is.na(goal_num) & op_clean == ">"  ~ score_num >  goal_num,
      !is.na(score_num) & !is.na(goal_num) & op_clean == "<=" ~ score_num <= goal_num,
      !is.na(score_num) & !is.na(goal_num) & op_clean == "<"  ~ score_num <  goal_num,
      !is.na(meets_string) ~ meets_string,
      TRUE ~ NA
    )
  )

phase0_prepped <- phase0_prepped %>%
  mutate(
    Goal  = ifelse(grepl("^0?\\.\\d+$", Goal),  paste0(as.numeric(Goal)  * 100, "%"), Goal),
    Score = ifelse(grepl("^0?\\.\\d+$", Score), paste0(as.numeric(Score) * 100, "%"), Score)
  )


# 4) Build gt with Option A (icon tooltip) + Score coloring
tbl <- phase0_prepped %>%
  select(
    `Outcome Measure`,
    Operator, Goal, Score, meets_goal
  ) %>%
  gt() %>%
  cols_label(
    `Outcome Measure` = "Outcome Measure",
    #`Display Description` = "Display Description",
    #Info = "",
    Goal = "Goal",
    Operator = "",
    Score = "Score"
  ) %>%
  cols_align(align = "center", columns = c(Goal, Operator, Score)) %>%
  ak_gt_theme3() %>%
  # cols_width(
  #   `Outcome Measure` ~ px(260),
  #   Goal ~ px(90),
  #   Operator ~ px(60),
  #   Score ~ px(100)
  # ) %>%
  #tab_options(table.font.size = px(14), data_row.padding = px(6)) %>%
  # Color Score cell by pass/fail/NA
  tab_style(
    style = list(
      cell_fill(color = "#E8F5E9"),   # light green
      cell_text(color = "#1B5E20", weight = "600")
    ),
    locations = cells_body(columns = Score, rows = meets_goal %in% TRUE)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#FFF9C4"),   # light yellow
      cell_text(color = "#7A6A00", weight = "600")
    ),
    locations = cells_body(columns = Score, rows = meets_goal %in% FALSE)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#ECEFF1"),   # light grey for NA/uncomparable
      cell_text(color = "#455A64")
    ),
    locations = cells_body(columns = Score, rows = is.na(meets_goal))
  ) %>%
  # Hide helper column
  cols_hide(columns = meets_goal)



tbl





















# ---- Fake "today" values + baseline for jump height ----
outcome1 <- tibble::tibble(
  Variable = "Passive Knee Extension",
  Description = "Supine with a long arm goniometer (Norkin & White, 1995).  Bony landmarks: greater trochanter, the lateral femoral condyle, and the lateral mallelous.",
  Goal = "0º",
  Score    = 2
)

outcome2 <- tibble::tibble(
  Variable = "Passive Knee Flexion",
  Description = "Supine with a long arm goniometer (Norkin & White, 1995).  Bony landmarks: greater trochanter, the lateral femoral condyle, and the lateral mallelous.",
  Goal = "125+",
  Score    = 100
)

phase0_outcomes <- bind_rows(outcome1, outcome2)


phase0_criteria <- readxl::read_xlsx("sample_data/acl-protocol-criteria-2025.xlsx", sheet="Phase0")


# Build a hover-tooltip table (HTML output)
phase0_criteria %>%
  select(`Outcome Measure`, `Display Description`, `Additional Information`) %>%
  mutate(
    Info = paste0(
      "<span title='", htmltools::htmlEscape(`Additional Information`), "'>&#9432;</span>"
    )
  ) %>%
  select(`Outcome Measure`, `Display Description`, Info) %>%
  gt() %>%
  fmt_markdown(columns = Info) %>%     # render the HTML icon
  cols_label(
    `Outcome Measure` = "Outcome Measure",
    `Display Description` = "Display Description",
    Info = "Details"
  ) %>%
  cols_width(
    `Outcome Measure` ~ px(260),
    `Display Description` ~ px(500),
    Info ~ px(80)
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = px(14),
    data_row.padding = px(6)
  )






