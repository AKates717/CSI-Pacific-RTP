library(shiny)
library(tidyverse)
library(readxl)
library(openxlsx)
library(DT)
library(here)
library(bslib)
library(janitor)

#
# Themes & Formatting ----
#



custom_theme <- bslib::bs_theme(
  version = 5,
  preset = "bootstrap",
  spacer = "0.5rem",
  # bg = "#FFF",
  # fg = "#000000",
  primary = "#000000"
)



#
# Paths & constants ----
#
SOURCE_PATH <- here("sample_data", "acl-protocol-criteria-2025.xlsx")  # your uploaded workbook
SOURCE_SHEET <- "criteria_full"                     # sheet to read measures from

TARGET_PATH  <-here("sample_data", "outcome_data.xlsx")

SHEET_P0     <- "Phase0_data"

COLS <- c("Phase","Outcome Measure","Date", "Timestamp","Side","Value","Units","Notes")

#
# Helpers ----
#
empty_df <- function() {
  data.frame(
    "Phase"           = numeric(),
    "Outcome Measure" = character(),
    "Date"            = as.Date(character()),
    "Timestamp"       = character(),
    "Side"            = character(),
    "Value"           = numeric(),
    "Units"           = character(),
    "Notes"           = character(),
    check.names = FALSE
  )
}

ensure_workbook <- function() {
  # Create the target workbook with both phase sheets if missing
  if (!file.exists(TARGET_PATH)) {
    wb <- createWorkbook()
    addWorksheet(wb, SHEET_P0)
    
    writeData(wb, SHEET_P0, empty_df(), colNames = TRUE)
    
    saveWorkbook(wb, TARGET_PATH, overwrite = TRUE)
  } else {
    # If exists, ensure both sheets exist with headers
    wb <- loadWorkbook(TARGET_PATH)
    existing_sheets <- names(wb)
    if (!(SHEET_P0 %in% existing_sheets)) {
      addWorksheet(wb, SHEET_P0); writeData(wb, SHEET_P0, empty_df(), colNames = TRUE)
    }
    saveWorkbook(wb, TARGET_PATH, overwrite = TRUE)
  }
}

read_current_data <- function(sheet_name) {
    df <- openxlsx::read.xlsx(TARGET_PATH, sheet = sheet_name, check.names = FALSE)
  df
}

append_row <- function(sheet_name, row_df) {
  wb <- loadWorkbook(TARGET_PATH)
  existing <- tryCatch(readWorkbook(TARGET_PATH, sheet = sheet_name), error = function(e) NULL)
  n_existing <- if (is.null(existing)) 0 else nrow(existing)
  start_row <- n_existing + 2  # +1 for header, then first empty row
  writeData(wb, sheet = sheet_name, x = row_df, startRow = start_row, colNames = FALSE, keepNA = TRUE)
  saveWorkbook(wb, TARGET_PATH, overwrite = TRUE)
}

#---- Load measure lookup from your criteria_full sheet (robust to header variants)
load_measure_lookup <- function() {
  if (!file.exists(SOURCE_PATH)) {
    warning("Source Excel not found: ", SOURCE_PATH)
    return(data.frame(measure = character(), units = character(), phase = character(), stringsAsFactors = FALSE))
  }
  df <- tryCatch(read_excel(SOURCE_PATH, sheet = SOURCE_SHEET, .name_repair = "minimal"),
                 error = function(e) NULL)
  if (is.null(df)) {
    warning("Could not read sheet '", SOURCE_SHEET, "' from ", SOURCE_PATH)
    return(data.frame(measure = character(), units = character(), phase = character(), stringsAsFactors = FALSE))
  }
  
  # Column picking (no renaming or sorting; keep sheet order)
  nm <- trimws(tolower(names(df)))
  col_phase   <- which(nm %in% c("phase","phase ","phase_number","phase_no"))
  col_measure <- which(nm %in% c("outcome measure","outcome_measure","measure","measure_name"))
  col_units   <- which(nm %in% c("units","unit","default units","default_units"))
  
  if (!length(col_measure)) {
    warning("No 'Outcome Measure' column found in criteria_full.")
    return(data.frame(measure = character(), units = character(), phase = character(), stringsAsFactors = FALSE))
  }
  
  m <- as.character(df[[col_measure[1]]])
  u <- if (length(col_units)) as.character(df[[col_units[1]]]) else rep("", length(m))
  p_raw <- if (length(col_phase)) df[[col_phase[1]]] else rep(NA, length(m))
  
  normalize_phase <- function(x) {
    if (is.numeric(x)) return(paste0("Phase ", as.integer(x)))
    x <- trimws(as.character(x))
    if (!nzchar(x)) return(NA_character_)
    # If any digits appear, use "Phase <first number>"
    d <- regmatches(x, regexpr("\\d+", x))
    if (length(d) && nzchar(d)) return(paste0("Phase ", d))
    # Otherwise, keep a clean label (e.g., "All")
    tolower_first <- tolower(x)
    if (tolower_first %in% c("all", "any")) return(tools::toTitleCase(tolower_first))
    tools::toTitleCase(x)
  }
  
  p <- vapply(p_raw, normalize_phase, character(1))
  
  out <- data.frame(
    measure = trimws(m),
    units   = ifelse(is.na(u), "", trimws(u)),
    phase   = p,
    stringsAsFactors = FALSE
  )
  
  # Drop empties, de-dup while preserving first appearance (sheet order)
  out <- out[nzchar(out$measure), , drop = FALSE]
  out <- out[!duplicated(out[c("measure","phase")]), , drop = FALSE]
  
  out
}


MEASURES <- load_measure_lookup()






measures_by_phase <- function(phase_label) {
  if (nrow(MEASURES) == 0) return(character())
  hits <- MEASURES$measure[
    !is.na(MEASURES$phase) &
      (MEASURES$phase == phase_label | tolower(MEASURES$phase) %in% c("all", "any"))
  ]
  hits[!duplicated(hits)]
}


units_for_measure <- function(measure) {
  if (nrow(MEASURES) == 0) return(NA_character_)
  u <- MEASURES$units[match(measure, MEASURES$measure)]
  if (length(u) == 0) NA_character_ else ifelse(is.na(u) | !nzchar(u), NA_character_, u)
}


