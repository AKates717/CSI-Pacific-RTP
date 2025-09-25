# app.R
library(shiny)
library(readxl)
library(openxlsx)
library(DT)
library(here)

#========================
# Paths & constants
#========================
SOURCE_PATH <- here("sample_data", "acl-protocol-criteria-2025.xlsx")  # your uploaded workbook
SOURCE_SHEET <- "criteria_full"                     # sheet to read measures from

TARGET_PATH  <- "outcome_data.xlsx"                 # where entries are saved

SHEET_P0     <- "Phase0_data"
SHEET_P1     <- "Phase1_data"

COLS <- c("Outcome Measure","Date","Side","Value","Units","Notes")

#========================
# Helpers
#========================
empty_df <- function() {
  data.frame(
    "Outcome Measure" = character(),
    "Date"            = as.Date(character()),
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
    addWorksheet(wb, SHEET_P1)
    writeData(wb, SHEET_P0, empty_df(), colNames = TRUE)
    writeData(wb, SHEET_P1, empty_df(), colNames = TRUE)
    saveWorkbook(wb, TARGET_PATH, overwrite = TRUE)
  } else {
    # If exists, ensure both sheets exist with headers
    wb <- loadWorkbook(TARGET_PATH)
    existing_sheets <- names(wb)
    if (!(SHEET_P0 %in% existing_sheets)) {
      addWorksheet(wb, SHEET_P0); writeData(wb, SHEET_P0, empty_df(), colNames = TRUE)
    }
    if (!(SHEET_P1 %in% existing_sheets)) {
      addWorksheet(wb, SHEET_P1); writeData(wb, SHEET_P1, empty_df(), colNames = TRUE)
    }
    saveWorkbook(wb, TARGET_PATH, overwrite = TRUE)
  }
}

read_current_data <- function(sheet_name) {
  if (!file.exists(TARGET_PATH)) return(empty_df())
  df <- tryCatch(readWorkbook(TARGET_PATH, sheet = sheet_name), error = function(e) empty_df())
  if (nrow(df) == 0) return(empty_df())
  missing <- setdiff(COLS, names(df))
  if (length(missing)) df[missing] <- lapply(missing, function(x) rep(NA, nrow(df)))
  df[, COLS, drop = FALSE]
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
  
  # Normalize column names (lowercase, trim)
  nm <- trimws(tolower(names(df)))
  
  # Try to detect relevant columns
  col_phase   <- which(nm %in% c("phase","phase ", "phase_number","phase_no"))
  col_measure <- which(nm %in% c("outcome measure","outcome_measure","measure","measure_name"))
  col_units   <- which(nm %in% c("units","unit","default units","default_units"))
  
  # Fail-safe: if measure missing, return empty lookup
  if (length(col_measure) == 0) {
    warning("No 'Outcome Measure' column found in criteria_full.")
    return(data.frame(measure = character(), units = character(), phase = character(), stringsAsFactors = FALSE))
  }
  
  # Pull columns (units/phase may be missing)
  m <- as.character(df[[col_measure[1]]])
  u <- if (length(col_units)) as.character(df[[col_units[1]]]) else rep("", length(m))
  p_raw <- if (length(col_phase)) df[[col_phase[1]]] else rep(NA, length(m))
  
  # Normalize phase labels to "Phase 0" / "Phase 1" when possible
  norm_phase <- function(x) {
    x_chr <- trimws(tolower(as.character(x)))
    ifelse(grepl("(^|\\D)0(\\D|$)", x_chr) | grepl("phase\\s*0", x_chr), "Phase 0",
           ifelse(grepl("(^|\\D)1(\\D|$)", x_chr) | grepl("phase\\s*1", x_chr), "Phase 1", NA))
  }
  
  p <- norm_phase(p_raw)
  
  out <- data.frame(
    measure = trimws(m),
    units   = ifelse(is.na(u), "", trimws(u)),
    phase   = p,
    stringsAsFactors = FALSE
  )
  
  # Drop empty measures, de-dupe
  out <- out[nzchar(out$measure), , drop = FALSE]
  out <- unique(out)
  
  out
}

MEASURES <- load_measure_lookup()

measures_by_phase <- function(phase_label) {
  if (nrow(MEASURES) == 0) return(character())
  hits <- MEASURES$measure[is.na(MEASURES$phase) | MEASURES$phase == phase_label]
  sort(unique(hits))
}

units_for_measure <- function(measure) {
  if (nrow(MEASURES) == 0) return(NA_character_)
  u <- MEASURES$units[match(measure, MEASURES$measure)]
  if (length(u) == 0) NA_character_ else ifelse(is.na(u) | !nzchar(u), NA_character_, u)
}

#========================
# UI
#========================
ui <- navbarPage(
  title = "Outcome Entry → Excel",
  tabPanel(
    "Phase 0",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "measure_p0", "Outcome Measure (Phase 0)",
          choices  = measures_by_phase("Phase 0"),
          selected = character(0),
          options  = list(
            placeholder = "Select or type…",
            create = TRUE,
            onInitialize = I('function() { this.clear(true); }')  # <- force no preselect
          )
        ),
        dateInput("date_p0", "Date"),
        selectInput("side_p0", "Side", choices = c("involved","uninvolved")),
        numericInput("value_p0", "Value", value = NA, step = 0.01),
        textInput("units_p0", "Units", value = ""),
        textAreaInput("notes_p0", "Notes", rows = 3, placeholder = "optional"),
        actionButton("save_p0", "Save to Excel (Phase 0)", class = "btn-primary"),
        tags$hr(),
        verbatimTextOutput("status_p0", placeholder = TRUE)
      ),
      mainPanel(
        h4("Current Phase 0 data"),
        DTOutput("table_p0")
      )
    )
  ),
  tabPanel(
    "Phase 1",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "measure_p1", "Outcome Measure (Phase 1)",
          choices  = measures_by_phase("Phase 1"),
          selected = character(0),
          options  = list(
            placeholder = "Select or type…",
            create = TRUE,
            onInitialize = I('function() { this.clear(true); }')  # <- force no preselect
          )
        ),
        dateInput("date_p1", "Date"),
        selectInput("side_p1", "Side", choices = c("involved","uninvolved")),
        numericInput("value_p1", "Value", value = NA, step = 0.01),
        textInput("units_p1", "Units", value = ""),
        textAreaInput("notes_p1", "Notes", rows = 3, placeholder = "optional"),
        actionButton("save_p1", "Save to Excel (Phase 1)", class = "btn-primary"),
        tags$hr(),
        verbatimTextOutput("status_p1", placeholder = TRUE)
      ),
      mainPanel(
        h4("Current Phase 1 data"),
        DTOutput("table_p1")
      )
    )
  )
)

#========================
# Server
#========================
server <- function(input, output, session) {
  ensure_workbook()
  
  # Load current data for both phases
  data_p0 <- reactiveVal(read_current_data(SHEET_P0))
  data_p1 <- reactiveVal(read_current_data(SHEET_P1))
  
  output$table_p0 <- renderDT({
    datatable(data_p0(), options = list(pageLength = 10), rownames = FALSE)
  })
  output$table_p1 <- renderDT({
    datatable(data_p1(), options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Auto-fill Units when a known measure is selected/typed (Phase 0)
  observeEvent(input$measure_p0, ignoreInit = TRUE, {
    req(nzchar(input$measure_p0))
    u <- units_for_measure(input$measure_p0)
    if (!is.na(u)) updateTextInput(session, "units_p0", value = u)
  })
  
  # Auto-fill Units when a known measure is selected/typed (Phase 1)
  observeEvent(input$measure_p1, ignoreInit = TRUE, {
    req(nzchar(input$measure_p1))
    u <- units_for_measure(input$measure_p1)
    if (!is.na(u)) updateTextInput(session, "units_p1", value = u)
  })
  
  # Save Phase 0
  observeEvent(input$save_p0, {
    validate(
      need(nzchar(input$measure_p0), "Choose or type an outcome measure."),
      need(!is.null(input$date_p0), "Pick a date."),
      need(!is.na(input$value_p0), "Enter a numeric value."),
      need(nzchar(input$units_p0), "Units cannot be blank.")
    )
    
    new_row <- data.frame(
      "Outcome Measure" = input$measure_p0,
      "Date"            = as.Date(input$date_p0),
      "Side"            = input$side_p0,
      "Value"           = as.numeric(input$value_p0),
      "Units"           = input$units_p0,
      "Notes"           = input$notes_p0,
      check.names = FALSE
    )
    
    tryCatch({
      append_row(SHEET_P0, new_row)
      data_p0(read_current_data(SHEET_P0))
      output$status_p0 <- renderText(sprintf(
        "Saved ✔  (%s | %s | %s = %s %s)",
        new_row[["Outcome Measure"]], new_row[["Date"]], new_row[["Side"]],
        new_row[["Value"]], new_row[["Units"]]
      ))
      updateNumericInput(session, "value_p0", value = NA)
      updateTextInput(session, "notes_p0", value = "")
    }, error = function(e) {
      output$status_p0 <- renderText(paste("Error:", e$message))
    })
  })
  
  # Save Phase 1
  observeEvent(input$save_p1, {
    validate(
      need(nzchar(input$measure_p1), "Choose or type an outcome measure."),
      need(!is.null(input$date_p1), "Pick a date."),
      need(!is.na(input$value_p1), "Enter a numeric value."),
      need(nzchar(input$units_p1), "Units cannot be blank.")
    )
    
    new_row <- data.frame(
      "Outcome Measure" = input$measure_p1,
      "Date"            = as.Date(input$date_p1),
      "Side"            = input$side_p1,
      "Value"           = as.numeric(input$value_p1),
      "Units"           = input$units_p1,
      "Notes"           = input$notes_p1,
      check.names = FALSE
    )
    
    tryCatch({
      append_row(SHEET_P1, new_row)
      data_p1(read_current_data(SHEET_P1))
      output$status_p1 <- renderText(sprintf(
        "Saved ✔  (%s | %s | %s = %s %s)",
        new_row[["Outcome Measure"]], new_row[["Date"]], new_row[["Side"]],
        new_row[["Value"]], new_row[["Units"]]
      ))
      updateNumericInput(session, "value_p1", value = NA)
      updateTextInput(session, "notes_p1", value = "")
    }, error = function(e) {
      output$status_p1 <- renderText(paste("Error:", e$message))
    })
  })
}

shinyApp(ui, server)
