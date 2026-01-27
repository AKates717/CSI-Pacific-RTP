function(input, output, session) {
  ensure_workbook()
  
  # simple df for displaying testing/collection instructions
  criteria_simple <- readxl::read_excel(SOURCE_PATH, sheet = SOURCE_SHEET, .name_repair = "minimal") |>
    dplyr::select(
      `Outcome Measure`,
      `Display Description`,
      `Additional Information`
    ) |>
    dplyr::mutate(`Outcome Measure` = trimws(as.character(`Outcome Measure`)))
  
  # Load current data for both phases
  data_p0 <- reactiveVal(read_current_data(TARGET_SHEET) %>%
                           filter(Phase == 0))
  data_p1 <- reactiveVal(read_current_data(TARGET_SHEET) %>%
                           filter(Phase == 1))
  data_p2 <- reactiveVal(read_current_data(TARGET_SHEET) %>%
                           filter(Phase == 2))
  data_p3 <- reactiveVal(read_current_data(TARGET_SHEET) %>%
                           filter(Phase == 3))
  
  
  
  
  
  
  
  
  # Daily ----
  
  observeEvent(input$submit, {
    # ✅ Validate all fields
    missing_fields <- c()
    
    if (is.null(input$date) || is.na(input$date)) missing_fields <- c(missing_fields, "Date")
    if (is.null(input$sleep_hours) || is.na(input$sleep_hours)) missing_fields <- c(missing_fields, "Hours of Sleep")
    if (is.null(input$fatigue) || input$fatigue == "") missing_fields <- c(missing_fields, "Fatigue")
    if (is.null(input$muscle_soreness) || input$muscle_soreness == "") missing_fields <- c(missing_fields, "Muscle Soreness")
    if (is.null(input$stress) || input$stress == "") missing_fields <- c(missing_fields, "Stress")
    if (is.null(input$knee_soreness) || input$knee_soreness == "") missing_fields <- c(missing_fields, "Knee Soreness")
    
    if (length(missing_fields) > 0) {
      toastr_warning(
        title = "Missing fields",
        message = paste("Please complete all fields before saving."),
        position = "bottom-right",
        progressBar = TRUE,
        timeOut = 3000,
        closeButton = TRUE
      )
      return(NULL)
    }
    
    # ✅ If all complete, ask for confirmation
    ask_confirmation(
      session = session,
      inputId = "confirm_save",
      title   = "Confirm",
      text    = "Please confirm your submission",
      type    = "question",
      btn_labels = c("Cancel", "Submit"),
      btn_colors = c("#999999", "#447099")
    )
  })
  
  observeEvent(input$confirm_save, {
    if (!isTRUE(input$confirm_save)) return(NULL)
    
    new_row <- data.frame(
      Timestamp       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Date            = as.character(input$date),
      hours_of_sleep  = as.numeric(input$sleep_hours),
      fatigue         = as.integer(input$fatigue),
      muscle_soreness = as.integer(input$muscle_soreness),
      stress          = as.integer(input$stress),
      knee_soreness   = as.integer(input$knee_soreness),
      stringsAsFactors = FALSE
    )
    
    xlsx_path  <- here("sample_data", "outcome_data.xlsx")
    sheet_name <- "daily"
    
    if (!file.exists(xlsx_path)) {
      wb <- createWorkbook(); addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, new_row, startRow = 1, colNames = TRUE)
      saveWorkbook(wb, xlsx_path, overwrite = TRUE)
    } else {
      wb <- loadWorkbook(xlsx_path)
      if (!(sheet_name %in% names(wb))) {
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet_name, new_row, startRow = 1, colNames = TRUE)
        saveWorkbook(wb, xlsx_path, overwrite = TRUE)
      } else {
        existing <- tryCatch(readWorkbook(wb, sheet = sheet_name),
                             warning = function(w) NULL, error = function(e) NULL)
        if (is.null(existing) || !is.data.frame(existing) || ncol(existing) == 0) {
          writeData(wb, sheet_name, new_row, startRow = 1, colNames = TRUE)
        } else {
          next_row <- nrow(existing) + 2
          writeData(wb, sheet_name, new_row, startRow = next_row, colNames = FALSE)
        }
        saveWorkbook(wb, xlsx_path, overwrite = TRUE)
      }
    }
    
    # ✅ Success toast (auto-closes)
    toastr_success(
      message = "Entry saved successfully!",
      title = "Wellness Log Updated",
      position = "bottom-right",
      progressBar = TRUE,
      timeOut = 1000,
      closeButton = TRUE
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #Phase 0 ----
  
  #displaying testing/collection instructions
  output$description_p0 <- renderText({
    req(input$measure_p0)
    m <- trimws(input$measure_p0)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No description available.")
    val <- row$`Display Description`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No description available."
    else gsub("\r\n?", "\n", val)   # normalize CR/LF so CSS pre-wrap works
  })
  
  output$info_p0 <- renderText({
    req(input$measure_p0)
    m <- trimws(input$measure_p0)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No additional information.")
    val <- row$`Additional Information`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No additional information."
    else gsub("\r\n?", "\n", val)
  })
  
  
  output$p0_card_title <- renderUI({
    m <- input$measure_p0
    if (is.null(m) || !nzchar(m)) {
      tags$em("Select an outcome measure")
    } else {
      tags$div(class = "d-flex align-items-center gap-2", m)
    }
  })
  
  # Title in the card
  output$p0_title <- renderText({
    m <- input$measure_p0
    if (is.null(m) || !nzchar(m)) "Select an outcome measure" else m
  })
  
  # Helper: fetch first matching row for the selected measure
  get_crit_row <- function(measure) {
    req(measure)
    criteria_simple |>
      dplyr::filter(`Outcome Measure` == trimws(measure)) |>
      dplyr::slice(1)
  }
  
  # Helper: emit a <p><strong>Label:</strong> value</p> only if value exists
  emit_row <- function(label, value) {
    if (is.null(value)) return(NULL)
    val_chr <- trimws(as.character(value))
    if (!nzchar(val_chr) || is.na(val_chr)) return(NULL)
    tags$p(tags$strong(paste0(label, ": ")), val_chr)
  }
  
  # Optional rows
  output$goal_row_p0 <- renderUI({
    row <- get_crit_row(input$measure_p0)
    if (!nrow(row)) return(NULL)
    emit_row("Criteria", row$Goal[[1]])
  })
  
  output$reps_row_p0 <- renderUI({
    row <- get_crit_row(input$measure_p0)
    if (!nrow(row)) return(NULL)
    emit_row("Repetitions", row$Repetitions[[1]])
  })
  
  output$calc_row_p0 <- renderUI({
    row <- get_crit_row(input$measure_p0)
    if (!nrow(row)) return(NULL)
    emit_row("Calculation", row$Calculation[[1]])
  })
  
  
  
  # Create Output Table
  output$table_p0 <- renderDT({
    datatable(data_p0() %>% 
                arrange(desc(Timestamp)) %>% 
                select(-c(Phase,Units,Timestamp)) %>%
                mutate(
                  Date = as.Date(Date, origin = "1899-12-30"),
                  Date = format(Date, "%b %d, %Y")
                ),
              options = list(pageLength = 5), rownames = FALSE)
  })
  
  
  # Auto-fill Units when a known measure is selected/typed (Phase 0)
  observeEvent(input$measure_p0, ignoreInit = TRUE, {
    req(nzchar(input$measure_p0))
    u <- units_for_measure(input$measure_p0)
    if (!is.na(u)) updateTextInput(session, "units_p0", value = u)
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
      "Phase"           = 0,
      "Outcome Measure" = input$measure_p0,
      "Date"            = as.Date(input$date_p0),
      "Timestamp"       = Sys.time(),
      "Side"            = input$side_p0,
      "Value"           = as.numeric(input$value_p0),
      "Units"           = input$units_p0,
      "Notes"           = input$notes_p0,
      check.names = FALSE
    )
    
    tryCatch({
      append_row(TARGET_SHEET, new_row)
      data_p0(read_current_data(TARGET_SHEET))
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #Phase 1 ----
  
  #displaying testing/collection instructions
  output$description_p1 <- renderText({
    req(input$measure_p1)
    m <- trimws(input$measure_p1)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No description available.")
    val <- row$`Display Description`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No description available."
    else gsub("\r\n?", "\n", val)   # normalize CR/LF so CSS pre-wrap works
  })
  
  output$info_p1 <- renderText({
    req(input$measure_p1)
    m <- trimws(input$measure_p1)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No additional information.")
    val <- row$`Additional Information`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No additional information."
    else gsub("\r\n?", "\n", val)
  })
  
  
  output$p1_card_title <- renderUI({
    m <- input$measure_p1
    if (is.null(m) || !nzchar(m)) {
      tags$em("Select an outcome measure")
    } else {
      tags$div(class = "d-flex align-items-center gap-2", m)
    }
  })
  
  # Title in the card
  output$p1_title <- renderText({
    m <- input$measure_p1
    if (is.null(m) || !nzchar(m)) "Select an outcome measure" else m
  })
  
  # Helper: fetch first matching row for the selected measure
  get_crit_row <- function(measure) {
    req(measure)
    criteria_simple |>
      dplyr::filter(`Outcome Measure` == trimws(measure)) |>
      dplyr::slice(1)
  }
  
  # Helper: emit a <p><strong>Label:</strong> value</p> only if value exists
  emit_row <- function(label, value) {
    if (is.null(value)) return(NULL)
    val_chr <- trimws(as.character(value))
    if (!nzchar(val_chr) || is.na(val_chr)) return(NULL)
    tags$p(tags$strong(paste0(label, ": ")), val_chr)
  }
  
  # Optional rows
  output$goal_row_p1 <- renderUI({
    row <- get_crit_row(input$measure_p1)
    if (!nrow(row)) return(NULL)
    emit_row("Criteria", row$Goal[[1]])
  })
  
  output$reps_row_p1 <- renderUI({
    row <- get_crit_row(input$measure_p1)
    if (!nrow(row)) return(NULL)
    emit_row("Repetitions", row$Repetitions[[1]])
  })
  
  output$calc_row_p1 <- renderUI({
    row <- get_crit_row(input$measure_p1)
    if (!nrow(row)) return(NULL)
    emit_row("Calculation", row$Calculation[[1]])
  })
  
  # Create Output Table
  output$table_p1 <- renderDT({
    datatable(data_p1() %>%
                filter(Phase == 1) %>%
                arrange(desc(Timestamp)) %>% 
                select(-c(Phase,Units,Timestamp)) %>%
                mutate(
                  Date = as.Date(Date, origin = "1899-12-30"),
                  Date = format(Date, "%b %d, %Y")
                ),
              options = list(pageLength = 5), rownames = FALSE)
  })
  
  
  
  # Auto-fill Units when a known measure is selected/typed (Phase 1)
  observeEvent(input$measure_p1, ignoreInit = TRUE, {
    req(nzchar(input$measure_p1))
    u <- units_for_measure(input$measure_p1)
    if (!is.na(u)) updateTextInput(session, "units_p1", value = u)
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
      "Phase"           = 1,
      "Outcome Measure" = input$measure_p1,
      "Date"            = as.Date(input$date_p1),
      "Timestamp"       = Sys.time(),
      "Side"            = input$side_p1,
      "Value"           = as.numeric(input$value_p1),
      "Units"           = input$units_p1,
      "Notes"           = input$notes_p1,
      check.names = FALSE
    )
    
    tryCatch({
      append_row(TARGET_SHEET, new_row)
      data_p1(read_current_data(TARGET_SHEET))
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
  
  
  
  
  
  
  
  
  
  #Phase 2 ----
  
  #displaying testing/collection instructions
  output$description_p2 <- renderText({
    req(input$measure_p2)
    m <- trimws(input$measure_p2)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No description available.")
    val <- row$`Display Description`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No description available."
    else gsub("\r\n?", "\n", val)   # normalize CR/LF so CSS pre-wrap works
  })
  
  output$info_p2 <- renderText({
    req(input$measure_p2)
    m <- trimws(input$measure_p2)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No additional information.")
    val <- row$`Additional Information`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No additional information."
    else gsub("\r\n?", "\n", val)
  })
  
  
  output$p2_card_title <- renderUI({
    m <- input$measure_p2
    if (is.null(m) || !nzchar(m)) {
      tags$em("Select an outcome measure")
    } else {
      tags$div(class = "d-flex align-items-center gap-2", m)
    }
  })
  
  # Title in the card
  output$p2_title <- renderText({
    m <- input$measure_p2
    if (is.null(m) || !nzchar(m)) "Select an outcome measure" else m
  })
  
  # Helper: fetch first matching row for the selected measure
  get_crit_row <- function(measure) {
    req(measure)
    criteria_simple |>
      dplyr::filter(`Outcome Measure` == trimws(measure)) |>
      dplyr::slice(1)
  }
  
  # Helper: emit a <p><strong>Label:</strong> value</p> only if value exists
  emit_row <- function(label, value) {
    if (is.null(value)) return(NULL)
    val_chr <- trimws(as.character(value))
    if (!nzchar(val_chr) || is.na(val_chr)) return(NULL)
    tags$p(tags$strong(paste0(label, ": ")), val_chr)
  }
  
  # Optional rows
  output$goal_row_p2 <- renderUI({
    row <- get_crit_row(input$measure_p2)
    if (!nrow(row)) return(NULL)
    emit_row("Criteria", row$Goal[[1]])
  })
  
  output$reps_row_p2 <- renderUI({
    row <- get_crit_row(input$measure_p2)
    if (!nrow(row)) return(NULL)
    emit_row("Repetitions", row$Repetitions[[1]])
  })
  
  output$calc_row_p2 <- renderUI({
    row <- get_crit_row(input$measure_p2)
    if (!nrow(row)) return(NULL)
    emit_row("Calculation", row$Calculation[[1]])
  })
  
  # Create Output Table
  output$table_p2 <- renderDT({
    datatable(data_p2() %>% 
                filter(Phase == 2) %>%
                arrange(desc(Timestamp)) %>% 
                select(-c(Phase,Units,Timestamp)) %>%
                mutate(
                  Date = as.Date(Date, origin = "1899-12-30"),
                  Date = format(Date, "%b %d, %Y")
                ),
              options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  
  # Auto-fill Units when a known measure is selected/typed (Phase 1)
  observeEvent(input$measure_p2, ignoreInit = TRUE, {
    req(nzchar(input$measure_p2))
    u <- units_for_measure(input$measure_p2)
    if (!is.na(u)) updateTextInput(session, "units_p2", value = u)
  })
  
  
  
  # Save Phase 2
  observeEvent(input$save_p2, {
    validate(
      need(nzchar(input$measure_p2), "Choose or type an outcome measure."),
      need(!is.null(input$date_p2), "Pick a date."),
      need(!is.na(input$value_p2), "Enter a numeric value."),
      need(nzchar(input$units_p2), "Units cannot be blank.")
    )
    
    new_row <- data.frame(
      "Phase"           = 2,
      "Outcome Measure" = input$measure_p2,
      "Date"            = as.Date(input$date_p2),
      "Timestamp"       = Sys.time(),
      "Side"            =   if (is.null(input$side_p2) || input$side_p2 == "") {
        "DL"
      } else {
        input$side_p2
      },
      "Value"           = as.numeric(input$value_p2),
      "Units"           = input$units_p2,
      "Notes"           = input$notes_p2,
      check.names = FALSE
    )
    
    tryCatch({
      append_row(TARGET_SHEET, new_row)
      data_p2(read_current_data(TARGET_SHEET))
      output$status_p2 <- renderText(sprintf(
        "Saved ✔  (%s | %s | %s = %s %s)",
        new_row[["Outcome Measure"]], new_row[["Date"]], new_row[["Side"]],
        new_row[["Value"]], new_row[["Units"]]
      ))
      updateNumericInput(session, "value_p2", value = NA)
      updateTextInput(session, "notes_p2", value = "")
    }, error = function(e) {
      output$status_p2 <- renderText(paste("Error:", e$message))
    })
  })
  
  
  
  
  
  
  
  
  
  #Phase 3 ----
  
  #displaying testing/collection instructions
  output$description_p3 <- renderText({
    req(input$measure_p3)
    m <- trimws(input$measure_p3)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No description available.")
    val <- row$`Display Description`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No description available."
    else gsub("\r\n?", "\n", val)   # normalize CR/LF so CSS pre-wrap works
  })
  
  output$info_p3 <- renderText({
    req(input$measure_p3)
    m <- trimws(input$measure_p3)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No additional information.")
    val <- row$`Additional Information`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No additional information."
    else gsub("\r\n?", "\n", val)
  })
  
  
  output$p3_card_title <- renderUI({
    m <- input$measure_p3
    if (is.null(m) || !nzchar(m)) {
      tags$em("Select an outcome measure")
    } else {
      tags$div(class = "d-flex align-items-center gap-2", m)
    }
  })
  
  # Title in the card
  output$p3_title <- renderText({
    m <- input$measure_p3
    if (is.null(m) || !nzchar(m)) "Select an outcome measure" else m
  })
  
  # # Helper: fetch first matching row for the selected measure
  # get_crit_row <- function(measure) {
  #   req(measure)
  #   criteria_simple |>
  #     dplyr::filter(`Outcome Measure` == trimws(measure)) |>
  #     dplyr::slice(1)
  # }
  
  # # Helper: emit a <p><strong>Label:</strong> value</p> only if value exists
  # emit_row <- function(label, value) {
  #   if (is.null(value)) return(NULL)
  #   val_chr <- trimws(as.character(value))
  #   if (!nzchar(val_chr) || is.na(val_chr)) return(NULL)
  #   tags$p(tags$strong(paste0(label, ": ")), val_chr)
  # }
  
  # Optional rows
  output$goal_row_p3 <- renderUI({
    row <- get_crit_row(input$measure_p3)
    if (!nrow(row)) return(NULL)
    emit_row("Criteria", row$Goal[[1]])
  })
  
  output$reps_row_p3 <- renderUI({
    row <- get_crit_row(input$measure_p3)
    if (!nrow(row)) return(NULL)
    emit_row("Repetitions", row$Repetitions[[1]])
  })
  
  output$calc_row_p3 <- renderUI({
    row <- get_crit_row(input$measure_p3)
    if (!nrow(row)) return(NULL)
    emit_row("Calculation", row$Calculation[[1]])
  })
  
  # Create Output Table
  output$table_p3 <- renderDT({
    datatable(data_p3() %>% 
                filter(Phase == 3) %>%
                arrange(desc(Timestamp)) %>% 
                select(-c(Phase,Units,Timestamp)) %>%
                mutate(
                  Date = as.Date(Date, origin = "1899-12-30"),
                  Date = format(Date, "%b %d, %Y")
                ),
              options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  
  # Auto-fill Units when a known measure is selected/typed (Phase 1)
  observeEvent(input$measure_p3, ignoreInit = TRUE, {
    req(nzchar(input$measure_p3))
    u <- units_for_measure(input$measure_p3)
    if (!is.na(u)) updateTextInput(session, "units_p3", value = u)
  })
  
  
  
  # Save Phase 3
  observeEvent(input$save_p3, {
    validate(
      need(nzchar(input$measure_p3), "Choose or type an outcome measure."),
      need(!is.null(input$date_p3), "Pick a date."),
      need(!is.na(input$value_p3), "Enter a numeric value."),
      need(nzchar(input$units_p3), "Units cannot be blank.")
    )
    
    new_row <- data.frame(
      "Phase"           = 3,
      "Outcome Measure" = input$measure_p3,
      "Date"            = as.Date(input$date_p3),
      "Timestamp"       = Sys.time(),
      "Side"            =   if (is.null(input$side_p3) || input$side_p3 == "") {
        "DL"
      } else {
        input$side_p3
      },
      "Value"           = as.numeric(input$value_p3),
      "Units"           = input$units_p3,
      "Notes"           = input$notes_p3,
      check.names = FALSE
    )
    
    tryCatch({
      append_row(TARGET_SHEET, new_row)
      data_p3(read_current_data(TARGET_SHEET))
      output$status_p3 <- renderText(sprintf(
        "Saved ✔  (%s | %s | %s = %s %s)",
        new_row[["Outcome Measure"]], new_row[["Date"]], new_row[["Side"]],
        new_row[["Value"]], new_row[["Units"]]
      ))
      updateNumericInput(session, "value_p3", value = NA)
      updateTextInput(session, "notes_p3", value = "")
    }, error = function(e) {
      output$status_p3 <- renderText(paste("Error:", e$message))
    })
  })
  
}

