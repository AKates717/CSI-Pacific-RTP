function(input, output, session) {
  ensure_workbook()
  
  #Set Up ----
  
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
  data_p4 <- reactiveVal(read_current_data(TARGET_SHEET) %>%
                           filter(Phase == 4))
  
  
  
  
  
  
  
  # Daily Mon ----
  
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
    data_p0() %>%
      {
        if (!is.null(input$measure_p0) && input$measure_p0 != "") {
          filter(., Outcome.Measure == input$measure_p0)
        } else {
          .
        }
      } %>%
      arrange(desc(Timestamp)) %>%
      select(-c(Phase, Units, Timestamp)) %>%
      mutate(
        Date = as.Date(Date, origin = "1899-12-30"),
        Date = format(Date, "%b %d, %Y")
      ) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
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
  
  # # Helper: fetch first matching row for the selected measure
  # get_crit_row <- function(measure) {
  #   req(measure)
  #   criteria_simple |>
  #     dplyr::filter(`Outcome Measure` == trimws(measure)) |>
  #     dplyr::slice(1)
  # }
  # 
  # # Helper: emit a <p><strong>Label:</strong> value</p> only if value exists
  # emit_row <- function(label, value) {
  #   if (is.null(value)) return(NULL)
  #   val_chr <- trimws(as.character(value))
  #   if (!nzchar(val_chr) || is.na(val_chr)) return(NULL)
  #   tags$p(tags$strong(paste0(label, ": ")), val_chr)
  # }
  
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
    data_p1() %>%
      {
        if (!is.null(input$measure_p1) && input$measure_p1 != "") {
          filter(., Outcome.Measure == input$measure_p1)
        } else {
          .
        }
      } %>%
      arrange(desc(Timestamp)) %>%
      select(-c(Phase, Units, Timestamp)) %>%
      mutate(
        Date = as.Date(Date, origin = "1899-12-30"),
        Date = format(Date, "%b %d, %Y")
      ) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
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
    }, error_fun = function(e) {
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
  
  # # Helper: fetch first matching row for the selected measure
  # get_crit_row <- function(measure) {
  #   req(measure)
  #   criteria_simple |>
  #     dplyr::filter(`Outcome Measure` == trimws(measure)) |>
  #     dplyr::slice(1)
  # }
  # 
  # # Helper: emit a <p><strong>Label:</strong> value</p> only if value exists
  # emit_row <- function(label, value) {
  #   if (is.null(value)) return(NULL)
  #   val_chr <- trimws(as.character(value))
  #   if (!nzchar(val_chr) || is.na(val_chr)) return(NULL)
  #   tags$p(tags$strong(paste0(label, ": ")), val_chr)
  # }
  
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
    data_p2() %>%
      {
        if (!is.null(input$measure_p2) && input$measure_p2 != "") {
          filter(., Outcome.Measure == input$measure_p2)
        } else {
          .
        }
      } %>%
      arrange(desc(Timestamp)) %>%
      select(-c(Phase, Units, Timestamp)) %>%
      mutate(
        Date = as.Date(Date, origin = "1899-12-30"),
        Date = format(Date, "%b %d, %Y")
      ) %>%
      datatable(options = list(pageLength = 20), rownames = FALSE)
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
        "Both"
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
    data_p3() %>%
      {
        if (!is.null(input$measure_p3) && input$measure_p3 != "") {
          filter(., Outcome.Measure == input$measure_p3)
        } else {
          .
        }
      } %>%
      arrange(desc(Timestamp)) %>%
      select(-c(Phase, Units, Timestamp)) %>%
      mutate(
        Date = as.Date(Date, origin = "1899-12-30"),
        Date = format(Date, "%b %d, %Y")
      ) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
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
        "Both"
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
  
  
  
  
  
  
  
  
  
  #Phase 4 ----
  
  #displaying testing/collection instructions
  output$description_p4 <- renderText({
    req(input$measure_p4)
    m <- trimws(input$measure_p4)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No description available.")
    val <- row$`Display Description`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No description available."
    else gsub("\r\n?", "\n", val)   # normalize CR/LF so CSS pre-wrap works
  })
  
  output$info_p4 <- renderText({
    req(input$measure_p4)
    m <- trimws(input$measure_p4)
    row <- criteria_simple |>
      dplyr::filter(`Outcome Measure` == m) |>
      dplyr::slice(1)
    
    if (nrow(row) == 0) return("No additional information.")
    val <- row$`Additional Information`[[1]]
    if (is.null(val) || is.na(val) || !nzchar(val)) "No additional information."
    else gsub("\r\n?", "\n", val)
  })
  
  
  output$p4_card_title <- renderUI({
    m <- input$measure_p4
    if (is.null(m) || !nzchar(m)) {
      tags$em("Select an outcome measure")
    } else {
      tags$div(class = "d-flex align-items-center gap-2", m)
    }
  })
  
  # Title in the card
  output$p4_title <- renderText({
    m <- input$measure_p4
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
  output$goal_row_p4 <- renderUI({
    row <- get_crit_row(input$measure_p4)
    if (!nrow(row)) return(NULL)
    emit_row("Criteria", row$Goal[[1]])
  })
  
  output$reps_row_p4 <- renderUI({
    row <- get_crit_row(input$measure_p4)
    if (!nrow(row)) return(NULL)
    emit_row("Repetitions", row$Repetitions[[1]])
  })
  
  output$calc_row_p4 <- renderUI({
    row <- get_crit_row(input$measure_p4)
    if (!nrow(row)) return(NULL)
    emit_row("Calculation", row$Calculation[[1]])
  })
  
  # Create Output Table
  output$table_p4 <- renderDT({
    data_p4() %>%
      {
        if (!is.null(input$measure_p4) && input$measure_p4 != "") {
          filter(., Outcome.Measure == input$measure_p4)
        } else {
          .
        }
      } %>%
      arrange(desc(Timestamp)) %>%
      select(-c(Phase, Units, Timestamp)) %>%
      mutate(
        Date = as.Date(Date, origin = "1899-12-30"),
        Date = format(Date, "%b %d, %Y")
      ) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  
  
  # Auto-fill Units when a known measure is selected/typed (Phase 4)
  observeEvent(input$measure_p4, ignoreInit = TRUE, {
    req(nzchar(input$measure_p4))
    u <- units_for_measure(input$measure_p4)
    if (!is.na(u)) updateTextInput(session, "units_p4", value = u)
  })
  
  
  
  # Save Phase 4
  observeEvent(input$save_p4, {
    validate(
      need(nzchar(input$measure_p4), "Choose or type an outcome measure."),
      need(!is.null(input$date_p4), "Pick a date."),
      need(!is.na(input$value_p4), "Enter a numeric value."),
      need(nzchar(input$units_p4), "Units cannot be blank.")
    )
    
    new_row <- data.frame(
      "Phase"           = 4,
      "Outcome Measure" = input$measure_p4,
      "Date"            = as.Date(input$date_p4),
      "Timestamp"       = Sys.time(),
      "Side"            =   if (is.null(input$side_p4) || input$side_p4 == "") {
        "Both"
      } else {
        input$side_p4
      },
      "Value"           = as.numeric(input$value_p4),
      "Units"           = input$units_p4,
      "Notes"           = input$notes_p4,
      check.names = FALSE
    )
    
    tryCatch({
      append_row(TARGET_SHEET, new_row)
      data_p4(read_current_data(TARGET_SHEET))
      output$status_p4 <- renderText(sprintf(
        "Saved ✔  (%s | %s | %s = %s %s)",
        new_row[["Outcome Measure"]], new_row[["Date"]], new_row[["Side"]],
        new_row[["Value"]], new_row[["Units"]]
      ))
      updateNumericInput(session, "value_p4", value = NA)
      updateTextInput(session, "notes_p4", value = "")
    }, error = function(e) {
      output$status_p4 <- renderText(paste("Error:", e$message))
    })
  })
  
  
  
  
  
  
  
  
  #Isometric ----
  
  ##Housekeeping ----
  
  #Define directory for csvs
  default_save_dir <- file.path(getwd(), "data")
  
  #timing
  live_start_time <- reactiveVal(NULL)
  timer_armed     <- reactiveVal(FALSE)
  live_elapsed_s  <- reactiveVal(0)
  start_click_time <- reactiveVal(NULL)
  
  #Units
  g <- 9.80665
  to_units <- function(x) {
    if (identical(input$units, "N")) x * g else x
  }
  
  # R-side rolling buffer
  buf <- reactiveVal(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
  buf_fast <- shiny::debounce(reactive(buf()), 150)  # for numbers if you want them faster
  buf_plot <- shiny::debounce(reactive(buf()), 250)  # plot refresh ~4 Hz
  
  # Loading Notification Helpers
  show_connecting_modal <- function() {
    showModal(modalDialog(
      title = "Connecting to Progressor…",
      div(
        class = "d-flex align-items-center gap-3",
        tags$div(class = "spinner-border", role = "status",
                 tags$span(class = "visually-hidden", "Loading...")),
        div("Preparing live stream. Please wait…")
      ),
      footer = NULL,
      easyClose = FALSE
    ))
  }
  
  hide_connecting_modal <- function() {
    removeModal()
  }
  
  show_timeout_modal <- function() {
    showModal(modalDialog(
      title = "Connection timed out",
      "Unable to start streaming data. Please press the button on the device to 
     wake it, then reload the browser page.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  }
  
  
  
  
  
  
  
  
  
  ##Start Button ----
  
  observeEvent(input$start, {
    
    #Loading Notification after button push
    show_connecting_modal()
    start_click_time(Sys.time())
    
    # Require limb before starting
    if (is.null(input$limb) || !nzchar(input$limb)) {
      showModal(modalDialog(
        title = "Select limb",
        "Please select a limb before starting collection.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # # reset full session
    # session_df(data.frame(
    #   t_us = numeric(0),
    #   t_s = numeric(0),
    #   weight_raw_kg = numeric(0),
    #   offset_raw_kg = numeric(0),
    #   weight_adj_kg = numeric(0)
    # ))
    
    # reset full session (chunks)
    session_chunks(list())
    session_n(0L)
    
    # (optional) reset peak hold at start of a new session
    peak_hold(NA_real_)
    
    # arm timer, but don't start it yet
    live_start_time(NULL)
    live_elapsed_s(0)
    timer_armed(TRUE)
    
    
    py$start_stream(
      name_prefix = "Progressor_3177",
      auto_tare   = isTRUE(input$auto_tare),
      reset_buffer = TRUE
    )
  })
  
  
  
  
  
  
  
  
  
  
  ##Stop Button ----
  observeEvent(input$stop, {
    py$stop_stream()
    
    # Freeze timer at the current elapsed time
    if (!is.null(live_start_time())) {
      live_elapsed_s(as.numeric(difftime(Sys.time(), live_start_time(), units = "secs")))
    }
    
    # Prevent further updates
    live_start_time(NULL)
    timer_armed(FALSE)
    start_click_time(NULL)   # if you use this
    # hide_connecting_modal()  # if present
  })
  
  
  
  
  
  
  
  
  
  ##Clear Data ----
  observeEvent(input$clear, {
    buf(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
    peak_hold(NA_real_)
    offset(0)
    
    # Reset timer to zero on Clear
    live_elapsed_s(0)
    live_start_time(NULL)
    timer_armed(FALSE)
    start_click_time(NULL)   # if you use this
    
    # Reset summary table (peaks cleared)
    summary_tbl_data(data.frame(
      Phase = input$phase_num,
      Test  = input$test_type,
      Limb  = if (is.null(input$limb) || input$limb == "") NA_character_ else input$limb,
      `Peak Force (kg)` = NA_real_,
      `Peak Force (N)`  = NA_real_,
      check.names = FALSE
    ))
    
  })
  
  
  
  
  
  
  
  # Full session storage (not trimmed)
  # session_df <- reactiveVal(data.frame(
  #   t_us = numeric(0),
  #   t_s = numeric(0),
  #   weight_raw_kg = numeric(0),
  #   offset_raw_kg = numeric(0),
  #   weight_adj_kg = numeric(0)
  # ))
  
  # Full session storage as list-of-chunks (fast appends)
  session_chunks <- reactiveVal(list())
  session_n <- reactiveVal(0L)  # optional: quick row count without binding
  
  
  
  
  
  
  
  ##Peak Hold ----
  peak_hold <- reactiveVal(NA_real_)
  
  observeEvent(input$reset_peak, {
    peak_hold(NA_real_)
  })
  
  # observeEvent(input$clear, {
  #   buf(data.frame(t_us=numeric(0), weight=numeric(0), t_s=numeric(0)))
  #   peak_hold(NA_real_)
  # })
  
  
  
  
  ##Offset ----
  offset <- reactiveVal(0)
  
  observeEvent(input$zero_now, {
    df <- buf()
    if (nrow(df) >= 1) {
      offset(df$weight[nrow(df)])
      peak_hold(NA_real_)  # optional: reset peak hold after zero
    }
  })
  
  output$offset_status <- renderText({
    sprintf("Software offset: %.3f %s", to_units(offset()), input$units)
  })
  
  
  
  
  ## Poll python buffer on a timer ----
  observe({
    invalidateLater(125, session)
    if (!isTRUE(py$is_streaming())) return()
    
    # grab new samples (and clear python-side buffer)
    samps <- py$get_samples(clear = TRUE)
    
    # Hide modal when first data arrives
    if (isTRUE(timer_armed()) && length(samps) > 0) {
      hide_connecting_modal()
    }
    
    # Timeout if no data after 10 seconds
    if (isTRUE(timer_armed()) &&
        !is.null(start_click_time()) &&
        length(samps) == 0) {
      
      elapsed <- as.numeric(difftime(Sys.time(), start_click_time(), units = "secs"))
      
      if (elapsed > 10) {
        hide_connecting_modal()
        show_timeout_modal()
        timer_armed(FALSE)
        start_click_time(NULL)
      }
    }
    
    # If we are armed and the first samples just arrived, start the timer now
    if (isTRUE(timer_armed()) && is.null(live_start_time()) && length(samps) > 0) {
      live_start_time(Sys.time())
      timer_armed(FALSE)
    }
    
    # update "time live" while streaming
    if (!is.null(live_start_time())) {
      live_elapsed_s(as.numeric(difftime(Sys.time(), live_start_time(), units = "secs")))
    }
    
    if (length(samps) > 0) {
      newdf <- data.frame(
        t_us   = vapply(samps, function(x) x[[1]], numeric(1)),
        weight = vapply(samps, function(x) x[[2]], numeric(1))
      )
      newdf$t_s <- newdf$t_us / 1e6
      
      # ---- append to FULL session (raw kg + offset info) 
      off_kg <- offset()
      
      sess_new <- data.frame(
        t_us = newdf$t_us,
        t_s  = newdf$t_s,
        weight_raw_kg = newdf$weight,
        offset_raw_kg = rep(off_kg, nrow(newdf)),
        weight_adj_kg = newdf$weight - off_kg
      )
      
      #session_df(rbind(session_df(), sess_new))
      
      # append chunk (cheap)
      chunks <- session_chunks()
      chunks[[length(chunks) + 1]] <- sess_new
      session_chunks(chunks)
      
      # update running count (optional)
      session_n(session_n() + nrow(sess_new))
      
      
      
      #
      cur <- buf()
      out <- rbind(cur, newdf)
      
      # keep last ~20 seconds of data
      if (nrow(out) > 2) {
        tmax <- max(out$t_s)
        out <- out[out$t_s >= (tmax - 20), , drop = FALSE]
      }
      buf(out)
      
      # update peak hold
      new_peak_raw <- max(out$weight - offset(), na.rm = TRUE)
      ph <- peak_hold()
      if (is.na(ph) || new_peak_raw > ph) peak_hold(new_peak_raw)
      
    }
  }, priority = 1000)
  
  
  
  
  
  
  
  
  
  ##Value Boxes ----
  output$vb_time_value <- renderText({
    sprintf("%d s", as.integer(floor(live_elapsed_s())))
  })
  
  
  output$vb_current_value <- renderText({
    df <- buf_fast()
    if (nrow(df) < 1) return("—")
    sprintf("%.1f %s", round(to_units((df$weight - offset())[nrow(df)]), 1), input$units)
  })
  
  output$vb_peak_value <- renderText({
    ph <- peak_hold()
    if (is.na(ph)) return("—")
    sprintf("%.1f %s", round(to_units(ph), 1), input$units)
  })
  
  
  
  
  
  
  
  
  ##Main Plot ----
  output$plot <- renderPlot({
    
    df <- buf_plot()
    validate(need(nrow(df) >= 2, "No live data yet. Click Start Button."))
    
    plot_df <- df %>%
      mutate(
        force = to_units(weight - offset())
      )
    
    # peak-hold in display units (stored raw)
    ph_raw <- peak_hold()
    ph_disp <- if (is.na(ph_raw)) NA_real_ else to_units(ph_raw)
    
    ggplot(plot_df, aes(x = t_s, y = force)) +
      geom_line(linewidth = 0.8, colour = "#2C3E50") +
      geom_hline(
        yintercept = ph_disp,
        linewidth = 0.8,
        colour = if (is.na(ph_disp)) NA else "#C0392B",
        linetype = "dashed"
      ) +
      labs(
        x = "Time (s)",
        y = paste0("Force (", input$units, ")")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10),
        plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA)
      )
  }, bg = "white")
  
  
  
  
  
  
  
  
  
  ## Main Table ----
  #Summary table data (always exists; peaks filled on Calculate)
  summary_tbl_data <- reactiveVal(data.frame(
    Name = NA_character_,
    Test = NA_character_,
    Limb = NA_character_,
    `Peak Force (kg)` = NA_real_,
    `Peak Force (N)`  = NA_real_,
    check.names = FALSE
  ))
  
  # Initialize + keep Name/Test/Limb up to date as user changes inputs
  observe({
    # keep existing peak values (if already calculated)
    cur <- summary_tbl_data()
    
    summary_tbl_data(data.frame(
      Phase = input$phase_num,
      Test = input$test_type,
      Limb = if (is.null(input$limb) || input$limb == "") NA_character_ else input$limb,
      `Peak Force (kg)` = cur$`Peak Force (kg)`,
      `Peak Force (N)`  = cur$`Peak Force (N)`,
      check.names = FALSE
    ))
  })
  
  # When Calculate is clicked, fill peak values using current peak_hold()
  observeEvent(input$calc_summary, {
    cur <- summary_tbl_data()
    ph_raw <- peak_hold()
    
    summary_tbl_data(data.frame(
      Phase = cur$Phase,
      Test = cur$Test,
      Limb = cur$Limb,
      `Peak Force (kg)` = if (is.na(ph_raw)) NA_real_ else round(ph_raw, 2),
      `Peak Force (N)`  = if (is.na(ph_raw)) NA_real_ else round(ph_raw * g, 1),
      check.names = FALSE
    ))
  })
  
  #GT
  output$summary_tbl <- gt::render_gt({
    df <- summary_tbl_data()
    req(nrow(df) == 1)
    
    gt::gt(df) |>
      gt::sub_missing(columns = everything(), missing_text = "—") |>
      gt::cols_align(align = "left", columns = c(Phase, Test, Limb)) |>
      gt::cols_align(align = "right", columns = c(`Peak Force (kg)`, `Peak Force (N)`)) |>
      gt::tab_options(
        table.font.size = gt::px(14),
        data_row.padding = gt::px(6)
      ) |>
      ak_gt_theme3() |>
      gt::cols_align(align = "center")
  })
  
  
  
  
  
  
  
  ##Save Raw Data ----
  save_msg <- reactiveVal("")
  
  observeEvent(input$save_csv, {
    chunks <- session_chunks()
    if (length(chunks) == 0) {
      save_msg("Nothing to save yet.")
      return()
    }
    df <- do.call(rbind, chunks)
    if (nrow(df) < 1) {
      save_msg("Nothing to save yet.")
      return()
    }
    
    dir <- normalizePath(default_save_dir, winslash = "/", mustWork = FALSE)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    nm   <- slugify(input$athlete_name)
    limb <- slugify(input$limb)
    test <- slugify(input$test_type)
    
    fname <- paste(stamp, nm, limb, test, sep = "_")
    path  <- file.path(dir, paste0(fname, ".csv"))
    
    # --- add Newton columns at save time ---
    df_out <- df %>%
      mutate(
        weight_raw_N = weight_raw_kg * g,
        weight_adj_N = weight_adj_kg * g
      )
    
    write.csv(df_out, path, row.names = FALSE)
    
    # Show full path so you can find it easily
    save_msg(paste("Saved:", fname))
  })
  
  output$save_status <- renderText(save_msg())
  
  
  
  
  
  
  ##Save Summary Data ----
  
  #helper to get the append working properly
  ensure_trailing_newline <- function(path) {
    if (!file.exists(path)) return(invisible(TRUE))
    sz <- file.info(path)$size
    if (is.na(sz) || sz < 1) return(invisible(TRUE))
    
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    seek(con, where = sz - 1, origin = "start")
    last <- readChar(con, nchars = 1, useBytes = TRUE)
    
    if (!identical(last, "\n")) {
      cat("\n", file = path, append = TRUE)
    }
    invisible(TRUE)
  }
  
  db_msg <- reactiveVal("")
  
  output$db_status <- renderText(db_msg())
  
  observeEvent(input$save_db, {
    
    # 1) Get the current table row
    df <- summary_tbl_data()   # <- THIS is your 1-row summary data.frame
    req(nrow(df) == 1)
    
    # 2) Require limb selected
    if (is.na(df$Limb) || !nzchar(df$Limb)) {
      showModal(modalDialog(
        title = "Missing selection",
        "Please select a limb before saving.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # 3) Require Calculate has been run (i.e., peak values exist)
    if (is.na(df$`Peak Force (kg)`) || is.na(df$`Peak Force (N)`)) {
      showModal(modalDialog(
        title = "Not calculated",
        "Please click Calculate before saving to the database.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # 4) Add timestamp
    out <- df
    out$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    
    # 5) Write/append to data/tindeq_results.csv
    dir.create("data", showWarnings = FALSE, recursive = TRUE)
    path <- file.path("data", "tindeq_results.csv")
    
    if (!file.exists(path)) {
      write.csv(out, path, row.names = FALSE)
    } else {
      ensure_trailing_newline(path)  # <-- ADD THIS LINE
      write.table(
        out, path,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE,
        eol = "\n"
      )
    }
    
    
    db_msg("Saved to database")
    
    # auto-clear after 3 seconds
    later::later(function() {
      db_msg("")
    }, delay = 3)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  output$status <- renderText({
    streaming <- py$is_streaming()
    df <- buf()
    if (nrow(df) >= 2) {
      duration <- max(df$t_s) - min(df$t_s)
      hz <- (nrow(df) - 1) / duration
      sprintf("Streaming: %s\nPoints in plot: %d\nWindow: %.1f s\nObserved Hz: %.2f",
              streaming, nrow(df), duration, hz)
    } else {
      sprintf("Streaming: %s\nPoints in plot: %d", streaming, nrow(df))
    }
  })
  
  
  # Windowed data for metrics (last N seconds)
  metric_df <- reactive({
    df <- buf()
    validate(need(nrow(df) >= 1, NULL))
    
    win <- input$metric_window
    if (is.null(win) || !is.finite(win) || win <= 0) win <- 2
    
    tmax <- max(df$t_s)
    df[df$t_s >= (tmax - win), , drop = FALSE]
  })
  
  
  
  
  
}

