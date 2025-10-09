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
  data_p0 <- reactiveVal(read_current_data(SHEET_P0))
  data_p1 <- reactiveVal(read_current_data(SHEET_P1))
  
  
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

