function(input, output, session) {
  ensure_workbook()
  
  # Load current data for both phases
  data_p0 <- reactiveVal(read_current_data(SHEET_P0))
  data_p1 <- reactiveVal(read_current_data(SHEET_P1))
  data_p2 <- reactiveVal(read_current_data(SHEET_P2))
  
  
  #Phase 0 ----
  
  
  # Create Output Table
  output$table_p0 <- renderDT({
    datatable(data_p0() %>% arrange(desc(Date)) %>% select(-c(Units,Side)), options = list(pageLength = 5), rownames = FALSE)
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
  
  #Phase 1 ----
  
  # Create Output Table
  output$table_p1 <- renderDT({
    datatable(data_p1(), options = list(pageLength = 5), rownames = FALSE)
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
  
  #Phase 2 ----
  
  # Create Output Table
  output$table_p2 <- renderDT({
    datatable(data_p2(), options = list(pageLength = 5), rownames = FALSE)
  })
  
  
  
  # Auto-fill Units when a known measure is selected/typed (Phase 2)
  observeEvent(input$measure_p2, ignoreInit = TRUE, {
    req(nzchar(input$measure_p2))
    u <- units_for_measure(input$measure_p2)
    if (!is.na(u)) updateTextInput(session, "units_p2", value = u)
  })
  
  
  
  # Save Phase 1
  observeEvent(input$save_p2, {
    validate(
      need(nzchar(input$measure_p2), "Choose or type an outcome measure."),
      need(!is.null(input$date_p2), "Pick a date."),
      need(!is.na(input$value_p2), "Enter a numeric value."),
      need(nzchar(input$units_p2), "Units cannot be blank.")
    )
    
    new_row <- data.frame(
      "Outcome Measure" = input$measure_p2,
      "Date"            = as.Date(input$date_p2),
      "Side"            = input$side_p2,
      "Value"           = as.numeric(input$value_p2),
      "Units"           = input$units_p2,
      "Notes"           = input$notes_p2,
      check.names = FALSE
    )
    
    tryCatch({
      append_row(SHEET_P2, new_row)
      data_p2(read_current_data(SHEET_P2))
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

  
}

