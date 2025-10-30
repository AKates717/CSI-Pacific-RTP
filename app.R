# app.R
library(shiny)
library(openxlsx)
library(here)
library(shinyWidgets)
library(shinytoastr)

ui <- fluidPage(
  useToastr(),  # enables toast notifications
  
  titlePanel("Daily Wellness"),
  dateInput("date", "Date", value = Sys.Date()),
  numericInput("sleep_hours", "Hours of Sleep", value = NA, min = 0, max = 24, step = 0.5),
  
  radioButtons("fatigue",        "Fatigue",        choices = 1:5, selected = character(0), inline = TRUE),
  radioButtons("muscle_soreness","Muscle Soreness",choices = 1:5, selected = character(0), inline = TRUE),
  radioButtons("stress",         "Stress",         choices = 1:5, selected = character(0), inline = TRUE),
  radioButtons("knee_soreness",  "Knee Soreness",  choices = 1:5, selected = character(0), inline = TRUE),
  
  actionButton("submit", "Save")
)

server <- function(input, output, session) {
  
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
      title   = "Confirm Save",
      text    = "Are you sure you want to save this entry?",
      type    = "warning",
      btn_labels = c("Cancel", "Yes, Save"),
      btn_colors = c("#999999", "#447099")
    )
  })
  
  observeEvent(input$confirm_save, {
    if (!isTRUE(input$confirm_save)) return(NULL)
    
    new_row <- data.frame(
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
      timeOut = 3000,
      closeButton = TRUE
    )
  })
}

shinyApp(ui, server)
