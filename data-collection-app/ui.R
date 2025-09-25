ui <- page_navbar(
  title = span(
    img(src='csi.png', 
        style = "margin-bottom: 10px; padding-right: 0px; padding-bottom: 0px;", 
        height =25),
    "ACL Rehab Data Collection"
  ),
  window_title = "ACL Rehab Data Collection",
  theme = custom_theme,
  
  nav_panel(
    "Phase 0",
    layout_sidebar(
      sidebar = sidebar(
        width = "35%",
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
        actionButton("save_p0", "Save to Excel", class = "btn-primary"),
        tags$hr(),
        verbatimTextOutput("status_p0", placeholder = TRUE)
      ),
      
        h4("Recently Collected Data"),
        card(DTOutput("table_p0"))
      
    )
  ),
  
  
  
  nav_panel(
    "Phase 1",
    layout_sidebar(
      sidebar = sidebar(
        width = "35%",
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
     
        h4("Recently Collected Data"),
        DTOutput("table_p1")
      
    )
  ),
  
  
  
  nav_panel(
    "Phase 2",
    layout_sidebar(
      sidebar = sidebar(
        width = "35%",
        selectizeInput(
          "measure_p2", "Outcome Measure (Phase 2)",
          choices  = measures_by_phase("Phase 2"),
          selected = character(0),
          options  = list(
            placeholder = "Select or type…",
            create = TRUE,
            onInitialize = I('function() { this.clear(true); }')  # <- force no preselect
          )
        ),
        dateInput("date_p2", "Date"),
        selectInput("side_p2", "Side", choices = c("involved","uninvolved")),
        numericInput("value_p2", "Value", value = NA, step = 0.01),
        textInput("units_p2", "Units", value = ""),
        textAreaInput("notes_p2", "Notes", rows = 3, placeholder = "optional"),
        actionButton("save_p2", "Save to Excel (Phase 2)", class = "btn-primary"),
        tags$hr(),
        verbatimTextOutput("status_p2", placeholder = TRUE)
      ),
      
      h4("Recently Collected Data"),
      DTOutput("table_p2")
      
    )
  )
)