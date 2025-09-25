ui <- page_navbar(
  title = span(
    img(src='csi.png', 
        style = "margin-bottom: 10px; padding-right: 0px; padding-bottom: 0px;", 
        height =25),
    "ACL Rehab Data Collection"
  ),
  window_title = "ACL Rehab Data Collection",
  theme = custom_theme,
  # global card styles (once)
  tags$head(tags$style(HTML("
.ak-card { border: 1px solid var(--bs-border-color); border-left: 4px solid var(--bs-primary);
  border-radius: 12px; box-shadow: 0 2px 10px rgba(0,0,0,.05); }
.ak-card .card-header { background: var(--bs-gray-100); font-weight: 600; padding: .5rem .75rem; }
.ak-card .card-body { padding: .75rem .9rem; }
.ak-section-title { margin: 0; font-size: .95rem; font-weight: 600; }
.desc-text, .info-text { white-space: pre-wrap; line-height: 1.3; margin: 0; }
.ak-sep { border-top: 1px solid var(--bs-border-color); margin: .5rem 0; }
")))
  ,
  
  
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
      
      
      card(
        class = "ak-card",
        card_header(uiOutput("p0_card_title"), class = "py-2 px-3"),
        card_body(
          class = "py-2 px-3",
          tags$div(class = "d-flex flex-column gap-2",
                   tags$div("Display Description", class = "ak-section-title"),
                   div(class = "desc-text", textOutput("description_p0", container = span)),
                   div(class = "ak-sep"),
                   tags$div("Additional Information", class = "ak-section-title"),
                   div(class = "info-text", textOutput("info_p0", container = span))
          )
        )
      ),
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
  )
)