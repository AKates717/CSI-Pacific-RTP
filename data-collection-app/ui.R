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
  header = tags$head(
    tags$link(rel = "stylesheet", href = "app.css")
  ),

  
  nav_panel(
    title = "Daily Wellness",
    
    useToastr(),
    
    tags$style(HTML("
    .wellness-container {
      display:flex; justify-content:center; align-items:flex-start;
      padding:0.6rem 0.5rem 1rem 0.5rem;
    }
    .wellness-card {
      width:100%; max-width:480px;
      background:#fff; border-radius:10px;
      box-shadow:0 2px 8px rgba(0,0,0,0.05);
      padding:1rem 1rem 0.6rem 1rem;
    }
    .card-header h4 {
      font-weight:600; color:#333;
      margin-bottom:0.5rem; font-size:1.1rem;
    }

    /* field blocks tighter */
    .wellness-field {
      background:#fafbfe;
      border:1px solid #e5e7eb;
      border-radius:8px;
      padding:6px 10px;
      margin-bottom:6px;
    }
    .wellness-field .shiny-input-container label {
      display:block;
      font-weight:600;
      font-size:0.9rem;
      color:#333;
      margin-bottom:2px;
    }
    .shiny-input-container { margin-bottom:0; }

    /* numeric/date inputs */
    .wellness-field input.form-control {
      border:1px solid #cbd5e1;
      border-radius:6px;
      padding:4px 6px;
      height:auto;
      font-size:0.9rem;
      box-shadow:none;
    }
    .wellness-field input.form-control:focus {
      border-color:#447099;
      outline:1px solid rgba(68,112,153,.25);
      outline-offset:1px;
    }
    #sleep_hours { width:90px; }

    /* compact radio pills */
    .wellness-field .radio-inline {
      display:inline-flex !important;
      align-items:center;
      justify-content:center;
      white-space:nowrap;
      gap:3px;
      margin:2px 5px 2px 0;
      padding:4px 8px;
      border:1px solid #cbd5e1;
      border-radius:9999px;
      background:#f8fafc;
      color:#1f2937;
      cursor:pointer;
      font-weight:600;
      font-size:0.9rem;
      transition:background .12s,border-color .12s,color .12s;
    }
    .wellness-field .radio-inline:hover {
      background:#eef2f7;
      border-color:#94a3b8;
    }
    .wellness-field .radio-inline input[type='radio'] {
      width:1px;height:1px;opacity:0;position:absolute;pointer-events:none;
    }
    .wellness-field .radio-inline:has(input[type='radio']:checked) {
      background:#447099;
      border-color:#447099;
      color:#fff;
    }

    /* tighter button */
    #submit {
      width:100%; font-weight:600; font-size:0.95rem;
      padding:.4rem; border-radius:6px;
      background:#447099 !important; border:none;
      margin-top:4px;
    }
    #submit:hover { background:#365b7a !important; }

    @media (max-width:600px){
      .wellness-card { padding:0.8rem; }
      .wellness-field { padding:5px 8px; margin-bottom:5px; }
      .wellness-field .radio-inline { padding:5px 8px; }
    }
  ")),
    
    div(
      class = "wellness-container",
      card(
        class = "wellness-card",
        card_header(h4("Daily Wellness Questionnaire")),
        card_body(
          div(class = "wellness-field",
              dateInput("date", "Date", value = Sys.Date())
          ),
          div(class = "wellness-field",
              numericInput("sleep_hours", "Hours of Sleep", value = 8, min = 0, max = 24, step = 0.5)
          ),
          div(class = "wellness-field",
              radioButtons("fatigue", "Fatigue", choices = 1:5, selected = character(0), inline = TRUE)
          ),
          div(class = "wellness-field",
              radioButtons("muscle_soreness", "Muscle Soreness", choices = 1:5, selected = character(0), inline = TRUE)
          ),
          div(class = "wellness-field",
              radioButtons("stress", "Stress", choices = 1:5, selected = character(0), inline = TRUE)
          ),
          div(class = "wellness-field",
              radioButtons("knee_soreness", "Knee Soreness", choices = 1:5, selected = character(0), inline = TRUE)
          ),
          div(style = "margin-top:6px; text-align:center;",
              actionButton("submit", "Submit"))
        )
      )
    )
  ),
  
  
  
  
  
  
  
  
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
        shinyWidgets::radioGroupButtons(
          inputId  = "side_p0",
          label    = "Side",
          choices  = c("Left", "Right"),
          justified = TRUE,          # buttons fill width evenly
          checkIcon = list(yes = icon("check")),   # checkmark on selected
          selected = character(0)
        ),
        numericInput("value_p0", "Value", value = NA, step = 0.01),
        textInput("units_p0", "Units", value = ""),
        textAreaInput("notes_p0", "Notes", rows = 3, placeholder = "optional"),
        actionButton("save_p0", "Save to Excel", class = "btn-primary"),
        tags$hr(),
        verbatimTextOutput("status_p0", placeholder = TRUE)
      ),
      
      
      card(
        class = "ak-card has-stripe accent-primary tight",
        card_body(
          # Heading styled by your CSS (first child inside card-body)
          h3(textOutput("p0_title", container = span)),
          
          # Main text blocks (keep your existing renderText)
          p(class = "desc-text", textOutput("description_p0", container = span)),
          p(class = "info-text",  textOutput("info_p0",        container = span))
          
          # # Optional fields (render nothing if empty)
          # uiOutput("goal_row_p0"),
          # uiOutput("reps_row_p0"),
          # uiOutput("calc_row_p0")
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
        shinyWidgets::radioGroupButtons(
          inputId  = "side_p1",
          label    = "Side",
          choices  = c("Left", "Right"),
          justified = TRUE,          # buttons fill width evenly
          selected = character(0),
          checkIcon = list(yes = icon("check"))   # checkmark on selected
        ),
        numericInput("value_p1", "Value", value = NA, step = 0.01),
        textInput("units_p1", "Units", value = ""),
        textAreaInput("notes_p1", "Notes", rows = 3, placeholder = "optional"),
        actionButton("save_p1", "Save to Excel (Phase 1)", class = "btn-primary"),
        tags$hr(),
        verbatimTextOutput("status_p1", placeholder = TRUE)
      ),
      
      card(
        class = "ak-card has-stripe accent-primary tight",
        card_body(
          # Heading styled by your CSS (first child inside card-body)
          h3(textOutput("p1_title", container = span)),
          
          # Main text blocks (keep your existing renderText)
          p(class = "desc-text", textOutput("description_p1", container = span)),
          p(class = "info-text",  textOutput("info_p1",        container = span))
          
          # # Optional fields (render nothing if empty)
          # uiOutput("goal_row_p0"),
          # uiOutput("reps_row_p0"),
          # uiOutput("calc_row_p0")
        )
      ),
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
        shinyWidgets::radioGroupButtons(
          inputId  = "side_p2",
          label    = "Side",
          choices  = c("Left", "Right"),
          justified = TRUE,          # buttons fill width evenly
          selected = character(0),
          checkIcon = list(yes = icon("check"))   # checkmark on selected
        ),
        numericInput("value_p2", "Value", value = NA, step = 0.01),
        textInput("units_p2", "Units", value = ""),
        textAreaInput("notes_p2", "Notes", rows = 3, placeholder = "optional"),
        actionButton("save_p2", "Save to Excel (Phase 2)", class = "btn-primary"),
        tags$hr(),
        verbatimTextOutput("status_p2", placeholder = TRUE)
      ),
      
      card(
        class = "ak-card has-stripe accent-primary tight",
        card_body(
          # Heading styled by your CSS (first child inside card-body)
          h3(textOutput("p2_title", container = span)),
          
          # Main text blocks (keep your existing renderText)
          p(class = "desc-text", textOutput("description_p2", container = span)),
          p(class = "info-text",  textOutput("info_p2",        container = span))
          
          # # Optional fields (render nothing if empty)
          # uiOutput("goal_row_p0"),
          # uiOutput("reps_row_p0"),
          # uiOutput("calc_row_p0")
        )
      ),
      DTOutput("table_p2")
      
    )
  )
  

  
  
  
  
  
  
  
  
)