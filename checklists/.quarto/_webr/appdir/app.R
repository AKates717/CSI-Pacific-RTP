#knitr::opts_knit$set(root.dir = normalizePath(".."))

source("Global.R")
source("plot_functions.R")
source("Themes.R")





ui <- fluidPage(
  #sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
  selectInput("outcome", "Select Outcome Measure:", choices = c("fatigue", "stress"), selected = "fatigue"),
  plotlyOutput("distPlot")
)



server <- function(input, output) {
  
  # output$distPlot <- renderPlot({
  #   x <- faithful$waiting
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   hist(x, breaks = bins, col = "#75AADB", border = "white")
  # })
  
  daily_mon_raw <- read_xlsx("sample_data/outcome_data.xlsx", sheet = "daily") %>%
    clean_names()
  
  output$distPlot <- renderPlotly({
    
    metric <- input$outcome
    
    v1 <- daily_mon_raw %>%
      transmute(
        date,
        value = .data[[metric]]
      )
    
    
    n <- seq_along(v1$metric) #just creating a sequence, essentially a row number
    m <- cumsum(v1$metric) /n #this one plus the one above just does a cummean I guess
    m2 <- cumsum(v1$metric * v1$metric) / n #
    v <- (m2 - m * m) * (n / (n - 1)) #
    s <- sqrt(v) #st dev
    CoV <- s/m #Cov
    
    v1$CoV <- CoV
    
    v1 <- v1 %>%
      mutate(Upper = round(avg + (avg * CoV),2),
             Lower = round(avg - (avg * CoV),2)) %>%
      mutate(Flag = case_when(
        metric < Lower ~ "Decrease",
        metric > Upper ~ "Increase",
        TRUE ~ "Normal"
      )) %>%
      mutate(Colour = case_when(
        Flag == "Normal" ~ "gray40",
        Flag == "Decrease" ~ "#009E73",
        Flag == "Increase" ~ "#D55E00"
      ))
    
    p_metric <- v1 %>%
      mutate(date = as.Date(date)) %>%          # or as.Date(timestamp)
      ggplot(aes(x = date, y = metric)) +
      geom_col(fill = v1$Colour, alpha = 0.9, width = 0.8) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), size = 1, width = 0.5) +
      geom_point(aes(y = avg), size = 2, shape = 23, fill = "#0072B2", stroke = .5) +
      geom_text(aes(label = metric), position = position_stack(vjust = .5), angle = 90, fontface = "bold", color = "white", size = 3) +
      scale_x_date(breaks = "1 month", labels = label_date("%b")) +
      scale_y_continuous(limits = c(0, 5), oob = oob_squish) +
      ak_plot_theme() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()
      )
    
    ggplotly(p_metric)
    
    
  })
  
  
}






shinyApp(ui = ui, server = server)
