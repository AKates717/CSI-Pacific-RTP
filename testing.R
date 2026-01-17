source("Global.R")
source("plot_functions.R")
source("Themes.R")



plot_ind_phase_outcomes <- function(phase, outcome){
  
  df <- phase %>%
    dplyr::filter(outcome_measure == outcome)
  
  
  p <- df %>%
    ggplot(aes(x = as.Date(date), y = value, fill = side, text = date_ddmmyear2)) +
    geom_col(position = position_dodge(preserve = 'single'), alpha = 0.5) +
    geom_point(data = subset(df, value == 0), aes(fill = side, label = value), shape = 15, position = position_nudge(x = 0.1, y = 0.03)) +
    ak_plot_theme() +
    scale_x_date(breaks = "1 day", date_labels = "%b %d") +
    labs(y = outcome, x = NULL) +
    scale_fill_manual(
      values = c(
        Left  = if (inj_side == "Left") "red"   else "black",
        Right = if (inj_side == "Right") "red"  else "black"
      ),
      guide = "none")
  # scale_colour_manual(
  #   values = setNames(
  #     ifelse(unique(df$side) == inj_side, "red", "black"),
  #     unique(df$side)
  #   ),
  #   guide = "none"
  # )
  
  ggplotly(p, tooltip = c("text", "label")) %>%
    layout(legend = list(orientation = "h", x = 0.3, y = -0.1)) %>%
    style(hovertemplate = paste("<b>%{text}</b> <br><i>Score:</i>  %{y}<extra></extra>"),traces = 1) %>%
    style(hovertemplate = paste("<b>%{label}</b> <br><i>Score:</i>  %{y}<extra></extra>"),traces = 2) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d"))
}



plot_ind_phase_outcomes(outcomes_raw_phase0, "Swelling")

plot_ind_phase_outcomes(outcomes_raw_phase0, "Passive Knee Extension")
plot_ind_phase_outcomes(outcomes_raw_phase0, "Passive Knee Flexion")
plot_ind_phase_outcomes(outcomes_raw_phase0, "Quad Strength")
plot_ind_phase_outcomes(outcomes_raw_phase0, "Quad Strength")
