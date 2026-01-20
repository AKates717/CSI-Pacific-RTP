source("Global.R")
source("plot_functions.R")
source("Themes.R")


#Plot for outcomes with only one repetition and with a potnetial score of 0 (i.e. knee extension)
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











#ISO Testing

outcome <- "Hamstrings"

#need to load iso_joint from Global.R



plot_iso_magnitude <- function(outcome){
  

df <- iso_joint %>%
  dplyr::filter(test == outcome)

p <-  df %>%
  ggplot(aes(x = as.factor(date_ddmmyear), fill = limb, group = limb, text = date_ddmmyear2)) +
  stat_summary(aes(y = peak_force_kg), fun = "max", geom = "bar", just = 1, width = 0.4, alpha = 0.8, position = position_dodge(width = 0.4)) +
  geom_point(aes(y = peak_force_kg), shape = 21, size = 2, alpha = 0.5, position = position_dodge(width = 0.4)) +
  ak_plot_theme() +
  scale_x_discrete(labels = function(x) format(as.Date(x), "%b %e, %Y")) +
  labs(y = "Peak Force (kg)", x = NULL) +
  scale_fill_manual(
    values = c(
      Left  = if (inj_side == "Left") "red"   else "black",
      Right = if (inj_side == "Right") "red"  else "black"
    ),
    guide = "none")

ggplotly(p)

ggplotly(p, tooltip = c("text")) %>%
  layout(legend = list(orientation = "h", x = 0.3, y = -0.2)) %>%
  style(hovertemplate = paste("<b>%{text}</b> <br><i>Score:</i>  %{y}<extra></extra>"),traces = c(1,2,3,4)) %>%
  config(displaylogo = FALSE) %>%
  config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d"))

}


plot_iso_magnitude("Quads")
plot_iso_magnitude("Hamstrings")
