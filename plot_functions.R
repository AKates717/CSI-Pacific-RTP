#1. Custom Themes ----
#ak_plot_theme
ak_plot_theme <- function(..., base_size = 12) {
  theme(
    text = element_text(size = base_size),
    axis.ticks = element_blank(),
    axis.title = element_text(color = "black",
                              face = "bold"),
    axis.text = element_text(color = "black",
                             face = "bold"),
    axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8),
    plot.title.position = "plot",
    plot.title = element_text(size = 16,
                              #face = "bold",
                              color = "black",
                              vjust = 5),
    plot.subtitle = element_text(color = "black",
                                 hjust = 0.5),
    plot.caption = element_text(size = 8,
                                face = "italic",
                                color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#e0e0e0"),
    panel.grid.major.x = element_blank(), #maybe..
    panel.background = element_rect(fill = "#ffffff"),
    plot.background = element_blank(),
    panel.border = element_blank())
}



#2. Stat Summary Functions ----
#this is a function for the stat_summary pointrange geom
mean_peak <- function(x) {
  data.frame(y = (mean(x) + 0.0001),
             ymax = max(x),
             ymin = mean(x))
}

#95%CI calculation.. calculating t-value because of small sample sizes
#t-value*(sd/sqrt(n))
mean_95CI <- function (x) {
  data.frame(y = (mean(x) + 0.001),
  ymax = mean(x) + qt(p = .05/2, df=length(x)-1, lower.tail = FALSE) *(sd(x)/sqrt(length(x))),
  ymin= mean(x) - qt(p = .05/2, df=length(x)-1, lower.tail = FALSE) *(sd(x)/sqrt(length(x))))
}
###



#3a. stat_summary_plot ----
#plot change in mean across n most recent dates + visualize all jumps, with max value highlighted, plus track real changes using TEM
stat_summary_plot <- function (nm_data, y, colour1, tt_helper, title) { #clunky but need the tt_helper for a weird tooltip bug
 
  TEM_df <- nm_data %>%
    group_by(date_ddmmyear) %>%
    dplyr::summarise(mean = mean({{y}})) %>%
    mutate(diff = c(0,diff(mean)))

  TEM <- sqrt((sum(TEM_df$diff)^2) / (2 * nrow(TEM_df)))
  
  TEM_df <- TEM_df %>%
    mutate(real = ifelse(abs(diff) > TEM, "*", NA))
  
  p <- ggplot(nm_data, aes(x = factor(date_ddmmyear, ordered = T), text = date_ddmmyear2)) +
    geom_hline(aes(yintercept = mean({{y}})), alpha = 0.3, linetype = 2, linewidth = 1.5) + #prob want this to plot the all time average
    stat_summary(aes(y = {{y}}), fun.data=mean_peak, geom="pointrange", size = 4, color = colour1) +
    stat_summary(aes(y = {{y}}), fun = "mean", geom = "point", size = 4, color = colour1) +
    stat_summary(aes(y = {{y}}), fun = "max", geom = "point", size = 2, color = colour1) +
    stat_summary(aes(y = {{y}}, group = 5), fun = "mean", geom = "line", linewidth = 1, color = colour1) +
    geom_jitter(aes(y = {{y}}), shape = 1, size = 2, alpha = 0.3, width = 0.2) +
    geom_text(data = TEM_df, aes(x = factor(date_ddmmyear, ordered = T), y = mean, label = real), inherit.aes = FALSE, size = 5, vjust = -0.5, nudge_x = 0.1) +
    ak_plot_theme() +
    #theme(axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8)) +
    #axis.text.y = element_text(size = 6)) +
    # annotate(geom = "text",
    #          x = factor(nm_data$date_ddmmyear),
    #          y = 0,
    #          label = nm_data$date_ddmmyear2,
    #          vjust = 3.5) +   #this adds the format that I want but messes with entire chart
    # theme(axis.text.x = element_blank()) +
    labs(
      x = NULL,
      y = NULL,
      title = title)
  
  with_options(list(digits = 4), ggplotly(p, tooltip = c("text"))) %>%
    style(hovertemplate = "<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>",traces = 3) %>%
    style(text = paste(nm_data$x1, "\n",
                       tt_helper), traces = 5) %>%
    style(hoverinfo = "skip", traces = c(1,2,4)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d")) %>%
    style(textposition = "top") %>% # For the geom_text to scale properly with all metrics
    layout(margin = list(t = 10, b = 0, l = 0))
    #config(scrollZoom = TRUE)
}
###


#3b. stat_summary_plot2 ----
#edits for secondary objectives
stat_summary_plot2 <- function (nm_data, y, tt_helper, title) { #clunky but need the tt_helper for a weird tooltip bug
  p <- nm_data %>%
    ggplot(aes(x = factor(date_ddmmyear), text = date_ddmmyear2)) +
    geom_hline(aes(yintercept = mean(.data[[y]])), alpha = 0.3, linetype = 2, linewidth = 1.5) + #problem here..?
    stat_summary(aes(y = .data[[y]]), fun.data=mean_peak, geom="pointrange", size = 4, color="#5C5C6B") +
    stat_summary(aes(y = .data[[y]]), fun = "mean", geom = "point", size = 4, color = "#5C5C6B") +
    stat_summary(aes(y = .data[[y]]), fun = "max", geom = "point", size = 2, color="#5C5C6B") +
    stat_summary(aes(y = .data[[y]], group = 5), fun = "mean", geom = "line", linewidth = 1, color="#5C5C6B") +
    geom_jitter(aes(y = .data[[y]]), shape = 1, size = 2, alpha = 0.3, width = 0.2) +
    ak_plot_theme() +
    #theme(axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8)) +
          #axis.text.y = element_text(size = 12)) +
    labs(x = NULL, y = NULL, title = title)
  
  with_options(list(digits = 4), ggplotly(p, tooltip = c("text"))) %>%
    style(hovertemplate = "<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>",traces = 3) %>%
    style(text = paste(nm_data$x1, "\n",
                       tt_helper), traces = 5) %>%
    style(hoverinfo = "skip", traces = c(2,4)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d")) %>%
    layout(margin = list(t = 0, b = 0, l = 0))
}

#3c. stat_summary_plot 3 ----
#Regression Line
stat_summary_plot3 <- function (nm_data, y, colour1, tt_helper, title) { #clunky but need the tt_helper for a weird tooltip bug
  p <- nm_data %>%
    ggplot(aes(x = factor(date_ddmmyear, ordered = T), text = date_ddmmyear2)) +
    geom_line(stat = "smooth", method = "lm", aes(y = {{y}}, group = 1), fullrange = T,  alpha = 0.4, linetype = 2, linewidth = 1.5, color = "#5C5C6B") +
    stat_summary(aes(y = {{y}}), fun.data=mean_peak, geom="pointrange", size = 4, color = colour1) +
    stat_summary(aes(y = {{y}}), fun = "mean", geom = "point", size = 4, color = colour1) +
    stat_summary(aes(y = {{y}}), fun = "max", geom = "point", size = 2, color = colour1) +
    stat_summary(aes(y = {{y}}, group = 5), fun = "mean", geom = "line", linewidth = 1, color = colour1) +
    geom_jitter(aes(y = {{y}}), shape = 1, size = 2, alpha = 0.3, width = 0.2) +
    ak_plot_theme() +
    #theme(axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8)) +
          #axis.text.y = element_text(size = 12)) +
    # annotate(geom = "text",
    #          x = factor(nm_data$date_ddmmyear),
    #          y = 0,
    #          label = nm_data$date_ddmmyear2,
    #          vjust = 3.5) +   #this adds the format that I want but messes with entire chart
    # theme(axis.text.x = element_blank()) +
    labs(
      x = NULL,
      y = NULL,
      title = title)
  
  with_options(list(digits = 4), ggplotly(p, tooltip = c("text"))) %>%
    style(text = paste(nm_data$x1, "\n",
                       tt_helper), traces = 5) %>%
    
    style(
      hovertemplate = "<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>",
      traces = c(3)
    ) %>%
    style(hoverinfo = "skip", traces = c(1,2,4)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d"))  #%>%
  #config(scrollZoom = TRUE)
}


#3d. stat_summary_plot 4 ----
#95%CI i.e. for when we dont care about the max value but more about the spread i.e. TTS
stat_summary_plot4 <- function (nm_data, y, colour1, tt_helper, title) { #clunky but need the tt_helper for a weird tooltip bug
  
  TEM_df <- nm_data %>%
    group_by(date_ddmmyear) %>%
    dplyr::summarise(mean = mean({{y}})) %>%
    mutate(diff = c(0,diff(mean)))
  
  TEM <- sqrt((sum(TEM_df$diff)^2) / (2 * nrow(TEM_df))) # 0.423
  
  TEM_df <- TEM_df %>%
    mutate(real = ifelse(abs(diff) > TEM, "*", NA))
  
  p <- ggplot(nm_data, aes(x = factor(date_ddmmyear, ordered = T), text = date_ddmmyear2)) +
    geom_hline(aes(yintercept = mean({{y}})), alpha = 0.3, linetype = 2, linewidth = 1.5) + #prob want this to plot the all time average
    stat_summary(aes(y = {{y}}), fun.data=mean_95CI, geom="pointrange", size = 4, color = colour1) +
    stat_summary(aes(y = {{y}}), fun = "mean", geom = "point", size = 4, color = colour1) +
    stat_summary(aes(y = {{y}}, group = 5), fun = "mean", geom = "line", linewidth = 1, color = colour1) +
    geom_jitter(aes(y = {{y}}), shape = 1, size = 2, alpha = 0.3, width = 0.2) +
    geom_text(data = TEM_df, aes(x = factor(date_ddmmyear, ordered = T), y = mean, label = real), inherit.aes = FALSE, size = 5, vjust = -0.5, nudge_x = 0.1) +
    ak_plot_theme() +
    #theme(axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8)) +
    labs(
      x = NULL,
      y = NULL,
      title = title)
  
  with_options(list(digits = 4), ggplotly(p, tooltip = c("text"))) %>%
    style(hovertemplate = "<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>",traces = 3) %>%
    style(text = paste(nm_data$x1, "\n",
                       tt_helper), traces = 4) %>%
    style(hoverinfo = "skip", traces = c(2)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d")) %>%
    style(textposition = "top") %>% # For the geom_text to scale properly with all metrics
    layout(margin = list(t = 10, b = 0, l = 0))
}

#3d. Stat_Summary_Highlight ... kind of works
stat_sum_highlight <- function(data, y, colour, var, title){
  TEM_df <- data %>%
    group_by(date_ddmmyear) %>%
    dplyr::summarise(mean = mean(.data[[var]])) %>%
    mutate(diff = c(0,diff(mean)))
  
  TEM <- sqrt((sum(TEM_df$diff)^2) / (2 * nrow(TEM_df))) # 0.423
  
  TEM_df <- TEM_df %>%
    mutate(real = ifelse(abs(diff) > TEM, "*", NA))
  
  d <- highlight_key(data, ~y)
  
  p <- ggplot(d, aes(x = factor(date_ddmmyear, ordered = T), text = date_ddmmyear2)) +
    geom_hline(aes(yintercept = mean(y)), alpha = 0.3, linetype = 2, linewidth = 1.5) + #prob want this to plot the all time average
    stat_summary(aes(y = y), fun.data=mean_peak, geom="pointrange", size = 4, color = colour) +
    stat_summary(aes(y = y), fun = "mean", geom = "point", size = 4, color = colour) +
    stat_summary(aes(y = y), fun = "max", geom = "point", size = 2, color = colour) +
    stat_summary(aes(y = y, group = 5), fun = "mean", geom = "line", linewidth = 1, color = colour) +
    geom_jitter(aes(y = y), shape = 1, size = 2, alpha = 0.3, width = 0.2) +
    geom_text(data = TEM_df, aes(x = factor(date_ddmmyear, ordered = T), y = mean, label = real), inherit.aes = FALSE, size = 5, vjust = -0.5, nudge_x = 0.1) +
    ak_plot_theme() +
    #theme(axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8)) +
    labs(x = NULL, y = NULL, title = title)
  
  ggg <-  with_options(list(digits = 4), ggplotly(p, tooltip = c("text"))) %>%
    style(hovertemplate = "<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>",traces = 3) %>%
    style(text = paste(data$x1, "\n",
                       y), traces = 5) %>%
    style(hoverinfo = "skip", traces = c(1,2,4)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d")) %>%
    style(textposition = "top") %>% # For the geom_text to scale properly with all metrics
    layout(margin = list(t = 10, b = 0, l = 0))
  
  highlight(ggg, on = "plotly_click", off = "plotly_doubleclick")
}


#3c. Symmetry plots  ----
#(percentage asymmetry)
BA_plot_percent <- function (BA_data, y1, y2, colour1, colour2) {
  p1 <- BA_data %>%
  ggplot(aes(x = factor(date_ddmmyear), text = date_ddmmyear2)) +
    geom_hline(yintercept = 0, alpha = 0.2, linetype = 2, linewidth = 1.5, color = "green") +
    geom_hline(yintercept = -15, alpha = 0.2, linetype = 2, linewidth = 1.5, color = "red") +
    geom_hline(yintercept = 15, alpha = 0.2, linetype = 2, linewidth = 1.5, color = "red") +
    annotate(geom = 'text', x = tail(factor(BA_data$date_ddmmyear, ordered = F), 1), y = -26, label = "R", alpha = 0.1, size = 20) +
    annotate(geom = 'text', x = tail(factor(BA_data$date_ddmmyear, ordered = F), 1), y = 21, label = "L", alpha = 0.1, size = 20) +
    stat_summary(aes(y = {{y1}}), fun.data=mean_95CI, geom="pointrange", position = position_nudge(x = -0.05), size = 3, color= colour1, alpha = 0.8) +
    stat_summary(aes(y = {{y1}}, group = 5), fun = "mean", geom = "line", linewidth = 1 , position = position_nudge(x = -0.05),color= colour1, alpha = 0.3) +
    stat_summary(aes(y = {{y2}}), fun.data=mean_95CI, geom="pointrange", position = position_nudge(x = 0.05), size = 3, color= colour2, alpha = 0.8) +
    stat_summary(aes(y = {{y2}}, group = 5), fun = "mean", geom = "line", linewidth = 1, position = position_nudge(x = 0.05), color= colour2, alpha = 0.3) +
    geom_point(aes(y = {{y1}}, color = {{y1}}), shape = 1, size = 2, alpha = 0.5, color = colour1, position = position_jitternudge(nudge.from = "jittered", width = 0.05,  x = -0.15)) +
    geom_jitter(aes(y = {{y2}}, color = {{y2}}), shape = 1, size = 2, alpha = 0.5, color = colour2, position = position_jitternudge(nudge.from = "jittered", width = 0.05,  x = 0.15)) +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,5), oob = scales::squish) +
    #ylim((0-max(abs(c(BA_data$balng, BA_data$basht)))),(0+max(abs(c(BA_data$balng, BA_data$basht))))) +
    ak_plot_theme() +
    theme(#axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8),
          #axis.text.y = element_text(size = 12),
          title = element_text(size = 16, face = "bold"),
          panel.grid.major =  element_line(color = "#f7f7f7")) +
    labs(
      x = NULL,
      y = NULL)
  
  ggplotly(p1, tooltip = c("text")) %>%
    style(hovertemplate = paste("<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>", "%", ifelse(mean(BA_data$balng) > 0, "L", "R")),traces = 7) %>%
    style(hovertemplate = paste("<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>", "%", ifelse(mean(BA_data$basht) > 0, "L", "R")),traces = 9) %>%
    style(text = paste(BA_data$x1, "\n", "ECC: ", BA_data$balng, "%", ifelse(BA_data$balng > 0, "L", "R")), traces = 10) %>%
    style(text = paste(BA_data$x1, "\n", "CON: ", BA_data$basht, "%", ifelse(BA_data$basht > 0, "L", "R")), traces = 11) %>%
    style(hoverinfo = "skip", traces = c(6,8)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d"))
}
###


BA_plot_percent_sj <- function (BA_data, y1, y2, colour1, colour2) {
  p1 <- BA_data %>%
    ggplot(aes(x = factor(date_ddmmyear), text = date_ddmmyear2)) +
    geom_hline(yintercept = 0, alpha = 0.2, linetype = 2, linewidth = 1.5, color = "green") +
    geom_hline(yintercept = -15, alpha = 0.2, linetype = 2, linewidth = 1.5, color = "red") +
    geom_hline(yintercept = 15, alpha = 0.2, linetype = 2, linewidth = 1.5, color = "red") +
    stat_summary(aes(y = {{y1}}), fun.data=mean_95CI, geom="pointrange", position = position_nudge(x = -0.05), size = 3, color= colour1, alpha = 0.8) +
    stat_summary(aes(y = {{y1}}, group = 5), fun = "mean", geom = "line", linewidth = 1 , position = position_nudge(x = -0.05),color= colour1, alpha = 0.3) +
    stat_summary(aes(y = {{y2}}), fun.data=mean_95CI, geom="pointrange", position = position_nudge(x = 0.05), size = 3, color= colour2, alpha = 0.8) +
    stat_summary(aes(y = {{y2}}, group = 5), fun = "mean", geom = "line", linewidth = 1, position = position_nudge(x = 0.05), color= colour2, alpha = 0.3) +
    geom_point(aes(y = {{y1}}, color = {{y1}}), shape = 1, size = 2, alpha = 0.5, color = colour1, position = position_jitternudge(nudge.from = "jittered", width = 0.05,  x = -0.15)) +
    geom_jitter(aes(y = {{y2}}, color = {{y2}}), shape = 1, size = 2, alpha = 0.5, color = colour2, position = position_jitternudge(nudge.from = "jittered", width = 0.05,  x = 0.15)) +
    scale_y_continuous(limits = c(-30,30), breaks = seq(-30,30,5), oob = scales::squish) +
    #ylim((0-max(abs(c(BA_data$balng, BA_data$basht)))),(0+max(abs(c(BA_data$balng, BA_data$basht))))) +
    ak_plot_theme() +
    theme(#axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8),
          #axis.text.y = element_text(size = 12),
          title = element_text(size = 16, face = "bold"),
          panel.grid.major =  element_line(color = "#f7f7f7")) +
    labs(
      x = NULL,
      y = NULL)
  
  ggplotly(p1, tooltip = c("text")) %>%
    style(hovertemplate = paste("<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>", "%", ifelse(mean(BA_data$basht) > 0, "L", "R")),traces = 5) %>%
    style(hovertemplate = paste("<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>", "%", ifelse(mean(BA_data$baland) > 0, "L", "R")),traces = 7) %>%
    style(text = paste(BA_data$x1, "\n", "CON: ", BA_data$basht, "%", ifelse(BA_data$basht > 0, "L", "R")), traces = 8) %>%
    style(text = paste(BA_data$x1, "\n", "LAND: ", BA_data$baland, "%", ifelse(BA_data$baland > 0, "L", "R")), traces = 9) %>%
    style(hoverinfo = "skip", traces = c(4,6)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d"))
}
###


#BALand
BALand_plot_percent <- function (BA_data, y1, title) {
  p2 <- BA_data %>%
    ggplot(aes(x = factor(date_ddmmyear), text = date_ddmmyear2)) +
    geom_hline(yintercept = 0, alpha = 0.3, linetype = 2, linewidth = 1.5, color = "green") +
    geom_hline(yintercept = -15, alpha = 0.3, linetype = 2, linewidth = 1.5, color = "red") +
    geom_hline(yintercept = 15, alpha = 0.3, linetype = 2, linewidth = 1.5, color = "red") +
    stat_summary(aes(y = {{y1}}), fun.data=mean_95CI, geom="pointrange", size = 5, color="#5C5C6B", alpha = 1.0) +
    stat_summary(aes(y = {{y1}}), fun = "mean", geom = "point", size = 4, color = "#5C5C6B") +
    stat_summary(aes(y = {{y1}}, group = 5), fun = "mean", geom = "line", linewidth = 1, color="#5C5C6B", alpha = 0.3) +
    geom_jitter(aes(y = {{y1}}, color = {{y1}}), shape = 1, size = 3, color = "#5C5C6B", alpha = 0.3, width = 0.1) +
    scale_y_continuous(limits = c(-25,25), breaks = seq(-25,25,5)) +
    ak_plot_theme() +
    theme(#axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8),
          #axis.text.y = element_text(size = 12),
          title = element_text(size = 16, face = "bold")) +
    labs(
      x = NULL,
      y = NULL,
      title = title)
  
  ggplotly(p2, tooltip = c("text")) %>%
    style(hovertemplate = paste("<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>", "%", ifelse(mean(BA_data$baland) > 0, "L", "R")),traces = 5) %>%
    style(text = paste("Landing ", BA_data$x1, "\n", BA_data$baland, "%", ifelse(BA_data$baland > 0, "L", "R")), traces = 6) %>%
    style(hoverinfo = "skip", traces = c(4)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d"))
}
###

#3d. Symmetry plots (magnitudes) ----
BA_plot_magnitude <- function (BA_data, y1, y2, color1, color2, tthelper1, tthelper2) {
  y_low <- round(min(c(tthelper1, tthelper2)),-1)
  p1 <- BA_data %>%
    ggplot(aes(x = factor(date_ddmmyear), text = date_ddmmyear2)) +
    stat_summary(aes(y = {{y1}}), fun = "mean", geom = "bar", fill = color1, just = 1, width = 0.4, alpha = 0.7) +
    stat_summary(aes(y = {{y2}}), fun = "mean", geom = "bar", fill = color2, just = 0, width = 0.4, alpha = 0.7) +
    geom_point(aes(y = {{y1}}, color = {{y1}}), shape = 21, size = 2, alpha = 0.5, color = "black", fill = color1, position = position_jitternudge(nudge.from = "jittered", width = 0.15,  x = -0.25)) +
    geom_point(aes(y = {{y2}}, color = {{y2}}), shape = 21, size = 2, alpha = 0.5, color = "black", fill = color2, position = position_jitternudge(nudge.from = "jittered", width = 0.15,  x = 0.25)) +
    ak_plot_theme() +
    theme(#axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8),
          title = element_text(size = 16, face = "bold")) +
    labs(
      x = NULL,
      y = NULL) +
    coord_cartesian(ylim = c(y_low, NA))
  
  with_options(list(digits = 4), ggplotly(p1, tooltip = c("text"))) %>%
    style(hovertemplate = "<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>",traces = c(1,2)) %>%
    style(text = paste(BA_data$x1, "\n",
                       tthelper1), traces = 3) %>%
    style(text = paste(BA_data$x1, "\n",
                       tthelper2), traces = 4) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d"))
}

#3e. Barplot for 1 variable ----
imtp_barplot <- function (BA_data, y1, color1, tthelper1, title) {
  y_low <- round(min(c(tthelper1)),-1)
  p1 <- BA_data %>%
    ggplot(aes(x = factor(date_ddmmyear), text = date_ddmmyear2)) +
    stat_summary(aes(y = {{y1}}), fun = "max", geom = "bar", fill = color1, width = 0.5, alpha = 0.7) +
    geom_point(aes(y = {{y1}}, color = {{y1}}), shape = 21, size = 2, alpha = 0.5, color = "black", fill = color1) +
    ak_plot_theme() +
    theme(#axis.text.x = element_text(angle = 40, hjust =0.6, vjust = 0.8),
          title = element_text(size = 16, face = "bold")) +
    labs(
      x = NULL,
      y = NULL,
      title = title) 
  #+ coord_cartesian(ylim = c(y_low, NA))
  
  with_options(list(digits = 4), ggplotly(p1, tooltip = c("text"))) %>%
    style(hovertemplate = "<b>%{text}</b> <br><i>Max:</i>  %{y}<extra></extra>",traces = c(1)) %>%
    style(text = paste(BA_data$x1, "\n",
                       tthelper1), traces = 2) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d"))
}

#Stat Summary Plot with text showing jump number
stat_summary_plot_text <- function (nm_data, y, colour1, tt_helper, title) { #clunky but need the tt_helper for a weird tooltip bug
  
  TEM_df <- nm_data %>%
    group_by(date_ddmmyear) %>%
    dplyr::summarise(mean = mean({{y}})) %>%
    mutate(diff = c(0,diff(mean)))
  
  TEM <- sqrt((sum(TEM_df$diff)^2) / (2 * nrow(TEM_df)))
  
  TEM_df <- TEM_df %>%
    mutate(real = ifelse(abs(diff) > TEM, "*", NA))
  
  p <- ggplot(nm_data, aes(x = factor(date_ddmmyear, ordered = T), text = date_ddmmyear2)) +
    geom_hline(aes(yintercept = mean({{y}})), alpha = 0.3, linetype = 2, linewidth = 1.5) + #prob want this to plot the all time average
    stat_summary(aes(y = {{y}}), fun.data=mean_peak, geom="pointrange", size = 4, color = colour1) +
    stat_summary(aes(y = {{y}}), fun = "mean", geom = "point", size = 4, color = colour1) +
    stat_summary(aes(y = {{y}}), fun = "max", geom = "point", size = 2, color = colour1) +
    stat_summary(aes(y = {{y}}, group = 5), fun = "mean", geom = "line", linewidth = 1, color = colour1) +
    geom_jitter(aes(y = {{y}}), shape = 1, size = 2, alpha = 0.3, position = position_jitter(seed =1)) +
    geom_text(aes(y = {{y}}, label = Jump), size = 4, alpha = 0.1, position = position_jitter(seed =1)) +
    geom_text(data = TEM_df, aes(x = factor(date_ddmmyear, ordered = T), y = mean, label = real), inherit.aes = FALSE, size = 5, vjust = -0.5, nudge_x = 0.1) +
    ak_plot_theme() +
    labs(
      x = NULL,
      y = NULL,
      title = title)
  
  with_options(list(digits = 4), ggplotly(p, tooltip = c("text"))) %>%
    style(hovertemplate = "<b>%{text}</b> <br><i>Mean:</i>  %{y}<extra></extra>",traces = 3) %>%
    style(text = paste(nm_data$x1, "\n",
                       tt_helper, "cm"), traces = 5) %>%
    style(hoverinfo = "skip", traces = c(1,2,4,6,7)) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = c("hoverCompare", "hoverclosest", "zoomIn2d", "zoomOut2d")) %>%
    style(textposition = "top") %>% # For the geom_text to scale properly with all metrics
    layout(margin = list(t = 10, b = 0, l = 0))
}



#Card Templates ----
plot_card1 <- function(header, output, footer){ #plotly card w/ large header
  card(
    #height = 300,
    class = "border-light",
    full_screen = TRUE,
    card_header(header, class = "h3 bg-transparent border-light"),
    card_body(
      class = "p-0 ",
      plotlyOutput(output)),
    card_footer(footer, class = "bg-transparent border-light")
  )
}

plot_card2 <- function(output){ #plotly card, no header
  card(
    full_screen = TRUE,
    class = "border-light",
    card_body(
      class = "p-0",
    plotlyOutput(output))
  )
}

plot_card3 <- function(header, output){ #plotly card w/ small header
  card(
    full_screen = TRUE,
    class = "border-light",
    card_header(header, class = "h5 bg-transparent border-light"),
    card_body(
      class = "p-0",
      plotlyOutput(output))
  )
}

plot_card4 <- function(header, output){ #plotly card w/ small header, no borders
  card(
    full_screen = TRUE,
    class = "border-primary border-0",
    card_header(header, class = "h6 bg-transparent border-primary border-0"),
    card_body(
      class = "p-0",
      plotlyOutput(output))
  )
}

plot_card5 <- function(output){ #plotly card, no header, no border
  card(
    full_screen = TRUE,
    class = "p-0 border-primary border-0",
    card_body(
      class = "p-0",
      plotlyOutput(output))
  )
}

