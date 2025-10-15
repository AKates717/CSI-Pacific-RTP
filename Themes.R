

#ak_gt_theme1 ----
#from shiny app, original gt, probably won't use this one
#with grand_summary_row
ak_gt_theme1 <- function(gt_tbl){
  
  n_rows <- nrow(gt_tbl$'_data')
  
  gt_tbl %>%
    tab_options(
      data_row.padding = px(4),
      heading.align = "Left",
      column_labels.background.color = "red",
      heading.title.font.size = px(26),
      heading.subtitle.font.size = px(14),
      heading.padding = px(2),
      table_body.hlines.width = px(0)
    ) %>%
    tab_style(
      style = cell_text(
        color = "red",
        weight = "bold",
        font = google_font("Merriweather")
      ),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_fill(color = "grey90"),
      locations = cells_body(rows = seq(1, n_rows,2))
    ) %>%
    tab_style(
      style = cell_fill(color = "grey90"),
      locations = cells_stub(rows = seq(1, n_rows,2))
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_grand_summary()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_stub_grand_summary()
    )
}


#ak_gt_theme2 ----
#without grand_summary_row
ak_gt_theme2 <- function(gt_tbl){
  
  n_rows <- nrow(gt_tbl$'_data')
  
  gt_tbl %>%
    tab_options(
      data_row.padding = px(4),
      heading.align = "Left",
      column_labels.background.color = "red",
      heading.title.font.size = px(26),
      heading.subtitle.font.size = px(14),
      heading.padding = px(2),
      table_body.hlines.width = px(0)
    ) %>%
    tab_style(
      style = cell_text(
        color = "red",
        weight = "bold",
        font = google_font("Merriweather")
      ),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_fill(color = "grey90"),
      locations = cells_body(rows = seq(1, n_rows,2))
    ) %>%
    tab_style(
      style = cell_fill(color = "grey90"),
      locations = cells_stub(rows = seq(1, n_rows,2))
    )
}


#ak_gt_theme3 ----
#This is the working gt theme right now, ignore the first 2, create others based off this
ak_gt_theme3 <- function(
    gt_tbl,
    accent        = "#D71920",            # brand accent (primary red)
    header_bg     = "#FCFCFC",            # column label background (light grey)
    stripe_bg     = "#FFFFFF",            # row striping (white)
    border_col    = "#A7A9AC",            # neutral borders (grey)
    title_font    = google_font("Roboto Slab"),
    body_font     = google_font("Open Sans"),
    base_size_px  = 16,
    corner_radius = 10
) {
  
  gt_tbl %>%
    # fonts & base
    opt_table_font(font = list(body_font, default_fonts())) %>%
    tab_options(
      table.width                 = pct(100),
      table.background.color      = "white",
      table.font.size             = px(base_size_px),
      data_row.padding            = px(8),
      
      heading.align               = "left",
      heading.title.font.size     = px(22),
      heading.subtitle.font.size  = px(13),
      heading.padding             = px(6),
      
      # borders
      table.border.top.color      = accent,      # brand bar at top
      table.border.top.width      = px(3),
      table.border.bottom.color   = border_col,
      table.border.bottom.width   = px(1),
      column_labels.vlines.color  = "transparent",
      column_labels.border.bottom.color = border_col,  # red underline under headers
      column_labels.border.bottom.width = px(1),
      table_body.hlines.color     = border_col,
      table_body.hlines.width     = px(1),
      table_body.vlines.color     = "transparent",
      
      # headers + striping
      column_labels.background.color = header_bg,
      row.striping.background_color  = stripe_bg
    ) %>%
    # title styling
    tab_style(
      style = cell_text(color = accent, weight = "700", font = title_font),
      locations = cells_title(groups = "title")
    ) %>%
    # column labels
    tab_style(
      style = cell_text(weight = "600", color = "#111827"),
      locations = cells_column_labels(everything())
    ) %>%
    # body text
    tab_style(
      style = cell_text(color = "#222222"),
      locations = cells_body()
    ) %>%
    # enable striping (use color set above)
    opt_row_striping() %>%
    # rounded corners + gentle red-tinted hover + subtle left accent bar
    opt_css(sprintf("
  .gt_table {
    border-collapse: separate !important;
    border-spacing: 0 !important;
    border-radius: %dpx !important;
    overflow: hidden !important;
  }
  .gt_table tbody tr:hover td {
    background-color: rgba(215, 25, 32, 0.06) !important; /* light brand red */
  }
  .gt_footnotes, .gt_sourcenotes { font-size: 0.9em; }
", corner_radius))
}




