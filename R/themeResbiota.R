themeResbiota <- function(base_size = 10){
  theme(
    # Titles
    plot.tag = element_text(size = base_size),
    plot.title = element_text(size = base_size, face = "bold"),
    plot.subtitle = element_text(size = base_size*0.9),
    plot.caption = element_text(size = base_size*0.9),
    # Axis
    axis.title = element_text(size = base_size*0.9, face = "bold"),
    axis.text.y = element_text(size = base_size*0.8, hjust = 1),
    axis.text.x = element_text(size = base_size*0.8, angle = 0, vjust = 1),
    axis.line = element_line(linewidth = 0.5),
    # Legend
    legend.key = element_blank(),
    legend.position = "none",
    legend.justification = "center",
    legend.title = element_text(size = base_size*0.8, face = "bold"),
    legend.text = element_text(size = base_size*0.7),
    # Grids
    strip.text = element_text(size = base_size*0.8, face = "bold"),
    strip.text.x = element_text(margin = margin(1,1,1,1)),
    strip.text.y = element_text(angle = -90, margin = margin(1,1,1,1)),
    # Margins/spacing/borders
    plot.margin = margin(12, 10, 12, 10),
    panel.spacing = unit(0.75, "lines"),
    panel.border = element_blank(),
    # Background
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill  = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    strip.background = element_rect(fill = "#dbdbdb", color = NA)
  )
}
