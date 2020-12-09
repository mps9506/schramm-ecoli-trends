
draw_power_figure <- function(x,
                              file_name = file_out("figures/fig_5.png"),
                              width = 6.5,
                              height = 4.5,
                              units = "in",
                              res = 300) {
  df <- read_rds(x)
  
  df <- df %>%
    mutate(p.change = as.factor(p.change),
           cv = paste0("CV = ", round(cv, 2)))
  
  df %>%
    ggplot(aes(samples_per_year, power, group = p.change, color = p.change)) +
    geom_point() +
    geom_line() +
    scale_color_viridis_d(name = "Effect Size", labels = c(80,70,60,50,40,30,20,10)) +
    scale_x_continuous(name = "Annual Samples (n)", breaks = c(2,4,6,8,10,12)) +
    scale_y_continuous(name = "Estimated Power") +
    facet_grid(vars(model), vars(cv)) +
    theme_ms() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 8))
  
  ggsave(file_name,
         device = ragg::agg_png(),
         width = width,
         height = height,
         units = units,
         dpi = res)

}

draw_power_figure("data/power_fig_dat.rds")
