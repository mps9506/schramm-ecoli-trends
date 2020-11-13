

plot_mk_power <- function(df,
                          file_name,
                          width,
                          height,
                          units,
                          res) {
  
  df <- read_rds(df)
  df <- df %>%
    unnest(c(power_chart_mk, p_est)) %>%
    filter(samples_per_year <= 12) %>%
    mutate(samples_per_year  = as.factor(samples_per_year))
  
  
  ggplot(df) +
    geom_histogram(aes(power,
                     color = as.factor(p.change),
                     fill = as.factor(p.change)),
                 alpha = 0.5) +
    facet_wrap(~as.factor(p.change)) +
    scale_fill_ordinal() +
    scale_color_ordinal() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_ms(grid = FALSE) +
    theme(legend.position = "bottom")
  
  ggsave(file_name,
         width = width,
         height = height,
         units = units,
         dpi = res)
}

