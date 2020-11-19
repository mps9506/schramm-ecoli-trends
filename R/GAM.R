

plot_mk_power <- function(site_info,
                          df,
                          file_name,
                          width,
                          height,
                          units,
                          res) {
  
  df <- read_rds(df)
  df <- df %>%
    unnest(c(power_chart_mk, p_est)) %>%
    filter(samples_per_year <= 12) %>%
    mutate(samples_per_year  = as.factor(samples_per_year)) %>%
    left_join(site_info) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "no TMDL",
      tmdl == 1 ~ "TMDL"
    ))
  
  
  ggplot(df) +
    geom_density_ridges(aes(power,
                            y = as.factor(p.change),
                     color = as.factor(p.change),
                     fill = as.factor(p.change)),
                 alpha = 0.5) +
    facet_wrap(~as.factor(tmdl)) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    labs(y = "effect size (% decrease)",
         x = "statistical power") +
    theme_ms(grid = FALSE) +
    theme(legend.position = "none")
  
  ggsave(file_name,
         width = width,
         height = height,
         units = units,
         dpi = res)
}

