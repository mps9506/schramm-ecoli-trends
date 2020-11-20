

plot_mk_power <- function(site_info,
                          df_mk,
                          df_lm,
                          file_name,
                          width,
                          height,
                          units,
                          res) {
  
  df <- read_rds(df_mk)
  df_both <- df %>%
    unnest(c(power_chart_mk, p_est)) %>%
    filter(samples_per_year <= 12) %>%
    mutate(samples_per_year  = as.factor(samples_per_year)) %>%
    left_join(site_info) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "non-TMDL sites",
      tmdl == 1 ~ "TMDL sites"),
      method = "Mann-Kendall")
  
  df <- read_rds(df_lm) %>%
    unnest(c(power_chart_lm, p_est)) %>%
    filter(samples_per_year <= 12) %>%
    mutate(samples_per_year  = as.factor(samples_per_year)) %>%
    left_join(site_info) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "non-TMDL sites",
      tmdl == 1 ~ "TMDL sites"),
      method = "GLM")
  
  df_both <- df_both %>%
    bind_rows(df) %>%
    mutate(tmdl = as.factor(tmdl),
           p.change = as.factor(p.change)) 
  
  ggplot(df_both) +
    geom_density_ridges(aes(x = power,
                            y = p.change,
                            color = p.change,
                            fill = p.change),
                        alpha = 0.5) +
    facet_grid(vars(method), vars(tmdl)) +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    scale_x_continuous(expand = c(0,0),
                       limits = c(0,1)) +
    scale_y_discrete(labels = c(80,40,20,10,5)) +
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





