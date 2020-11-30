

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


plot_gam_fit <- function(site_info,
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
    mutate(samples_per_year  = samples_per_year) %>%
    left_join(site_info) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "non-TMDL sites",
      tmdl == 1 ~ "TMDL sites"),
      method = "Mann-Kendall")
  
  df <- read_rds(df_lm) %>%
    unnest(c(power_chart_lm, p_est)) %>%
    filter(samples_per_year <= 12) %>%
    mutate(samples_per_year  = samples_per_year) %>%
    left_join(site_info) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "non-TMDL sites",
      tmdl == 1 ~ "TMDL sites"),
      method = "Linear Regression")
  
  df_both <- df_both %>%
    bind_rows(df) %>%
    mutate(tmdl = as.factor(tmdl),
           p.change = -p.change)
  
  df_both %>%
    group_by(method) %>%
    nest() %>%
    mutate(
      gams = map(.x = data,
                 ~ gam(power ~ s(cv, bs = "cr") + s(p.change, bs = "cr", k = 3) + s(samples_per_year, bs = "cr", k = 3),
                       data = .x,
                       family = betar(link = "logit", eps = .Machine$double.eps*1e6),
                       method = "REML")),
      newdata = map(.x = gams,
                        ~ {
                          df <- new_data(.x, terms = c("samples_per_year", "p.change", "cv [quart2]"))
                          predictions <- predict(object = .x,
                                                 newdata = df,
                                                 type = "link",
                                                 se.fit = TRUE)
                          df <- bind_cols(df, predictions)
                          ilink <- betar()$linkinv
                          
                          df <- mutate(df,
                                       fit_resp = ilink(fit),
                                       upr_ci = ilink(fit + (2 * se.fit)),
                                       lwr_ci = ilink(fit - (2 * se.fit)))
                          df
                          }
                    )) %>%
    ungroup() -> df_both
  

  df_both %>%
    unnest(newdata) %>%
    mutate(cv = round(cv, 2),
           cv = paste0("CV = ", cv),
           p.change = as.factor(p.change)) -> df_both


  ggplot(df_both) +
    # geom_pointrange(aes(x = samples_per_year, 
    #                     y = fit_resp, 
    #                     ymin = lwr_ci, 
    #                     ymax = upr_ci, 
    #                     color = p.change), alpha = 0.8, size = .15) +
    geom_point(aes(x = samples_per_year,
                   y = fit_resp,
                   color = p.change),
               alpha = 0.8) +
    geom_line(aes(x = samples_per_year,
                  y = fit_resp,
                  color = p.change),
              alpha = 0.8) +
    # geom_linerange(aes(x = samples_per_year,
    #                ymin = lwr_ci,
    #                ymax = upr_ci,
    #                color = p.change)) +
    scale_color_viridis_d(name = "effect size (% decrease)", direction = -1) +
    scale_x_continuous("samples per year", breaks = c(1:12)) +
    scale_y_continuous(trans = "log10") +
    facet_grid(vars(method), vars(cv)) +
    geom_hline(aes(linetype = "0.80 power", yintercept = 0.8)) +
    scale_linetype_manual(name = NULL, values = 2) +
    labs(x = "annual samples (n)",
         y = "power") +
    theme_ms(grid = "Y") +
    theme(legend.position = "bottom")

  ggsave(file_name,
         width = width,
         height = height,
         units = units,
         dpi = res)
  
}



