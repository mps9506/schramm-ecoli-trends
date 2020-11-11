##########################
#### set a plot theme ####
##########################

theme_ms <- function(axis = FALSE,
                     axis_title_just = "c",
                     plot_margin = margin(10, 10, 10, 10),
                     base_family = "Open Sans", 
                     base_size = 11,
                     ticks = TRUE,
                     ...) {
  theme_ipsum(axis = axis,
              axis_title_just = axis_title_just,
              plot_margin = plot_margin,
              base_family = base_family, 
              base_size = base_size,
              strip_text_family = "Open Sans",
              ticks = ticks,
              ...) +
    theme(text = element_text(family = "Open Sans", color = "#22211d"),
          panel.border = element_rect(color = "black", fill = NA))
}


###############################################
#### distribution of annual n measurements ####
###############################################

plot_annual_distribution <- function(df_ecoli,
                                     df_sites,
                                     file_name,
                                     width,
                                     height,
                                     units,
                                     res) {
  df_ecoli %>%
    group_by(MonitoringLocationIdentifier, ActivityStartDate) %>%
    mutate(ResultMeasureValue = DescTools::Gmean(ResultMeasureValue)) %>%
    ungroup() %>%
    mutate(year = lubridate::year(ActivityStartDate)) %>%
    group_by(MonitoringLocationIdentifier, year) %>%
    summarize(n = n(),
              .groups = "drop") %>%
    ### fill missing years
    tidyr::complete(MonitoringLocationIdentifier, year, fill = list(n = 0)) %>%
    left_join(df_sites %>% select(MonitoringLocationIdentifier,
                                          tmdl)) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "no TMDL",
      tmdl == 1 ~ "TMDL"
    ),
    tmdl = as.factor(tmdl)) %>%
    group_by(MonitoringLocationIdentifier, tmdl) %>%
    summarize(n = median(n)) %>%
    ggplot() +
    geom_density_line(data = . %>% select(-tmdl), 
                      aes(n, fill = "all SWQM stations"),
                      color = "transparent") +
    geom_density_line(aes(n, fill = tmdl),
                      color = "transparent",
                      alpha = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~tmdl) +
    scale_fill_manual(
      values = c("#b3b3b3a0", "#D55E00", "#0072B2"), 
      breaks = c("all SWQM stations", "no TMDL", "TMDL"),
      labels = c("all SWQM stations", "no TMDL", "TMDL"),
      name = NULL,
      guide = guide_legend(direction = "horizontal")
    ) +
    labs(x = "median annual samples per SWQM station (n)",
         y = "scaled density") +
    theme_ms(grid = FALSE) +
    theme(legend.position = "bottom")
  
  ggsave(file_name,
         width = width,
         height = height,
         units = units,
         dpi = res)
}

plot_ecoli <- function(df_ecoli,
                       df_sites,
                       file_name,
                       width,
                       height,
                       units,
                       res) {

  df_ecoli %>%
    group_by(MonitoringLocationIdentifier, ActivityStartDate) %>%
    mutate(ResultMeasureValue = DescTools::Gmean(ResultMeasureValue)) %>%
    ungroup() %>%
    group_by(MonitoringLocationIdentifier) %>%
    summarize(mean = DescTools::Gmean(ResultMeasureValue)) %>%
    left_join(df_sites %>% select(MonitoringLocationIdentifier, tmdl)) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "no TMDL",
      tmdl == 1 ~ "TMDL"
    ),
    tmdl = as.factor(tmdl)) %>%
    ggplot() +
    geom_density_line(data = . %>% select(-tmdl), 
                      aes(mean, fill = "all SWQM stations"),
                      color = "transparent") +
    geom_density_line(aes(mean, fill = tmdl),
                      color = "transparent",
                      alpha = 0.5) +
    scale_x_continuous(trans = "log10", expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~tmdl) +
    scale_fill_manual(
      values = c("#b3b3b3a0", "#D55E00", "#0072B2"), 
      breaks = c("all SWQM stations", "no TMDL", "TMDL"),
      labels = c("all SWQM stations", "no TMDL", "TMDL"),
      name = NULL,
      guide = guide_legend(direction = "horizontal")
    ) +
    labs(x = "SWQM Station Geometric Mean (MPN/100 mL)",
         y = "scaled density") +
    theme_ms(grid = FALSE) +
    theme(legend.position = "bottom")
  
  ggsave(file_name,
         width = width,
         height = height,
         units = units,
         dpi = res)

}



