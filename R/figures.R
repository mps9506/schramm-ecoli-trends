##########################
#### set a plot theme ####
##########################

theme_ms <- function(axis = FALSE,
                     axis_title_just = "c",
                     plot_margin = margin(10, 10, 10, 10),
                     base_family = "Source Sans Pro", 
                     base_size = 11,
                     ticks = TRUE,
                     ...) {
  theme_ipsum(axis = axis,
              axis_title_just = axis_title_just,
              plot_margin = plot_margin,
              base_family = base_family, 
              base_size = base_size,
              strip_text_family = "Source Sans Pro",
              ticks = ticks,
              ...) +
    theme(text = element_text(family = "Source Sans Pro", color = "#22211d"),
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
  
  keep_df <- df_ecoli %>%
    group_by(MonitoringLocationIdentifier, ActivityStartDate) %>%
    mutate(ResultMeasureValue = DescTools::Gmean(ResultMeasureValue)) %>%
    ungroup() %>%
    mutate(year = lubridate::year(ActivityStartDate)) %>%
    group_by(MonitoringLocationIdentifier, year) %>%
    summarize(n = n(),
              .groups = "drop") %>%
    ### fill missing years
    tidyr::complete(MonitoringLocationIdentifier, year, fill = list(n = 0)) %>%
    group_by(MonitoringLocationIdentifier) %>%
    summarize(median_n = median(n)) %>%
    filter(median_n >= 1 & median_n < 50)
  
  df_ecoli %>%
    filter(MonitoringLocationIdentifier %in% keep_df$MonitoringLocationIdentifier) %>%
    group_by(MonitoringLocationIdentifier, ActivityStartDate) %>%
    mutate(ResultMeasureValue = DescTools::Gmean(ResultMeasureValue)) %>%
    ungroup() %>%
    mutate(year = lubridate::year(ActivityStartDate)) %>%
    group_by(MonitoringLocationIdentifier, year) %>%
    summarize(n = n(),
              .groups = "drop") %>%
    ### fill missing years
    tidyr::complete(MonitoringLocationIdentifier, year, fill = list(n = 0)) %>%
    left_join(df_sites %>% 
                dplyr::select(MonitoringLocationIdentifier,
                                          tmdl)) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "Non-TMDL Sites",
      tmdl == 1 ~ "TMDL Sites"
    ),
    tmdl = as.factor(tmdl)) %>%
    group_by(MonitoringLocationIdentifier, tmdl) %>%
    summarize(n = median(n)) %>%
    ggplot() +
    geom_histogram(data = . %>% dplyr::select(-tmdl), 
                      aes(n, fill = "All SWQM Sites"),
                      color = "transparent",
                   binwidth = 1) +
    geom_histogram(aes(n, fill = tmdl),
                      color = "transparent",
                      alpha = 0.5,
                   binwidth = 1) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
    scale_x_continuous(expand = c(0,0), breaks = (c(2,4,6,8,10,12))) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~tmdl) +
    scale_fill_manual(
      values = c("#b3b3b3a0", "#D55E00", "#0072B2"), 
      breaks = c("All SWQM Sites", "Non-TMDL Sites", "TMDL Sites"),
      labels = c("All SWQM Sites", "Non-TMDL Sites", "TMDL Sites"),
      name = NULL,
      guide = guide_legend(direction = "horizontal")
    ) +
    coord_cartesian(xlim = c(0,13)) +
    labs(x = "Median Annual Samples Per SWQM Site (n)",
         y = "Count") +
    theme_ms(grid = FALSE) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 9),
          strip.text = element_text(size = 8))
  
  ggsave(file_name,
         device = ragg::agg_png(),
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
  
  keep_df <- df_ecoli %>%
    group_by(MonitoringLocationIdentifier, ActivityStartDate) %>%
    mutate(ResultMeasureValue = DescTools::Gmean(ResultMeasureValue)) %>%
    ungroup() %>%
    mutate(year = lubridate::year(ActivityStartDate)) %>%
    group_by(MonitoringLocationIdentifier, year) %>%
    summarize(n = n(),
              .groups = "drop") %>%
    ### fill missing years
    tidyr::complete(MonitoringLocationIdentifier, year, fill = list(n = 0)) %>%
    group_by(MonitoringLocationIdentifier) %>%
    summarize(median_n = median(n)) %>%
    filter(median_n >= 1 & median_n < 50)

  df_ecoli %>%
    filter(MonitoringLocationIdentifier %in% keep_df$MonitoringLocationIdentifier) %>%
    group_by(MonitoringLocationIdentifier, ActivityStartDate) %>%
    mutate(ResultMeasureValue = DescTools::Gmean(ResultMeasureValue)) %>%
    ungroup() %>%
    group_by(MonitoringLocationIdentifier) %>%
    summarize(mean = DescTools::Gmean(ResultMeasureValue)) %>%
    left_join(df_sites %>% dplyr::select(MonitoringLocationIdentifier, tmdl)) %>%
    mutate(tmdl = case_when(
      tmdl == 0 ~ "Non-TMDL Sites",
      tmdl == 1 ~ "TMDL Sites"
    ),
    tmdl = as.factor(tmdl)) %>%
    ggplot() +
    geom_density_line(data = . %>% dplyr::select(-tmdl), 
                      aes(mean, fill = "All SWQM Sites"),
                      color = "transparent") +
    geom_density_line(aes(mean, fill = tmdl),
                      color = "transparent",
                      alpha = 0.5) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
    scale_x_continuous(trans = "log10", expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_wrap(~tmdl) +
    scale_fill_manual(
      values = c("#b3b3b3a0", "#D55E00", "#0072B2"), 
      breaks = c("All SWQM Sites", "Non-TMDL Sites", "TMDL Sites"),
      labels = c("All SWQM Sites", "Non-TMDL Sites", "TMDL Sites"),
      name = NULL,
      guide = guide_legend(direction = "horizontal")
    ) +
    labs(x = "SWQM Station Geometric Mean (MPN/100 mL)",
         y = "Scaled Density") +
    theme_ms(grid = FALSE) +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 8))
  
  ggsave(file_name,
         device = ragg::agg_png(),
         width = width,
         height = height,
         units = units,
         dpi = res)

}



