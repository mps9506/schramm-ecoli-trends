##### POWER ANALYSIS

## generates a vector starting at mean mu and desired percent change over n-years
generate_trend <- function(mu, percent_change, n_years) {
  change <- mu*percent_change/100
  i <- 1:n_years
  b <- change/(n_years-1)
  mu <- mu + (i-1) * b
  x <- tibble(i = i,
              mu = mu)
  return(x)
}

## simulates lognormal data with for n samples per year with mean mu trending as
## specified in generate_trend
generate_random_data <- function(trend, samples_per_year, sd) {
  
  trend %>% 
    mutate(samples = purrr::map(mu,
                                ~{tibble(n_sample = 1:samples_per_year,
                                         monthly_sample = rlnorm(n = samples_per_year, 
                                                                 meanlog = .x, 
                                                                 sdlog = sd))})) %>%
    tidyr::unnest(samples) %>%
    mutate(t = i + ((n_sample-1)/samples_per_year)) %>%
    dplyr::select(t, monthly_sample)
  
}

power_ken <- function(r, mu, sd, percent_change, n_years, samples_per_year) {
  tibble(r = 1:r) %>%
    mutate(n_years = n_years,
           samples_per_year = samples_per_year) %>%
    mutate(trend = purrr::map(n_years, 
                              ~generate_trend(mu = mu, 
                                              percent_change = percent_change, 
                                              n_years = .x)),
           random_data = purrr::map2(trend, samples_per_year,
                                     ~generate_random_data(.x, .y, sd)), 
           mk_test = purrr::map(random_data,
                                ~EnvStats::kendallTrendTest(monthly_sample ~ t, data = .x)),
           p_value = purrr::map_dbl(mk_test,
                                    "p.value")) -> df
  power <- nrow(df %>% filter(p_value <= 0.1))/r
  return(power)
}

power_lm <- function(r, mu, sd, percent_change, n_years, samples_per_year) {
  tibble(r = 1:r) %>%
    mutate(n_years = n_years,
           samples_per_year = samples_per_year) %>%
    mutate(trend = purrr::map(n_years, 
                              ~generate_trend(mu = mu, 
                                              percent_change = percent_change, 
                                              n_years = .x)),
           random_data = purrr::map2(trend, samples_per_year,
                                     ~generate_random_data(.x, .y, sd)), 
           lm_test = purrr::map(random_data,
                                ~lm(log(monthly_sample) ~ t, data = .x)),
           p_value = purrr::map(lm_test,
                                    ~coef(summary(.x))[2,4])) -> df
  power <- nrow(df %>% filter(p_value <= 0.1))/r
  return(power)
}



create_power_chart <- function(r = 1000, ## number of resamples
                               samples_per_year, 
                               percents = c(-10, -20, -40, -80), ## vector of percent change to calculate
                               years = 7, ## how long to do trend test
                               mu, ## sample mean 
                               sd, ## sample sd
                               method = c("ken", "glm"),
                               pb) {
  
  pb$tick()$print()
  #samples_per_year <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  output <- tibble()
  
  if (method == "ken") {f <- formula(~power_ken(r, mu, sd, percent_change, .x, .y))}
  if (method == "glm") {f <- formula(~power_lm(r, mu, sd, percent_change, .x, .y))}

  
  for (i in percents) {
    percent_change = i
    output_percent <- tibble(n_years = rep(years, length(samples_per_year)),
                             samples_per_year = samples_per_year) %>%
      mutate(power = purrr::map2(n_years, 
                                        samples_per_year,
                                        f)) %>% 
      tidyr::unnest(power) %>% 
      mutate(p.change = percent_change)
    
    output <- bind_rows(output_percent, output)
  }
  
  return(output)
}


fit_power_mk <- function(df_ecoli) {
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
    filter(median_n >= 1)
  
  
  df_ecoli %>%
    filter(MonitoringLocationIdentifier %in% keep_df$MonitoringLocationIdentifier) %>%
    left_join(keep_df) %>%
    mutate(median_n = round(median_n, 0),
           ResultMeasureValue = case_when(
             ResultMeasureValue == 1 ~ ResultMeasureValue + 1,
             ResultMeasureValue > 1 ~ ResultMeasureValue)) %>%
    group_by(MonitoringLocationIdentifier, median_n) %>%
    nest() %>%
    mutate(fln = purrr::map(data, ~fitdist(.x$ResultMeasureValue, "lnorm")),
           mu = purrr::map_dbl(fln, ~.x$estimate["meanlog"]),
           sd = purrr::map_dbl(fln, ~.x$estimate["sdlog"]),
           cv = purrr::map_dbl(data, ~EnvStats::cv(.x$ResultMeasureValue))) %>%
    nest(p_est = c(median_n, mu, sd, cv)) -> df_ecoli
  
  n <- length(df_ecoli$p_est)
  pb <- progress_estimated(n = n)

  df_ecoli %>%
    mutate(power_chart_mk = purrr::map(p_est, ~create_power_chart(r = 1000,
                                                           samples_per_year = .x$median_n,
                                                           years = 7,
                                                           mu = .x$mu,
                                                           sd = .x$sd,
                                                           method = "ken",
                                                           pb = pb)))
  
}


fit_power_lm <- function(df_ecoli) {
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
    filter(median_n >= 1)
  

  df_ecoli %>%
    filter(MonitoringLocationIdentifier %in% keep_df$MonitoringLocationIdentifier) %>%
    left_join(keep_df) %>%
    mutate(median_n = round(median_n, 0),
           ResultMeasureValue = case_when(
             ResultMeasureValue == 1 ~ ResultMeasureValue + 1,
             ResultMeasureValue > 1 ~ ResultMeasureValue)) %>%
    group_by(MonitoringLocationIdentifier, median_n) %>%
    nest() %>%
    mutate(fln = purrr::map(data, ~fitdist(.x$ResultMeasureValue, "lnorm")),
           mu = purrr::map_dbl(fln, ~.x$estimate["meanlog"]),
           sd = purrr::map_dbl(fln, ~.x$estimate["sdlog"]),
           cv = purrr::map_dbl(data, ~EnvStats::cv(.x$ResultMeasureValue))) %>%
    nest(p_est = c(median_n, mu, sd, cv)) -> df_ecoli
  
  n <- length(df_ecoli$p_est)
  pb <- progress_estimated(n = n)
  
  df_ecoli %>%
    mutate(power_chart_lm = purrr::map(p_est, ~create_power_chart(r = 1000,
                                                           samples_per_year = .x$median_n,
                                                           years = 7,
                                                           mu = .x$mu,
                                                           sd = .x$sd,
                                                           method = "glm",
                                                           pb = pb)))
  
}


model_mk_lhood <- function(mk_results) {
  mk_results <- read_rds(mk_results)
  mk_results %>%
    unnest(c(p_est, power_chart_mk)) %>%
    mutate(y = case_when(
      power >= 0.8 ~ 1,
      power < 0.8 ~ 0
    )) %>%
    filter(samples_per_year <= 12) -> mk_df
  
  mk_m1 <- glm(y ~ cv + samples_per_year + p.change,
               data = mk_df,
               family = binomial(link = "logit"))
  
}

model_lm_lhood <- function(lm_results) {
  lm_results <- read_rds(lm_results)
  lm_results %>%
    unnest(c(p_est, power_chart_lm)) %>%
    mutate(y = case_when(
      power >= 0.8 ~ 1,
      power < 0.8 ~ 0
    )) %>%
    filter(samples_per_year <= 12) -> lm_df
  
  
  lm_m1 <- glm(y ~ cv + samples_per_year + p.change,
               data = lm_df,
               family = binomial(link = "logit"))
}

plot_lhood <- function(mk_lhood_model,
                       lm_lhood_model,
                       file_name,
                       width,
                       height,
                       units,
                       res) {
  
  fits_mk <- ggpredict(model = mk_lhood_model,
                    terms = c("samples_per_year[1:12]","p.change"))
  fits_mk <- fits_mk %>%
    mutate(model = "Mann-Kendall")
  
  fits_lm <- ggpredict(model = lm_lhood_model,
                     terms = c("samples_per_year[1:12]","p.change"))
  fits_lm <- fits_lm %>%
    mutate(model = "Linear Regression")
  
  df <- bind_rows(fits_mk, fits_lm)
  
  ggplot(df) +
    geom_line(aes(x = x,
                  y = predicted,
                  group = group,
                  color = group)) +
    geom_ribbon(aes(x,
                    ymin = conf.low,
                    ymax = conf.high,
                    group = group,
                    fill = group), alpha = 0.3) +
    facet_wrap(~model) +
    scale_fill_viridis_d(name = "Effect Size (% decrease)", 
                         labels = c(80, 40, 20, 10)) +
    scale_color_viridis_d(name = "Effect Size (% decrease)", 
                          labels = c(80, 40, 20, 10)) +
    scale_x_continuous(expand = c(0,0), breaks = c(2,4,6,8,10,12)) +
    labs(x = "Samples Per Year", y = "Probability of Adequate Power") +
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



##glm tables

make_glm_tables <- function(mk, lm) {
  
  t1 <- gtsummary::tbl_regression(mk, 
                                  exponentiate = TRUE,
                                  label = list(cv ~ "cv",
                                               samples_per_year ~ "Sample Size",
                                               p.change ~ "Effect Size")) %>%
    modify_header(label = "**Variable**") # update the column header
  
  t2 <- gtsummary::tbl_regression(lm, 
                                  exponentiate = TRUE,
                                  label = list(cv ~ "cv",
                                               samples_per_year ~ "Sample Size",
                                               p.change ~ "Effect Size")) %>%
    modify_header(label = "**Variable**") # update the column header
  
  t3 <- tbl_merge(tbls = list(t1, t2),
                  tab_spanner = c("**Mann-Kendall**", "**Linear Regression**")
    )
  
  as_flex_table(
    t3,
    include = everything(),
    return_calls = FALSE) %>%
    set_table_properties(layout = "autofit")
}



## make generic power figures



make_power_figure_data <- function(df_ecoli) {
  
  ## get the sample variance
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
    filter(median_n >= 1)
  
  
  df_ecoli %>%
    filter(MonitoringLocationIdentifier %in% keep_df$MonitoringLocationIdentifier) %>%
    left_join(keep_df) %>%
    mutate(median_n = round(median_n, 0),
           ResultMeasureValue = case_when(
             ResultMeasureValue == 1 ~ ResultMeasureValue + 1,
             ResultMeasureValue > 1 ~ ResultMeasureValue)) %>%
    group_by(MonitoringLocationIdentifier, median_n) %>%
    nest() %>%
    mutate(fln = purrr::map(data, ~fitdist(.x$ResultMeasureValue, "lnorm")),
           mu = purrr::map_dbl(fln, ~.x$estimate["meanlog"]),
           sd = purrr::map_dbl(fln, ~.x$estimate["sdlog"]),
           cv = purrr::map_dbl(data, ~EnvStats::cv(.x$ResultMeasureValue))) %>%
    ungroup() %>%
    dplyr::select(mu, sd, cv) %>%
    summarise(sd = quantile(sd, probs = c(0.25, 0.5, 0.75)),
              cv = quantile(cv, probs = c(0.25, 0.5, 0.75)),
              mu  = rep(quantile(mu, probs = c(0.5))),3) %>%
    nest(p_est = c(mu, sd)) -> df_ecoli
  
  n <- length(df_ecoli$p_est)
  pb <- progress_estimated(n)
  df_lm <- df_ecoli %>%
    mutate(power_chart_lm = purrr::map(p_est, ~create_power_chart(r = 1000,
                                                                  samples_per_year = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                                                  percents = c(-10, -20, -30, -40, -50, -60, -70, -80),
                                                                  years = 7,
                                                                  mu = .x$mu,
                                                                  sd = .x$sd,
                                                                  method = "glm",
                                                                  pb = pb))) %>%
    unnest(power_chart_lm) %>%
    dplyr::select(cv, n_years, samples_per_year, power, p.change) %>%
    mutate(model = "Linear Regression")
  
  n <- length(df_ecoli$p_est)
  pb <- progress_estimated(n)
  df_mk <- df_ecoli %>%
    mutate(power_chart_mk = purrr::map(p_est, ~create_power_chart(r = 1000,
                                                                  samples_per_year = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                                                  percents = c(-10, -20, -30, -40, -50, -60, -70, -80),
                                                                  years = 7,
                                                                  mu = .x$mu,
                                                                  sd = .x$sd,
                                                                  method = "ken",
                                                                  pb = pb))) %>%
    unnest(power_chart_mk) %>%
    dplyr::select(cv, n_years, samples_per_year, power, p.change) %>%
    mutate(model = "Mann-Kendall")
  
  df <- bind_rows(df_lm, df_mk)
  
  
  
}



draw_power_figure <- function(x,
                              file_name,
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