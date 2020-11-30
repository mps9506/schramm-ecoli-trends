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
generate_random_data <- function(trend, samples_per_year, cv) {
  
  trend %>% 
    mutate(samples = purrr::map(mu,
                                ~{tibble(n_sample = 1:samples_per_year,
                                         monthly_sample = rlnormAlt(n = samples_per_year, 
                                                                    mean = .x, 
                                                                    cv = cv))})) %>%
    tidyr::unnest(samples) %>%
    mutate(t = i + ((n_sample-1)/samples_per_year)) %>%
    dplyr::select(t, monthly_sample)
  
}

power_ken <- function(r, mu, cv, percent_change, n_years, samples_per_year) {
  tibble(r = 1:r) %>%
    mutate(n_years = n_years,
           samples_per_year = samples_per_year) %>%
    mutate(trend = purrr::map(n_years, 
                              ~generate_trend(mu = mu, 
                                              percent_change = percent_change, 
                                              n_years = .x)),
           random_data = purrr::map2(trend, samples_per_year,
                                     ~generate_random_data(.x, .y, cv)), 
           mk_test = purrr::map(random_data,
                                ~smwrStats::kensen.test(.x$monthly_sample, .x$t, n.min = Inf)),
           p_value = purrr::map_dbl(mk_test,
                                    "p.value")) -> df
  power <- nrow(df %>% filter(p_value <= 0.1))/r
  return(power)
}

power_lm <- function(r, mu, cv, percent_change, n_years, samples_per_year) {
  tibble(r = 1:r) %>%
    mutate(n_years = n_years,
           samples_per_year = samples_per_year) %>%
    mutate(trend = purrr::map(n_years, 
                              ~generate_trend(mu = mu, 
                                              percent_change = percent_change, 
                                              n_years = .x)),
           random_data = purrr::map2(trend, samples_per_year,
                                     ~generate_random_data(.x, .y, cv)), 
           lm_test = purrr::map(random_data,
                                ~lm(log(monthly_sample) ~ t, data = .x)),
           p_value = purrr::map(lm_test,
                                    ~coef(summary(.x))[2,4])) -> df
  power <- nrow(df %>% filter(p_value <= 0.1))/r
  return(power)
}



create_power_chart <- function(r = 1000, ## number of resamples
                               samples_per_year, 
                               percents = c(-5, -10, -20, -40, -80), ## vector of percent change to calculate
                               years = 7, ## how long to do trend test
                               mu, ## sample mean 
                               cv, ## sample coef of variation
                               method = c("ken", "glm"),
                               pb) {
  
  pb$tick()$print()
  #samples_per_year <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  output <- tibble()
  
  if (method == "ken") {f <- formula(~power_ken(r, mu, cv, percent_change, .x, .y))}
  if (method == "glm") {f <- formula(~power_lm(r, mu, cv, percent_change, .x, .y))}

  
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
    mutate(#fln = purrr::map(data, ~fitdist(.x$ResultMeasureValue, "lnorm")),
           mu = purrr::map_dbl(data, ~mean(.x$ResultMeasureValue)),
           cv = purrr::map_dbl(data, ~EnvStats::cv(.x$ResultMeasureValue))) %>%
    nest(p_est = c(median_n, mu, cv)) -> df_ecoli
  
  n <- length(df_ecoli$p_est)
  pb <- progress_estimated(n = n)

  df_ecoli %>%
    mutate(power_chart_mk = purrr::map(p_est, ~create_power_chart(r = 1000,
                                                           samples_per_year = .x$median_n,
                                                           years = 7,
                                                           mu = .x$mu,
                                                           cv = .x$cv,
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
    mutate(#fln = purrr::map(data, ~fitdist(.x$ResultMeasureValue, "lnorm")),
      mu = purrr::map_dbl(data, ~mean(.x$ResultMeasureValue)),
      cv = purrr::map_dbl(data, ~EnvStats::cv(.x$ResultMeasureValue))) %>%
    nest(p_est = c(median_n, mu, cv)) -> df_ecoli
  
  n <- length(df_ecoli$p_est)
  pb <- progress_estimated(n = n)
  
  df_ecoli %>%
    mutate(power_chart_lm = purrr::map(p_est, ~create_power_chart(r = 1000,
                                                           samples_per_year = .x$median_n,
                                                           years = 7,
                                                           mu = .x$mu,
                                                           cv = .x$cv,
                                                           method = "glm",
                                                           pb = pb)))
  
}



