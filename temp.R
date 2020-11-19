
#### fit distibutions to sites with more than >= 1 sample per year

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
           sd = purrr::map_dbl(fln, ~.x$estimate["sdlog"])) %>%
    nest(p_est = c(median_n, mu, sd)) %>%
    mutate(power_chart_mk = map(p_est, ~create_power_chart(r = 10,
                                                           samples_per_year = .x$median_n,
                                                           years = 7,
                                                           mu = .x$mu,
                                                           sd = .x$sd,
                                                           method = "ken")))

}




readd(mk_power) %>%
  unnest(c(power_chart_mk, p_est)) %>%
  filter(samples_per_year <= 12) %>%
  ggplot() +
  geom_boxplot(aes(samples_per_year, power, group = samples_per_year), alpha = 0.1) +
  facet_wrap(~p.change)


readd(mk_power) %>%
  unnest(c(power_chart_mk, p_est)) %>%
  filter(samples_per_year <= 12) %>%
  mutate(samples_per_year  = as.factor(samples_per_year)) -> m_df


ggplot(m_df) +
  geom_density(aes(power, 
                   color = as.factor(p.change),
                   fill = as.factor(p.change)),
                 alpha = 0.5) +
  theme_ms()



readd(lm_power) %>%
  unnest(c(power_chart_lm, p_est)) %>%
  filter(samples_per_year <= 12) %>%
  mutate(samples_per_year  = as.factor(samples_per_year)) %>%
  ggplot() +
  geom_density(aes(power, 
                   color = as.factor(p.change),
                   fill = as.factor(p.change)),
               alpha = 0.5) +
  theme_ms()


library(mgcv)
library(ggeffects)

m1 <- gam(power ~ s(mu) + s(sd) + samples_per_year,
    data = m_df %>% filter(p.change == -25),
    family = betar(link = "logit", eps = .Machine$double.eps*1e6),
    method = "ML")

plot(m1)
summary(m1)

ggpredict(m1, terms = c("samples_per_year", "sd", "mu"))
plot(ggpredict(m1, terms = c("samples_per_year", "mu", "sd")), 
     facet = TRUE,
     log.y = TRUE, 
     ci.style = "errorbar") + 
  scale_x_continuous("samples per year", breaks = c(1:12)) +
  theme_ms() +
  theme(legend.position = "bottom")



