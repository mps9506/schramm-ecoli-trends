readd(mk_power) %>%
  unnest(c(p_est, power_chart_mk)) %>%
  mutate(y = case_when(
    power >= 0.8 ~ 1,
    power < 0.8 ~ 0
  )) %>%
  filter(samples_per_year <= 12) -> temp_df

m1 <- glm(y ~ cv + samples_per_year + p.change,
    data = temp_df,
    family = binomial(link = "logit"))





fits <- ggpredict(model = m1,
         terms = c("samples_per_year[1:12]","p.change"))

ggplot(fits) +
  geom_line(aes(x = x,
                y = predicted,
                group = group,
                color = group)) +
  geom_ribbon(aes(x,
                  ymin = conf.low,
                  ymax = conf.high,
                  group = group,
                  fill = group), alpha = 0.5) +
  theme_ms()
