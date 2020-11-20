


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



fit_gam <- function(site_info,
                    df_mk,
                    df_lm) {
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
      method = "GLM")
  
  df_both <- df_both %>%
    bind_rows(df) %>%
    mutate(tmdl = as.factor(tmdl),
           p.change = as.factor(p.change))
    
  df_both %>%
    group_by(p.change, method) %>%
    nest() %>%
    mutate(
      gams = map(.x = data, 
                 ~ gam(power ~ s(mu, bs = "cr") + s(sd, bs = "cr") + s(samples_per_year, bs = "cr", k = 3),
                       data = .x,
                       family = betar(link = "logit", eps = .Machine$double.eps*1e6),
                       method = "ML")),
      predictions = map(.x = gams,
                        ~ ggpredict(.x, terms = c("samples_per_year", "sd [quart]")))) %>%
    ungroup() -> df_both
  
  df_both %>%
    unnest(predictions) %>%
    mutate(x = as.character(x),
           x = as.numeric(x))
  
  # p1 <- plot(df_both$predictions[[1]], 
  #      log.y = TRUE, 
  #      ci.style = "errorbar") + 
  #   scale_x_continuous("samples per year", breaks = c(1:12)) +
  #   scale_color_brewer(name = "log standard deviation",
  #                      palette = "Set1") +
  #   theme_ms() +
  #   theme(legend.position = "bottom") +
  #   labs(title = NULL)
  # p1
}


temp <- fit_gam(site_info = readd(site_info),
        df_mk = "data/mk_power_dat.rds",
        df_lm = "data/lm_power_dat.rds")

## need to convert x to numeric, but as.numeric converts the levels to numeric not the label
temp %>%
  mutate(group = as.character(group),
         group = as.numeric(group),
         group = exp(group)) -> temp
ggplot(temp) +
  #geom_point(aes(x, predicted, color = p.change)) +
  geom_pointrange(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, color = p.change)) +
  scale_x_continuous("samples per year", breaks = c(1:12)) +
  #scale_y_continuous(trans = "log10") +
  facet_grid(vars(method), vars(group)) +
  theme_ms(grid = "Y")
