readd(logit_mk)


gtsummary::tbl_regression(readd(logit_mk), exponentiate = TRUE,
                          label = list(cv ~ "cv",
                                       samples_per_year ~ "Sample Size",
                                       p.change ~ "Effect Size"))

gtsummary::tbl_regression(readd(logit_lm), exponentiate = TRUE,
                          label = list(cv ~ "cv",
                                       samples_per_year ~ "Sample Size",
                                       p.change ~ "Effect Size"))
