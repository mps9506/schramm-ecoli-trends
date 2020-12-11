Estimating statistical power for detecting long term trends in surface
water *Escherichia coli* concentrations
================

[![License: CC
BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

Data and code for Schramm, “Estimating statistical power for detecting
long term trends in surface water *Escherichia coli* concentrations.”

# Introduction

This project uses [R](https://www.r-project.org/) and the
[`drake`](https://docs.ropensci.org/drake/) package to manage workflow
and promote reproducibility. The simulations used in the manuscript take
approximately 40 hours to run on a laptop Intel processor. Therefore,
the simulations outputs are saved as `.rds` files that can be loaded in
any R session:

    data/lm_power_dat.rds
    data/mk_power_dat.rds
    data/power_fig_dat.rds

# Build the project

The following packages are loaded by the project, make sure they are
installed:

    ##  package       * version  date       lib source                            
    ##  archive       * 1.0.0    2020-10-09 [1] Github (jimhester/archive@0a05060)
    ##  assertthat      0.2.1    2019-03-21 [1] CRAN (R 3.6.2)                    
    ##  backports       1.1.5    2019-10-02 [1] CRAN (R 3.6.1)                    
    ##  base64enc       0.1-3    2015-07-28 [1] CRAN (R 3.6.0)                    
    ##  base64url       1.4      2018-05-14 [1] CRAN (R 3.6.2)                    
    ##  betareg       * 3.1-2    2019-05-16 [1] CRAN (R 3.6.2)                    
    ##  boot            1.3-24   2019-12-20 [1] CRAN (R 3.6.2)                    
    ##  broom           0.7.2    2020-10-20 [1] CRAN (R 3.6.3)                    
    ##  callr           3.4.1    2020-01-24 [1] CRAN (R 3.6.2)                    
    ##  cellranger      1.1.0    2016-07-27 [1] CRAN (R 3.6.2)                    
    ##  class           7.3-15   2019-01-01 [1] CRAN (R 3.6.2)                    
    ##  classInt        0.4-3    2020-04-07 [1] CRAN (R 3.6.3)                    
    ##  cli             2.0.2    2020-02-28 [1] CRAN (R 3.6.3)                    
    ##  codetools       0.2-16   2018-12-24 [1] CRAN (R 3.6.2)                    
    ##  colorspace      1.4-1    2019-03-18 [1] CRAN (R 3.6.1)                    
    ##  crayon        * 1.3.4    2017-09-16 [1] CRAN (R 3.6.2)                    
    ##  data.table      1.12.8   2019-12-09 [1] CRAN (R 3.6.2)                    
    ##  dataRetrieval * 2.7.6    2020-03-11 [1] CRAN (R 3.6.3)                    
    ##  DBI             1.1.0    2019-12-15 [1] CRAN (R 3.6.2)                    
    ##  dbplyr          1.4.2    2019-06-17 [1] CRAN (R 3.6.2)                    
    ##  DescTools     * 0.99.32  2020-01-17 [1] CRAN (R 3.6.2)                    
    ##  digest          0.6.25   2020-02-23 [1] CRAN (R 3.6.3)                    
    ##  dplyr         * 1.0.2    2020-08-18 [1] CRAN (R 3.6.3)                    
    ##  drake         * 7.9.0    2020-01-08 [1] CRAN (R 3.6.2)                    
    ##  e1071           1.7-3    2019-11-26 [1] CRAN (R 3.6.2)                    
    ##  ellipsis        0.3.1    2020-05-15 [1] CRAN (R 3.6.3)                    
    ##  EnvStats      * 2.4.0    2020-10-21 [1] CRAN (R 3.6.3)                    
    ##  evaluate        0.14     2019-05-28 [1] CRAN (R 3.6.2)                    
    ##  expm            0.999-4  2019-03-21 [1] CRAN (R 3.6.2)                    
    ##  extrafont     * 0.17     2014-12-08 [1] CRAN (R 3.6.2)                    
    ##  extrafontdb     1.0      2012-06-11 [1] CRAN (R 3.6.0)                    
    ##  fansi           0.4.1    2020-01-08 [1] CRAN (R 3.6.2)                    
    ##  filelock        1.0.2    2018-10-05 [1] CRAN (R 3.6.2)                    
    ##  fitdistrplus  * 1.1-1    2020-05-19 [1] CRAN (R 3.6.3)                    
    ##  flexmix         2.3-15   2019-02-18 [1] CRAN (R 3.6.2)                    
    ##  flextable     * 0.5.10   2020-05-15 [1] CRAN (R 3.6.3)                    
    ##  forcats       * 0.5.0    2020-03-01 [1] CRAN (R 3.6.3)                    
    ##  Formula         1.2-3    2018-05-03 [1] CRAN (R 3.6.0)                    
    ##  fs              1.5.0    2020-07-31 [1] CRAN (R 3.6.3)                    
    ##  furrr         * 0.1.0    2018-05-16 [1] CRAN (R 3.6.2)                    
    ##  future        * 1.19.1   2020-09-22 [1] CRAN (R 3.6.3)                    
    ##  future.callr  * 0.5.0    2019-09-28 [1] CRAN (R 3.6.3)                    
    ##  gdtools         0.2.1    2019-10-14 [1] CRAN (R 3.6.2)                    
    ##  generics        0.0.2    2018-11-29 [1] CRAN (R 3.6.2)                    
    ##  ggeffects     * 0.14.3   2020-04-20 [1] CRAN (R 3.6.3)                    
    ##  ggplot2       * 3.3.2    2020-06-19 [1] CRAN (R 3.6.3)                    
    ##  ggridges      * 0.5.2    2020-01-12 [1] CRAN (R 3.6.3)                    
    ##  globals         0.12.5   2019-12-07 [1] CRAN (R 3.6.1)                    
    ##  glue            1.4.2    2020-08-27 [1] CRAN (R 3.6.3)                    
    ##  gtable          0.3.0    2019-03-25 [1] CRAN (R 3.6.2)                    
    ##  gtsummary     * 1.3.5    2020-09-29 [1] CRAN (R 3.6.3)                    
    ##  haven           2.2.0    2019-11-08 [1] CRAN (R 3.6.2)                    
    ##  hms             0.5.3    2020-01-08 [1] CRAN (R 3.6.2)                    
    ##  hrbrthemes    * 0.8.0    2020-03-06 [1] CRAN (R 3.6.3)                    
    ##  htmltools       0.5.0    2020-06-16 [1] CRAN (R 3.6.3)                    
    ##  httr            1.4.2    2020-07-20 [1] CRAN (R 3.6.3)                    
    ##  igraph          1.2.5    2020-03-19 [1] CRAN (R 3.6.3)                    
    ##  insight         0.8.5    2020-06-08 [1] CRAN (R 3.6.3)                    
    ##  jsonlite        1.7.0    2020-06-25 [1] CRAN (R 3.6.3)                    
    ##  KernSmooth      2.23-16  2019-10-15 [1] CRAN (R 3.6.2)                    
    ##  knitr           1.29     2020-06-23 [1] CRAN (R 3.6.2)                    
    ##  lattice         0.20-38  2018-11-04 [1] CRAN (R 3.6.2)                    
    ##  lifecycle       0.2.0    2020-03-06 [1] CRAN (R 3.6.3)                    
    ##  listenv         0.8.0    2019-12-05 [1] CRAN (R 3.6.2)                    
    ##  lmtest          0.9-37   2019-04-30 [1] CRAN (R 3.6.2)                    
    ##  lubridate       1.7.4    2018-04-11 [1] CRAN (R 3.6.2)                    
    ##  magrittr        2.0.1    2020-11-17 [1] CRAN (R 3.6.3)                    
    ##  MASS          * 7.3-51.5 2019-12-20 [1] CRAN (R 3.6.2)                    
    ##  Matrix          1.2-18   2019-11-27 [1] CRAN (R 3.6.2)                    
    ##  modelr          0.1.5    2019-08-08 [1] CRAN (R 3.6.2)                    
    ##  modeltools      0.2-22   2018-07-16 [1] CRAN (R 3.6.0)                    
    ##  munsell         0.5.0    2018-06-12 [1] CRAN (R 3.6.2)                    
    ##  mvtnorm         1.0-11   2019-06-19 [1] CRAN (R 3.6.0)                    
    ##  nnet            7.3-12   2016-02-02 [1] CRAN (R 3.6.2)                    
    ##  officer         0.3.15   2020-11-01 [1] CRAN (R 3.6.2)                    
    ##  pillar          1.4.6    2020-07-10 [1] CRAN (R 3.6.3)                    
    ##  pkgconfig       2.0.3    2019-09-22 [1] CRAN (R 3.6.2)                    
    ##  plyr            1.8.6    2020-03-03 [1] CRAN (R 3.6.3)                    
    ##  processx        3.4.3    2020-07-05 [1] CRAN (R 3.6.2)                    
    ##  ps              1.3.0    2018-12-21 [1] CRAN (R 3.6.2)                    
    ##  purrr         * 0.3.4    2020-04-17 [1] CRAN (R 3.6.3)                    
    ##  R6              2.4.1    2019-11-12 [1] CRAN (R 3.6.2)                    
    ##  ragg          * 0.4.0    2020-10-05 [1] CRAN (R 3.6.3)                    
    ##  Rcpp            1.0.5    2020-07-06 [1] CRAN (R 3.6.3)                    
    ##  readr         * 1.3.1    2018-12-21 [1] CRAN (R 3.6.2)                    
    ##  readxl          1.3.1    2019-03-13 [1] CRAN (R 3.6.2)                    
    ##  reprex          0.3.0    2019-05-16 [1] CRAN (R 3.6.3)                    
    ##  rlang           0.4.7    2020-07-09 [1] CRAN (R 3.6.3)                    
    ##  rmarkdown       2.3      2020-06-18 [1] CRAN (R 3.6.3)                    
    ##  rstudioapi      0.10     2019-03-19 [1] CRAN (R 3.6.2)                    
    ##  Rttf2pt1        1.3.8    2020-01-10 [1] CRAN (R 3.6.2)                    
    ##  rvest         * 0.3.6    2020-07-25 [1] CRAN (R 3.6.3)                    
    ##  sandwich        2.5-1    2019-04-06 [1] CRAN (R 3.6.2)                    
    ##  scales          1.1.0    2019-11-18 [1] CRAN (R 3.6.2)                    
    ##  sessioninfo     1.1.1    2018-11-05 [1] CRAN (R 3.6.2)                    
    ##  sf            * 0.9-5    2020-07-14 [1] CRAN (R 3.6.3)                    
    ##  sjlabelled      1.1.3    2020-01-28 [1] CRAN (R 3.6.2)                    
    ##  storr           1.2.1    2018-10-18 [1] CRAN (R 3.6.2)                    
    ##  stringi         1.4.6    2020-02-17 [1] CRAN (R 3.6.2)                    
    ##  stringr       * 1.4.0    2019-02-10 [1] CRAN (R 3.6.2)                    
    ##  survival      * 3.1-8    2019-12-03 [1] CRAN (R 3.6.2)                    
    ##  systemfonts     0.3.2    2020-09-29 [1] CRAN (R 3.6.3)                    
    ##  textshaping     0.1.2    2020-10-08 [1] CRAN (R 3.6.3)                    
    ##  tibble        * 3.0.3    2020-07-10 [1] CRAN (R 3.6.3)                    
    ##  tidyr         * 1.1.2    2020-08-27 [1] CRAN (R 3.6.3)                    
    ##  tidyselect      1.1.0    2020-05-11 [1] CRAN (R 3.6.3)                    
    ##  tidyverse     * 1.3.0    2019-11-21 [1] CRAN (R 3.6.2)                    
    ##  txtq            0.2.0    2019-10-15 [1] CRAN (R 3.6.2)                    
    ##  units           0.6-7    2020-06-13 [1] CRAN (R 3.6.3)                    
    ##  uuid            0.1-4    2020-02-26 [1] CRAN (R 3.6.3)                    
    ##  vctrs           0.3.4    2020-08-29 [1] CRAN (R 3.6.3)                    
    ##  withr           2.1.2    2018-03-15 [1] CRAN (R 3.6.2)                    
    ##  xfun            0.15     2020-06-21 [1] CRAN (R 3.6.3)                    
    ##  xml2          * 1.3.2    2020-04-23 [1] CRAN (R 3.6.3)                    
    ##  yaml            2.2.0    2018-07-25 [1] CRAN (R 3.6.2)                    
    ##  zip             2.1.1    2020-08-27 [1] CRAN (R 3.6.3)                    
    ##  zoo             1.8-7    2020-01-10 [1] CRAN (R 3.6.2)                    
    ## 
    ## [1] C:/Users/michael.schramm/Documents/R/R-3.6.2/library

To replicate the project:

``` r
###########################
#### Load source files ####
###########################

source("R/packages.R")  # loads packages
source("R/download_data.R")
source("R/power.R")
source("R/GAM.R")
source("R/figures.R")
source("R/plan.R")      # creates the drake plan

## Uncomment this if you want custom fonts in
## ggplot figures. Otherwise no big deal, will
## revert to default.
#extrafont::loadfonts("win")

drake::make(
  plan
)
```

# Simulations

To rerun the simulations uncomment the following lines in `R\plan.R`
before running the code chunk above:

``` 
  ## power analysis
  ## uncomment to run, takes ~ 40 hours on quad core intel processor
  # mk_power = fit_power_mk(ecoli_data),
  # mk_power_dat = saveRDS(mk_power,
  #                       file = file_out("data/mk_power_dat.rds")),
  # lm_power = fit_power_lm(ecoli_data),
  # lm_power_dat = saveRDS(lm_power,
  #                        file = file_out("data/lm_power_dat.rds")),
```

and

``` 
  ## uncomment to run. takes about 2 hours
  # power_figure_data = make_power_figure_data(ecoli_data),
  # save_power_figue_data = saveRDS(power_figure_data,
  #                                 file = file_out("data/power_fig_dat.rds")),
```

Code and data are shared under the CC By 4.0 license. If you use the
code or data, please site this source per the DOI and the that of the
paper (Schramm, forthcoming).
