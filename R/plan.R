plan <- drake_plan(
  ##################################
  #### Data import and cleaning ####
  ##################################
  
  ## tceq assessment units
  au = download_au(url = "https://opendata.arcgis.com/datasets/175c3cb32f2840eca2bf877b93173ff9_4.zip?outSR=%7B%22falseM%22%3A-100000%2C%22xyTolerance%22%3A8.98315284119521e-9%2C%22mUnits%22%3A10000%2C%22zUnits%22%3A1%2C%22latestWkid%22%3A4269%2C%22zTolerance%22%3A2%2C%22wkid%22%3A4269%2C%22xyUnits%22%3A11258999068426.24%2C%22mTolerance%22%3A0.001%2C%22falseX%22%3A-400%2C%22falseY%22%3A-400%2C%22falseZ%22%3A0%7D",
                   rel_path = "/Surface_Water.shp"),
  
  ## find sites
  site_info = find_sites(au),
  ecoli_data = get_ecoli(site_info),
  
  ## power analysis
  ## uncomment to run, takes ~ 40 hours on quad core intel processor
  # mk_power = fit_power_mk(ecoli_data),
  # mk_power_dat = saveRDS(mk_power,
  #                       file = file_out("data/mk_power_dat.rds")),
  # lm_power = fit_power_lm(ecoli_data),
  # lm_power_dat = saveRDS(lm_power,
  #                        file = file_out("data/lm_power_dat.rds")),

  ## describe functional relationships
  dens_plot_mk_power = plot_mk_power(site_info = site_info,
                                     df_mk = file_in("data/mk_power_dat.rds"),
                                     df_lm = file_in("data/lm_power_dat.rds"),
                                     file_name = file_out("figures/fig_3.png"),
                                     width = 6.5,
                                     height = 3.5,
                                     units = "in",
                                     res = 300),
  
  logit_mk = model_mk_lhood(file_in("data/mk_power_dat.rds")),
  logit_lm = model_lm_lhood(file_in("data/lm_power_dat.rds")),

  lhood_plots = plot_lhood(logit_mk, 
                           logit_lm,
                           file_name = file_out("figures/fig_4.png"),
                           width = 6,
                           height = 3,
                           units = "in",
                           res = 300),
  
  lhood_table = make_glm_tables(logit_mk, 
                                logit_lm),
  ## make generic power figures
  ## uncomment to run. takes about 2 hours
  # power_figure_data = make_power_figure_data(ecoli_data),
  # save_power_figue_data = saveRDS(power_figure_data,
  #                                 file = file_out("data/power_fig_dat.rds")),
  
  power_figures = draw_power_figure("data/power_fig_dat.rds",
                                    file_name = file_out("figures/fig_5.png"),
                                    width = 6,
                                    height = 4.5,
                                    units = "in",
                                    res = 300),



  ## figures
  annual_sample_dist = plot_annual_distribution(ecoli_data,
                                                site_info,
                                                file_name = file_out("figures/fig_1.png"),
                                                width = 6,
                                                height = 3,
                                                units = "in",
                                                res = 300),

  ecoli_dist = plot_ecoli(ecoli_data,
                          site_info,
                          file_name = file_out("figures/fig_2.png"),
                          width = 6,
                          height = 3,
                          units = "in",
                          res = 300)
  
)


