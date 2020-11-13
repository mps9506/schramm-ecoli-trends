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
  #mk_power = fit_power_mk(ecoli_data),
  # mk_power_dat = saveRDS(mk_power,
  #                       file = file_out("data/mk_power_dat")),
  #lm_power = fit_power_lm(ecoli_data),
  #gam_power = fit_power_gam(ecoli_data),
  
  ## describe functional relationships
  dens_plot_mk_power = plot_mk_power(file_in("data/mk_power_dat"),
                                     file_name = file_out("figures/fig_3.png"),
                                     width = 140,
                                     height = 95,
                                     units = "mm",
                                     res = 300),
  
  
  ## figures
  annual_sample_dist = plot_annual_distribution(ecoli_data,
                                                site_info,
                                                file_name = file_out("figures/fig_1.png"),
                                                width = 140,
                                                height = 95,
                                                units = "mm",
                                                res = 300),

  ecoli_dist = plot_ecoli(ecoli_data,
                          site_info,
                          file_name = file_out("figures/fig_2.png"),
                          width = 140,
                          height = 95,
                          units = "mm",
                          res = 300)
  
)


