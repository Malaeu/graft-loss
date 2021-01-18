
# FORMAT FOR CIRCULATION

the_plan <- drake_plan(
  
  min_txpl_year = 2010,
  predict_horizon = 1,
  ntimes = 1000, # only used 500 currently
  
  phts_all = clean_phts(
    min_txpl_year = min_txpl_year,
    predict_horizon = predict_horizon,
    time = outcome_int_graft_loss,
    status = outcome_graft_loss,
    case = 'snake', 
    set_to_na = c("", "unknown", "missing")
  ), 
  
  labels = make_labels(colname_variable = 'variable', colname_label = 'label'),
  
  resamples = mc_cv_light(phts_all, ntimes = ntimes),

  # Slurm code is run between the resamples and mc_cv targets.
  
  mc_cv = load_mc_cv(),
  
  fig_mc_cv_vals = visualize_mc_cv(mc_cv),
  
  fig_mc_cv_inf = visualize_mc_cv_inference(mc_cv, 
                                            ftr_method = 'rsf',
                                            n_pred = 20),
  
  final_features = make_final_features(phts_all),
  
  final_data = juice(prep(make_recipe(phts_all, dummy_code = FALSE))),
  
  final_model = fit_orsf(
    trn = final_data,
    vars = final_features$variables
  ),
  
  final_partial = make_final_partial(final_model = final_model, 
                                     final_data = final_data, 
                                     final_features = final_features),
  
  partial_table_data = make_partial_table_data(final_partial, labels),
  
  tbl_partial = tabulate_partial_table_data(partial_table_data),
  tbl_one = tabulate_characteristics(phts_all, labels)
  
  
)


