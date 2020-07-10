
the_plan <- drake_plan(
  
  min_txpl_year = 2010,
  time_cutpoint = 2017,
  predict_horizon = 1,
  
  xgb_params = list(
    eta                = 0.02,
    num_parallel_trees = 5,
    max_depth          = 3,
    gamma              = 2.5,
    min_child_weight   = 3.5,
    subsample          = 2/3,
    colsample_bynode   = 1/2,
    objective          = "survival:cox",
    eval_metric        = "cox-nloglik"
  ),
  
  phts_all = clean_phts(
    min_txpl_year = min_txpl_year,
    predict_horizon = predict_horizon,
    time = outcome_int_graft_loss,
    status = outcome_graft_loss,
    case = 'snake', 
    set_to_na = c("", "unknown", "missing")
  ), 
  
  labels = make_labels(colname_variable = 'variable', colname_label = 'label'),
  recipe = make_recipe(phts_all),
  boots = bootstraps(phts_all, times = 5) %>% 
    boot_preproc(recipe = recipe)
   
)


