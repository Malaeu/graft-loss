

the_plan <- drake_plan(
  
  min_txpl_year = 2010,
  time_cutpoint = 2017,
  predict_horizon = 1,
  
  phts_all = clean_phts(
    min_txpl_year = min_txpl_year,
    predict_horizon = predict_horizon,
    time = outcome_int_graft_loss,
    status = outcome_graft_loss,
    case = 'snake', 
    set_to_na = c("", "unknown", "missing")
  ), 
  
  labels = make_labels(colname_variable = 'variable', colname_label = 'label'),
  
)


