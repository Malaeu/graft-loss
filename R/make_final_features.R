##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param phts_all
make_final_features <- function(phts_all, 
                                n_trees = 500, 
                                n_predictors = 20) {

  pre_proc_ftr_selector <- phts_all %>% 
    make_recipe() %>% 
    prep() %>% 
    juice() %>% 
    select(-ID)
  
  ftr_importance <- select_rsf(trn = pre_proc_ftr_selector,
                               n_predictors = n_predictors,
                               num.trees = n_trees,
                               return_importance = TRUE)
  
  ftrs <- pull(ftr_importance, name)
  
  ftrs_as_variables <- ftrs %>% 
    str_split('\\.\\.') %>% 
    map_chr(~.x[1]) %>% 
    unique()
  
  list(terms = ftrs,
       variables = ftrs_as_variables,
       importance = ftr_importance)

}
