##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##'
##' @param phts_all
##' @param final_recipe 
##' @param labels 
##' @param final_features
tabulate_missingness <- function(final_recipe, 
                                 phts_all, 
                                 final_features, 
                                 labels) {

  data_miss_var <- phts_all %>% 
    select(all_of(final_features$variables)) %>% 
    miss_var_summary() %>% 
    mutate(pct_miss = table_value(pct_miss))
  
  impute_medians <- final_recipe$steps[[1]]$medians %>% 
    enframe() %>% 
    mutate(impute_with = table_value(as.numeric(value))) %>% 
    filter(name %in% final_features$variables) %>% 
    select(-value)
  
  impute_modes <- final_recipe$steps[[2]]$modes %>% 
    enframe() %>% 
    mutate(impute_with = as.character(value)) %>% 
    filter(name %in% final_features$variables) %>% 
    select(-value)
  
  data_impute_values <- bind_rows(impute_medians, impute_modes) %>% 
    rename(variable = name, impute_miss = impute_with)
  
  data_importance <- final_features$importance %>% 
    rename(variable = name, importance = value) %>% 
    mutate(variable = str_remove(variable, '\\.\\..*$')) %>% 
    group_by(variable) %>% 
    summarize(importance = mean(importance)) %>% 
    mutate(
      importance_rescaled = rescale(importance, 
                                    from = c(0, max(importance)),
                                    to = c(0,1))
    )
  
  data_table <- data_miss_var %>% 
    left_join(data_importance) %>% 
    left_join(data_impute_values) %>% 
    arrange(desc(importance)) %>% 
    mutate(importance_rank = 1:n()) %>% 
    left_join(labels$variables)
  
  inline <- data_table %>% 
    as_inline(
      tbl_variables = c('variable'),
      tbl_values = c('n_miss', 
                     'pct_miss',
                     'impute_miss',
                     'importance', 
                     'importance_rescaled',
                     'importance_rank')
    )
  
  ft <- data_table %>% 
    select(label, ends_with('miss'), starts_with('impo')) %>% 
    flextable() %>% 
    set_header_labels(label = "Predictor variable",
                      n_miss = 'Number',
                      pct_miss = 'Percent',
                      impute_miss = 'Imputed to',
                      importance = 'Change in C-statistic',
                      importance_rescaled = 'Scaled',
                      importance_rank = 'Rank') %>% 
    add_header_row(values = c("Predictor variable", 
                              "Missing values", 
                              "Permutation Importance"),
                   colwidths = c(1, 3, 3)) %>% 
    theme_box() %>% 
    merge_v(j = 1, part = 'header') %>% 
    align(align = 'center', part = 'all') %>% 
    align(align = 'left', part = 'all', j = 1) %>% 
    width(width = 1) %>% 
    width(j = 1, width = 2.5)
  
  list(
    inline = inline,
    table = ft
  )
  

}
