##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param final_model
##' @param final_data
##' @param final_features
make_final_partial <- function(final_model, final_data, final_features) {
  
  map(
    .x = final_features$variables, 
    .f = ~ partial(
      model = final_model, 
      data = final_data, 
      variable_name = .x
    ) %>% 
      mutate(
        tmp = list(prediction[[1]]), 
        boot_results = map2(tmp, prediction, partial_boot),
        name = as.character(name),
        across(where(is.factor), as.factor)
      ) %>% 
      unnest_wider(boot_results) %>% 
      select(-prediction, -tmp)
  )

}
