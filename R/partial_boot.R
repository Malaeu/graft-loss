##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param reference
##' @param exposure
##' @param n_boots
partial_boot <- function(reference, exposure, n_boots = 1000){
  
  df_partial <- tibble(reference = reference, exposure = exposure)
  
  boots <- bootstraps(df_partial, m = n_boots) %>% 
    mutate(
      result = map(
        .x = splits,
        .f = ~ {
          
          boot_data <- training(.x)
          refr <- median(boot_data$reference)
          expo <- median(boot_data$exposure)
          
          list(
            prev = expo,
            ratio = expo / refr,
            diff = expo - refr
          )
          
        } 
      )
    ) %>% 
    unnest_wider(result)
  
  refr <- median(df_partial$reference)
  expo <- median(df_partial$exposure)
  
  estimate <- c(prev = expo, ratio = expo / refr, diff = expo - refr)
  
  bootBCa(
    estimate = estimate,
    estimates = as.matrix(boots[, c('prev','ratio','diff')]),
    n = length(exposure)
  ) %>% 
    t() %>% 
    cbind(est = estimate) %>% 
    as_tibble(rownames = 'measure') %>% 
    rename(lwr = `2.5%`, upr = `97.5%`) %>% 
    pivot_wider(names_from = measure,
                values_from = c(lwr, upr, est))
  
}
