##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param x
tb1_fun <- function(x){
  
  if(is.factor(x)) return(
    as.data.frame(100 * table(x) / sum(table(x))) %>%
      set_names('level', 'smry_value') %>%
      as_tibble() %>%
      mutate(smry_value = table_glue("{smry_value}%"))
  )
  
  tibble(
    level = NA_character_,
    smry_value = table_glue("{mean(x, na.rm=T)} ({sd(x, na.rm=T)})")
  )
  
  
}
