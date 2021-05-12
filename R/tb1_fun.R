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
  
  if(is_normal(x)){
    
    return(
      tibble(
        level = NA_character_,
        smry_value = table_glue("{mean(x, na.rm=T)} ({sd(x, na.rm=T)})")
      )
    )
    
  } else {
    
    lwr <- quantile(x, probs = 0.25, na.rm = TRUE)
    upr <- quantile(x, probs = 0.75, na.rm = TRUE)
    
    return(
      tibble(
        level = NA_character_,
        smry_value = table_glue("{median(x, na.rm=T)} ({lwr}, {upr})")
      )
    )
    
  }
  
  
  
  
}


is_normal <- function(x){
  
  shapiro.test(x)$p.value > 0.05
  
}