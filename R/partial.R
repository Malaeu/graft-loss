##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param model
##' @param data
##' @param variable_name
##' @param variable_values
partial <- function(model, data, variable_name, variable_values = NULL) {
  
  variable <- data[[variable_name]]
  variable_unique_count <- length(unique(na.omit(variable)))
  
  if(variable_unique_count <= 3)
    return(partial_catg(model, data, variable_name, variable_values))
  
  if(is.numeric(variable)) 
    return(partial_ctns(model, data, variable_name, variable_values))
  
  if(is.factor(variable) || is.character(variable))
    return(partial_catg(model, data, variable_name, variable_values))
  
  stop("unsupported type", call. = FALSE)
  
}






##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param model
##' @param data
##' @param variable_name
##' @param variable_values
.partial <- function(model, data, variable_name, variable_values){
  
  partial <- enframe(variable_values) %>% 
    mutate(variable = variable_name, .before = 1) %>% 
    mutate(prediction = list(NULL))
  
  for(v in seq_along(variable_values)){
    
    trn_pd <- data
    trn_pd[[variable_name]] <- variable_values[v]
    partial$prediction[[v]] <- 1 - predict(model, newdata = trn_pd, times = 1)
    
  }
  
  partial
  
}
