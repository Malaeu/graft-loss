##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param model
##' @param data
##' @param variable_name
##' @param variable_values
partial_catg <- function(model, data, variable_name, variable_values = NULL) {

  if(is.null(variable_values)){
    
    variable_values <- sort(unique(data[[variable_name]]))
    
  }
  
  .partial(model, data, variable_name, variable_values)

}


