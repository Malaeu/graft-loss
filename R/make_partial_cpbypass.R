##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param final_model
##' @param final_fata
make_partial_cpbypass <- function(final_model, final_data) {
  
  variable_values <- quantile(final_data$cpbypass, 
                              probs = seq(0.2, 0.8, length.out = 15))
  
  partial_data <- .partial(
    model = final_model,
    data = final_data,
    variable_name = 'cpbypass',
    variable_values = variable_values
  )
  
}
