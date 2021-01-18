##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param trn 
##' @param tst 
##' @param return_fit 
##' @param predict_horizon 
##' @param n_predictors 
##'
##' @title
fit_rsf <- function(trn,
                    vars,
                    tst = NULL,
                    predict_horizon = NULL) {
  
  model <- ranger(
    formula = Surv(time, status) ~ .,
    data = trn[, c('time', 'status', vars)],
    num.trees = 1000,
    min.node.size = 10,
    splitrule = 'C'
  )
  
  if(is.null(tst)) return(model)
  
  if(is.null(predict_horizon)) stop("specify prediction horizon", call. = F)
  
  ranger_predictrisk(model, 
                     newdata = tst, 
                     times = predict_horizon)

}


