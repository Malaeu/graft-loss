##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param trn
##' @param vars
##' @param tst
##' @param predict_horizon
fit_orsf <- function(trn,
                     vars,
                     tst = NULL,
                     predict_horizon = NULL) {
  
  model <- ORSF(trn[, c('time', 'status', vars)], ntree = 1000)
  
  if(is.null(tst)) return(model)
  
  if(is.null(predict_horizon)) stop("specify prediction horizon", call. = F)
  
  1 - predict(model,
              newdata = tst[, c('time', 'status', vars)],
              times = predict_horizon)
  
  
}
