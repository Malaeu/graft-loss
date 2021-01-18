##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##'
##' @param trn 
##' @param tst 
##' @param n_predictors 
##' @param predict_horizon 
##' 
fit_xgb <- function(trn,
                    vars,
                    tst = NULL,
                    predict_horizon = NULL){

  trn_x <- as.matrix(select(trn, -c(time, status)))
  trn_y <- as.matrix(select(trn, c(time, status)))
  
  xgb_label <- trn_y[, 1]
  censored <- trn_y[, 2] == 0
  xgb_label[censored] <- xgb_label[censored] * (-1)
  
  model <- sgb_fit(
    sgb_df = sgb_data(trn_x[, vars], xgb_label), 
    verbose = 0,
    params = list(
      eta = 0.01,
      max_depth = 3,
      gamma = 1/2,
      min_child_weight = 2,
      subsample = 1/2,
      colsample_bynode = 1/2,
      objective = "survival:cox",
      eval_metric = "cox-nloglik"
    )
  ) 
  
  if(is.null(tst)) return(model)
  
  if(is.null(predict_horizon)) stop("specify prediction horizon", call. = F)
  
  1 - predict(
    model,
    new_data = as.matrix(tst[, vars]),
    eval_times = predict_horizon
  )
  
}
