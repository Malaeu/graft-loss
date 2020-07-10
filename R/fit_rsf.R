##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param boots
fit_rsf <- function(data, nvars, ...) {

  fit_full <- ranger(formula = Surv(time, status) ~ ., data = boots$trn[[1]], 
    num.trees = 100, importance = 'permutation', min.node.size = 10, 
    replace = FALSE, splitrule = 'C')
  
  rdcd_variables <- c('time', 'status', get_topvars(fit_full, nvars = nvars))
  
  fit_rdcd_rsf <- ranger(formula = Surv(time, status) ~ ., 
    data = boots$trn[[1]][, rdcd_variables],
    num.trees = 500, min.node.size = 10, 
    replace = FALSE, splitrule = 'C')
  
  prd_rdcd_rsf <- predict(fit_rdcd_rsf, data = boots$tst[[1]])
  
  
  
  Score(
    object = list(1-prd_rdcd_rsf$survival[, ncol(prd_rdcd_rsf$survival), drop = T]),
    metric = 'AUC',
    formula = Surv(time, status) ~ 1, 
    data = boots$tst[[1]],
    summary = 'IPA',
    times = 1.0026
  )
  
  
  
}


get_topvars <- function(fit, nvars){
  fit$variable.importance %>% 
    enframe() %>% 
    arrange(desc(value)) %>% 
    slice(1:nvars) %>% 
    pull(name)
}
