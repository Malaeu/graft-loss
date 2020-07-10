
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @param min_txpl_year 
##' @param predict_horizon 
##' @param case 
##' @param set_to_na 
##'
##' @title

clean_phts <- function(
  min_txpl_year, predict_horizon,
  time, status,
  case = 'snake', set_to_na = '') {
  
  time_quo <- enquo(time)
  status_quo <- enquo(status)

  # stored in private directory to prevent data leak
  out <- read_sas('../../data/phts_txpl_ml.sas7bdat') %>% 
    filter(TXPL_YEAR >= min_txpl_year) %>% 
    select(
      -c(
        LSPRA,
        LSCPRAT,
        LSCPRAB,
        LSPRADTE,
        LSPRDTET,
        LSPRDTEB,
        LSFCPRA,
        LSFPRAT,
        LSFPRAB,
        TXCPRA,
        CPRAT,
        CPRAB,
        TXPRADTE,
        CPRADTET,
        CPRADTEB,
        TXFCPRA,
        FPRAT,
        FPRAB
      )
    ) %>% 
    clean_names() %>%
    mutate(
      # prevent improper names if you one-hot encode
      across(
        .cols = where(is.character), 
        ~ clean_chr(.x, case = case, set_to_na = set_to_na)
      ),
      # set event times of 0 to something non-zero but still small
      across(
        .cols = c(outcome_waitlist_interval, outcome_int_graft_loss),
        ~ replace(.x, list = .x == 0, values = 1/365)
      ),
      across(.cols = where(is.character), as.factor),
      # truncate survival data to match prediction horizon
      # outcome_int_graft_loss = pmin(outcome_int_graft_loss, predict_horizon + 1/365),
      # outcome_graft_loss = if_else(
      #   condition = outcome_int_graft_loss > predict_horizon, 
      #   true = 0, false = outcome_graft_loss 
      # ),
      prim_dx = fct_collapse(
        .f = factor(prim_dx),
        other = c('cardiac_tumor', 'myocarditis', 'other_specify')
      )
    ) %>% 
    rename(
      time = !!time_quo,
      status = !!status_quo
    ) %>% 
    select(-starts_with('outcome'))
  
  too_many_missing <- miss_var_summary(data = out) %>% 
    filter(pct_miss > 30) %>% 
    pull(variable)
  
  out[, too_many_missing] <- NULL
  
  out
  
}
