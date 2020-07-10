##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param boots
##' @param recipe
boot_preproc <- function(boots, recipe) {

  boots %>% 
    mutate(
      trn_raw = map(splits, training),
      tst_raw = map(splits, testing),
      preproc = map(trn_raw, ~prep(recipe, training = .x)),
      trn = map(preproc, juice),
      tst = map2(preproc, tst_raw, bake)
    ) %>% 
    select(id, trn, tst)

}
