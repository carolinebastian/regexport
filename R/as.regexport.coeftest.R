#' Coerce coeftest to class regexport
#' 
#' Apply revised standard errors from a coeftest (from package lmtest) to
#' a model and coerce into class regexport.
#' 
#' @param coeftest An object of class coeftest
#' @param model The lm or glm model from which the coeftest was calculated
#' @param sumstats Optional named list of additional summary statistics
#' 
#' @return An object of class regexport
#' 
#' @example 
#' library("lmtest")
#' data("Mandible", package = "lmtest")
#' fm <- lm(length ~ age, data = Mandible, subset=(age <= 28))
#' ct <- coeftest(fm, df = Inf)
#' 
#' as.regexport(ct, fm)
#' 
#' @export

as.regexport.coeftest <- function(coeftest, model, sumstats = NULL) {
  coef <- data.frame(var = gsub("`", "", row.names(coeftest)), 
                     est = coeftest[, "Estimate"], 
                     se = coeftest[, "Std. Error"],
                     stringsAsFactors = FALSE)
  row.names(coef) <- NULL
  
  coef$p = tryCatch(coeftest[, "Pr(>|t|)"], error = function(e) coef$p = coeftest[, "Pr(>|z|)"])
  
  m1 <- as.regexport(model)
  m1$coef <- coef
  m1$modeltype <- c("coef", m1$modeltype)
  return(m1)
}
