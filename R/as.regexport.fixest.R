#' @export

as.regexport.fixest <- function(model, ..., sumstats = NULL) {
  
  ms <- summary(model, ...)
  fam <- model$call$family
  
  coef <- data.frame(var = row.names(ms$coeftable), ms$coeftable)
  names(coef)[names(coef) %in% "Estimate"] <- "est"
  names(coef)[names(coef) %in% "Std..Error"] <- "se"
  names(coef)[names(coef) %in% "Pr...t.."] <- "p"
  
  coef <- coef[c("var", "est", "se", "p")]
  row.names(coef) <- NULL
  
  sstats <- list(`Deviance` = ms$deviance,
                 `AIC` = AIC(ms),
                 `BIC` = BIC(ms),
                 `Residual Degrees of Freedom` = ms$df.residual,
                 `Null Deviance` = ms$deviance,
                 `Dispersion` = ms$dispersion,
                 `Observations` = stats::nobs(model),
                 `Pseudo R-Squared` = ms$pseudo_r2,
                 `Log Likelihood` = ms$loglik)

  depvar <- as.character(formula(model))[2]
  
  return(structure(list(c(model$method, fam), depvar, coef, sstats, sumstats), 
                   names = c("modeltype", "depvar", "coef", "sumstats", 
                             "sumstats.user"), class = "regexport"))
}
