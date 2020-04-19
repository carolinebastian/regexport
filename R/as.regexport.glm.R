#' @export

as.regexport.glm <- function(model, sumstats = NULL) {
  
  ms <- summary(model)
  fam <- stats::family(model)$family
  
  coef <- as.data.frame(ms$coefficients, stringsAsFactors = FALSE)
  coef$var <- gsub("`", "", row.names(coef))
  coef$est <- coef$Estimate
  coef$se <- coef$`Std. Error`
  coef$p <- coef$`Pr(>|z|)`
  if(is.null(coef$p)) coef$p <- coef$`Pr(>|t|)`
  coef <- coef[c("var", "est", "se", "p")]
  row.names(coef) <- NULL
  
  sstats <- list(`Deviance` = ms$deviance,
                 `AIC` = ms$aic,
                 `Residual Degrees of Freedom` = ms$df.residual,
                 `Null Deviance` = ms$null.deviance,
                 `Null Degrees of Freedom` = ms$df.null,
                 `Dispersion` = ms$dispersion,
                 `Degrees of Freedom` = paste(ms$df, collapse = ", "),
                 `Observations` = stats::nobs(model))

  depvar <- as.character(attr(ms$terms, "variables")[2])
  
  return(structure(list(c("glm", fam), depvar, coef, sstats, sumstats), 
                   names = c("modeltype", "depvar", "coef", "sumstats", "sumstats.user"), class = "regexport"))
}
