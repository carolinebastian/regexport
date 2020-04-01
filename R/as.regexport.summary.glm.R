as.regexport.summary.glm <- function(model, sumstats = NULL) {
  
  fam <- model$family$family
  
  coef <- as.data.frame(model$coefficients, stringsAsFactors = FALSE)
  coef$var <- gsub("`", "", row.names(coef))
  coef$est <- coef$Estimate
  coef$se <- coef$`Std. Error`
  coef$p <- coef$`Pr(>|z|)`
  if(is.null(coef$p)) coef$p <- coef$`Pr(>|t|)`
  coef <- coef[c("var", "est", "se", "p")]
  row.names(coef) <- NULL
  
  sstats <- list(`Deviance` = model$deviance,
                 `AIC` = model$aic,
                 `Residual Degrees of Freedom` = model$df.residual,
                 `Null Deviance` = model$null.deviance,
                 `Null Degrees of Freedom` = model$df.null,
                 `Dispersion` = model$dispersion,
                 `Degrees of Freedom` = paste(model$df, collapse = ", "),
                 `Observations` = model$df[2] + model$df[3]) # I think that's right
  
  depvar <- as.character(attr(model$terms, "variables")[[2]])
  
  return(structure(list(c("glm", fam), depvar, coef, sstats, sumstats), 
                   names = c("modeltype", "depvar", "coef", "sumstats", "sumstats.user"), class = "regexport"))
}