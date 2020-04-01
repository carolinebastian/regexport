as.regexport.lm <- function(model, sumstats = NULL) {
  
  ms <- summary(model)
  
  coef <- as.data.frame(ms$coefficients, stringsAsFactors = FALSE)
  coef$var <- gsub("`", "", row.names(coef))
  coef$est <- coef$Estimate
  coef$se <- coef$`Std. Error`
  coef$p <- coef$`Pr(>|t|)`
  coef <- coef[c("var", "est", "se", "p")]
  row.names(coef) <- NULL
  
  sstats <- list(`R-squared` = ms$r.squared,
                 `Adj. R-squared` = ms$adj.r.squared,
                 `Sigma` = ms$sigma,
                 `Degrees of Freedom` = paste(ms$df, collapse = ", "),
                 `F Statistic` = sprintf("%1.3f (%1.0f, %1.0f)", ms$fstatistic[["value"]], ms$fstatistic[["numdf"]], ms$fstatistic[["dendf"]]),
                 `Observations` = stats::nobs(model))

  depvar <- as.character(attr(ms$terms, "variables")[2])
  
  return(structure(list("lm", depvar, coef, sstats, sumstats), names = c("modeltype", "depvar", "coef", "sumstats", "sumstats.user"), class = "regexport"))
}
