#' @export

as.regexport.summary.lm <- function(model, sumstats = NULL) {
  coef <- as.data.frame(model$coefficients, stringsAsFactors = FALSE)
  coef$var <- gsub("`", "", row.names(coef))
  coef$est <- coef$Estimate
  coef$se <- coef$`Std. Error`
  coef$p <- coef$`Pr(>|t|)`
  coef <- coef[c("var", "est", "se", "p")]
  row.names(coef) <- NULL
  
  sstats <- list(`R-squared` = model$r.squared,
                 `Adj. R-squared` = model$adj.r.squared,
                 `Sigma` = model$sigma,
                 `Degrees of Freedom` = paste(model$df, collapse = ", "),
                 `F Statistic` = sprintf("%1.3f (%1.0f, %1.0f)", model$fstatistic[["value"]], model$fstatistic[["numdf"]], model$fstatistic[["dendf"]]),
                 `Observations` = length(model$residuals))
  
  depvar <- as.character(attr(model$terms, "variables")[[2]])
  
  return(structure(list("lm", depvar, coef, sstats, sumstats), names = c("modeltype", "depvar", "coef", "sumstats", "sumstats.user"), class = "regexport"))
}