as.regexport.clogit <- function(model, sumstats = NULL) {
  ms <- summary(model)
  fam <- "clogit"
  
  coef <- as.data.frame(ms$coefficients, stringsAsFactors = FALSE)
  coef$var <- gsub("`", "", row.names(coef))
  coef <- coef[c("var", "coef", "se(coef)", "Pr(>|z|)")]
  names(coef) <- c("var", "est", "se", "p")
  row.names(coef) <- NULL
  
  sstats <- list(Concordance = ms$concordance[["C"]],
                 `  SE` = ms$concordance[["se(C)"]],
                 `Likelihood ratio test` = ms$logtest[["test"]],
                 `  Degrees of freedom` = ms$logtest[["df"]],
                 `  P` = ms$logtest[["pvalue"]],
                 `Wald test` = ms$waldtest[["test"]],
                 `  Degrees of freedom` = ms$waldtest[["df"]],
                 `  P` = ms$waldtest[["pvalue"]],
                 `Score (logrank) test` = ms$sctest[["test"]],
                 `  Degrees of freedom` = ms$sctest[["df"]],
                 `  P` = ms$sctest[["pvalue"]],
                 `Observations` = ms$nevent)
  
  depvar <- as.character(attr(ms$terms, "variables")[2])
  
  return(structure(list(c("glm", fam), depvar, coef, sstats, sumstats), 
                   names = c("modeltype", "depvar", "coef", "sumstats", "sumstats.user"), class = "regexport"))
}
