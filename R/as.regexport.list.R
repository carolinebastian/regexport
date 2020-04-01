


as.regexport.list <- function(model) {
  if(!"depvar" %in% names(model)) stop("The regexport class requires a depvar (dependent variable name)")
  if(!"coef" %in% names(model)) stop("The regexport class requires a data.frame named coef with coefficients")
  if(!"sumstats" %in% names(model)) stop("The regexport class requires a list of summary statistics named sumstats")
  
  if(!"var" %in% names(model$coef)) stop("coef should include a vector with variable names named var")
  if(!"est" %in% names(model$coef)) stop("coef should include a vector with coefficient estimates named est")
  
  class(model) <- "regexport"
  model
}