#' @rdname print.regexport
#' @export

print.reglist <- function(reglist, order = NULL, altnames = c(`^\\(Intercept\\)$` = "Constant"), suppress = NULL,
                          digits = 3, sumstats = c("R-squared", "Adj. R-squared", "Observations"), 
                          notes = "*** p<0.01, ** p<0.05, * p<0.1", siglevels = c(0.1, 0.05, 0.01),
                          sigformats = paste0("%1.", digits, "f", c("   ", "*  ", "** ", "***"))) {
  
  print(as.data.frame(reglist, order = order, altnames = altnames, suppress = suppress, digits = digits,
                      sumstats = sumstats, siglevels = siglevels, sigformats = sigformats))
  
  if(!is.null(notes)) cat(paste0("\n", notes))
  output <- reglist
}
