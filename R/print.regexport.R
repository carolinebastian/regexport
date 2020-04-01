#' Print methods for regexport and reglist
#' 
#' @param reg A regression model 
#' @param order A list of variable names to move to the top of the output table
#'        in the order specified
#' @param alt.names A named character vector giving the alternative names for
#'        input variables; the vector results will replace the names
#' @param suppress A regular expression or vector of regressors to be 
#'        suppressed from the output (useful for fixed effects)
#' @param digits The number of digits to use for rounding
#' @param sumstats Which summary statistics to output in the table
#' @param notes A character with notes to be shown at the bottom of the output 
#' @param siglevels A numeric vector with significance levels below which 
#'        special formatting will be used (typically stars); levels should be
#'        ordered from least to most significant (i.e. largest to smallest)
#' @param sigformats A character vector with \code{sprintf} formats to use when
#'        outputting the data; the first element should be the output format 
#'        for coefficients that are not significant at any level, followed by
#'        the formats for those significant at each level in siglevels
#'
#' @details These methods produce nicely formatted console output for objects
#' of class regexport or reglist. The output captured here is nice enough for
#' many purposes, such as quick sharing in e-mail. For more professional
#' outputs, the other functions in this package will be more suitable.
#' 
#' \code{as.data.frame.reglist} is at the core of these methods; converting a
#' \code{reglist} object to a data frame will yield a data frame which will
#' look essentially the same when printed, except without the notes. This
#' could be useful for output to a csv file or other purposes.
#'
#' @examples 
#' test <- list(lm(mpg~cyl+disp, data = mtcars),
#'              lm(mpg~cyl+disp+hp, data = mtcars),
#'              lm(mpg~cyl+disp+hp+drat, data = mtcars),
#'              lm(mpg~hp+drat+wt, data = mtcars))
#' 
#' test <- regexport(test)
#' print(test) # An object of class reglist
#' 
#' test <- test[[1]]
#' 
#' print(test) # An object of class regexport
#' 
#' @export 

print.regexport <- function(reg, order = NULL, altnames = c(`^\\(Intercept\\)$` = "Constant"), suppress = NULL,
                            digits = 3, sumstats = c("R-squared", "Adj. R-squared", "Observations"), 
                            notes = "*** p<0.01, ** p<0.05, * p<0.1", siglevels = c(0.1, 0.05, 0.01),
                            sigformats = paste0("%1.", digits, "f", c("   ", "*  ", "** ", "***"))) {
  
  print(as.data.frame(as.reglist(reg), order = order, altnames = altnames, suppress = suppress, digits = digits,
                      sumstats = sumstats, siglevels = siglevels, sigformats = sigformats))
  
  if(!is.null(notes)) cat(paste0("\n", notes))
  
  output <- reg
}
