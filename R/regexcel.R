#' Output a regression to Excel
#' 
#' @param reglist Regression output or a list of regression outputs that can be
#'        coerced into class reglist
#' @param file An openxlsx workbook or the name of a new Excel file to create 
#'        where output should be stored
#' @param sheet The name of the worksheet where results should be saved
#' @param order The order of variables in the coefficient list; any variables 
#'        not in this list will be ordered automatically according to their 
#'        order in the input regression models
#' @param altnames A named character vector giving the alternative names for
#'        input variables; the vector results will replace the names
#' @param regnames An optional vector with names for each regression to place
#'        on the row above the variable names (if NULL, this line will be 
#'        omitted).
#' @param siglevels What are the significance levels for which special 
#'        formatting should be used?
#' @param sigstyles What styles should be used for each of the significance
#'        levels? Defaults to typical stars used in regression tables
#' @param suppress A regular expression or vector of regressors to be 
#'        suppressed from the output (useful for fixed effects)
#' @param note Any notes to include at the bottom. Using "\%stars\%" will 
#'        include the levels used for significance testing if the default
#'        stars are used
#'
#' @details The \code{regexcel} function is used to output regressions to xlsx
#' format for presentation or further analysis in Microsoft Excel or other 
#' software that can read xlsx files. Because many users will wish to continue
#' their analysis in Excel, regexcel produces output that preserves numerical
#' formatting, but still produces the typical stars
#'
#' @examples 
#' data <- data.frame(x = c(2, 4, 5, 6, 8, 10), y = c(1, 1, 2, 2, 3, 3))
#' regression <- lm(y ~ x, data = data)
#' regexcel(regression)
#' @export 

regexcel <- function(reglist, file = "output.xlsx", sheet = "Regression", order = NULL, altnames = NULL, regnames = NULL, 
                     suppress = NULL, digits = 3, sumstats = c("R-squared", "Adj. R-squared", "Observations"),
                     notes = "*** p<0.01, ** p<0.05, * p<0.1", siglevels = c(0.1, 0.05, 0.01), 
                     coefstyles = NULL, sumstyles = NULL, sestyle = NULL, startrow = 1) {
  
  if(is.null(coefstyles)) {
    coefstyles <- list()
    
    for(a in 0:length(siglevels)) {
      coefstyles[[a + 1]] <- openxlsx::createStyle(numFmt = paste0("0.", paste0(paste(rep(0, digits), collapse = "")), 
                                                   paste(rep("\\*", a), collapse = "")), halign = "center")
    }
  }

  if(is.null(sestyle)) sestyle <- openxlsx::createStyle(numFmt = paste0("(0.", paste(rep(0, digits), collapse = ""), ")"), halign = "center")
  
  stylesum <- list(
    openxlsx::createStyle(numFmt = paste0("0.", paste0(paste(rep(0, digits), collapse = ""))), halign = "center"),
    Observations = openxlsx::createStyle(numFmt = "#,##0", halign = "center")
  )
  
  if("Observations" %in% names(sumstyles)) stylesum$Observations <- NULL
  if("" %in% names(sumstyles)) stylesum[[1]] <- NULL
  stylesum <- c(stylesum, sumstyles)
  
  if(typeof(file) == "character") {
    s <- Sys.getenv()
    work <- openxlsx::createWorkbook(creator = s[names(s) %in% c("USER", "USERNAME")])  
  }
  
  if(class(file)[1] == "Workbook") {
    work <- file
  }
  
  if(!sheet %in% names(work)) {
    openxlsx::addWorksheet(work, sheet)
    
    openxlsx::setColWidths(work, sheet, 1, 35)
    openxlsx::setColWidths(work, sheet, 1 + 1:length(reglist), 10)
  } else {
    warning(sprintf("Workbook already contains a sheet called '%s'. Overwriting existing data.", sheet))
  }
  
  if(!class(reglist) %in% c("list", "reglist")) reglist <- list(reglist)
  regs <- as.reglist(reglist)
  
  coeflist <- unique(c(order, unlist(sapply(regs, function(a) return(a$coef$var)))))
  
  if(!"(Intercept)" %in% order & "(Intercept)" %in% coeflist) coeflist <- c(coeflist[!coeflist %in% "(Intercept)"], "(Intercept)")
  if(!is.null(suppress) & length(suppress == 1)) coeflist <- coeflist[!grepl(suppress, coeflist)]
  if(!is.null(suppress) & length(suppress > 1)) coeflist <- coeflist[!coeflist %in% suppress]

  sstats <- lapply(regs, function(a) data.frame(var = names(a$sumstats)[names(a$sumstats) %in% sumstats], 
                                                value = suppressWarnings(as.numeric(unlist(a$sumstats)[names(a$sumstats) %in% sumstats])), 
                                                stringsAsFactors = FALSE))
  
  for(a in 1:length(regs)) {
    if(length(regs[[a]]$sumstats.user) > 0) {
      sstats[[a]] <- rbind(data.frame(var = names(regs[[a]]$sumstats.user), 
                                      value = suppressWarnings(as.numeric(unlist(regs[[a]]$sumstats.user))), 
                                      stringsAsFactors = FALSE), sstats[[a]])
      
      regs[[a]]$sumstats <- c(regs[[a]]$sumstats, regs[[a]]$sumstats.user)
    } else {
      
    }
  }
  
  uss <- lapply(sstats, "[[", "var")
  uss <- unique(unlist(uss))
  
  output <- data.frame(coef = c(rep(coeflist, each = 2), uss), stringsAsFactors = FALSE)
  output$coef[duplicated(output$coef)] <- paste0(output$coef[duplicated(output$coef)], "!SE")
  row.names(output) <- output$coef
  output$coef <- NULL
  
  for(a in 1:length(regs)) {
    output[[a]] <- NA
    
    for(b in 1:nrow(regs[[a]]$coef)) {
      if(regs[[a]]$coef$var[b] %in% coeflist) {
        output[[a]][row.names(output) == regs[[a]]$coef$var[b]] <- regs[[a]]$coef$est[b]
        output[[a]][row.names(output) == paste0(regs[[a]]$coef$var[b], "!SE")] <- regs[[a]]$coef$se[b]
      }
    }
    
    for(b in 1:nrow(sstats[[a]])) {
      output[[a]][row.names(output) == sstats[[a]]$var[b]] <- sstats[[a]]$value[b]
    }
  }
  
  names(output) <- sapply(regs, "[[", "depvar")
  
  for(a in names(altnames)) {
    names(output) <- sub(a, altnames[a], names(output), perl = TRUE)
    row.names(output) <- sub(a, altnames[a], row.names(output), perl = TRUE)
  }
  
  if(length(coefstyles) != length(siglevels) + 1) stop("The number of sigstyles must be the number of siglevels (breaks) plus 1")

  if(is.null(regnames)) {
    sr <- 2 + startrow
  } else {
    openxlsx::writeData(work, sheet, as.data.frame(as.list(regnames)), 2, 2 + startrow, colNames = FALSE)
    openxlsx::addStyle(work, sheet, openxlsx::createStyle(halign = "center"), 2 + startrow, 1:length(regnames) + 1)
    sr <- 3 + startrow
  }
  
  openxlsx::writeData(work, sheet, output, startRow = sr, startCol = 1, headerStyle = openxlsx::createStyle(border = "bottom", halign = "center"), 
                      rowNames = TRUE)
  
  for(a in grep("!SE", row.names(output))) {
    openxlsx::mergeCells(work, sheet, 1, (sr + a) + -1:0)  
  }
  
  openxlsx::addStyle(work, sheet, openxlsx::createStyle(valign = "top"), (sr - 1):(sr + nrow(output)), 1)
  
  nrs <- as.data.frame(as.list(1:length(output)))
  openxlsx::writeData(work, sheet, nrs, 2, startrow + 1, colNames = FALSE)
  openxlsx::addStyle(work, sheet, openxlsx::createStyle(wrapText = TRUE, numFmt = "(0)", halign = "center", 
                                                        valign = "bottom", border = "top"), startrow + 1, 1:length(output) + 1)
  
  for(a in 1:length(output)) {
    co <- regs[[a]]$coef
    
    for(b in 0:length(siglevels)) {
      mini <- siglevels[b + 1]
      maxi <- siglevels[b]
      
      if(is.na(mini)) mini <- 0
      if(length(maxi) == 0) maxi <- 1
      
      tofix <- co$var[co$p <= maxi & co$p >= mini]
      
      if(length(tofix) > 0) {
        openxlsx::addStyle(work, sheet, coefstyles[[b + 1]], which(row.names(output) %in% tofix) + sr, a + 1)
      }
    }
    
    openxlsx::addStyle(work, sheet, sestyle, grep("!SE", row.names(output)) + sr, a + 1)
  }
  
  for(a in 1:length(stylesum)) {
    if(a == 1) names(stylesum)[a]
    
    i <- which(row.names(output) %in% names(stylesum)[a])
    if(names(stylesum)[a] == "") i <- which(row.names(output) %in% uss)
    
    openxlsx::addStyle(work, sheet, stylesum[[a]], i + sr, 1:length(output) + 1, gridExpand = TRUE)
  }
  
  openxlsx::addStyle(work, sheet, openxlsx::createStyle(border = "bottom"), 
                     max(grep("!SE", row.names(output))) + sr, 1:length(output) + 1,
                     stack = TRUE)
  
  openxlsx::addStyle(work, sheet, openxlsx::createStyle(border = "bottom"), 
                     nrow(output) + sr, 1:length(output) + 1,
                     stack = TRUE)
  
  if(typeof(file) == "character") {
    openxlsx::saveWorkbook(work, file, overwrite = TRUE)
  }
  
  a <- file
}