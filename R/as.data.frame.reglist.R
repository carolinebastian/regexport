#' @export

as.data.frame.reglist <- function(reglist, order = NULL, altnames = c(`^\\(Intercept\\)$` = "Constant"), suppress = NULL,
                                  digits = 3, sumstats = c("R-squared", "Adj. R-squared", "Observations"), siglevels = c(0.1, 0.05, 0.01), 
                                  sigformats = paste0("%1.", digits, "f", c("   ", "*  ", "** ", "***"))) {
  regs <- reglist
  
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
  
  output <- data.frame(coef = c(rep(coeflist, each = 2), " ", uss), stringsAsFactors = FALSE)
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
  
  if(length(sigformats) != length(siglevels) + 1) stop("The number of sigformats must be the number of siglevels (breaks) plus 1")
  
  for(a in 1:length(regs)) {
    strver <- output[[a]][row.names(output) %in% regs[[a]]$coef$var]
    
    for(b in 0:length(siglevels)) {
      mini <- siglevels[b + 1]
      maxi <- siglevels[b]
      
      if(is.na(mini)) mini <- 0
      if(length(maxi) == 0) maxi <- 1
      
      whichrows <- sapply(row.names(output)[row.names(output) %in% regs[[a]]$coef$var], 
                          function(b) which(regs[[a]]$coef$var == b))
      
      strver <- ifelse(regs[[a]]$coef$p[whichrows] >= mini & regs[[a]]$coef$p[whichrows] <= maxi, 
                       sprintf(sigformats[b + 1], output[[a]][row.names(output) %in% regs[[a]]$coef$var]), strver)
    }
    
    spaces <- nchar(sigformats[1]) - nchar(sub(" +$", "", sigformats[1])) 
    
    sv <- output[[a]]
    sv[row.names(output) %in% regs[[a]]$coef$var] <- strver
    sv[grepl("!SE", row.names(output))] <- sprintf(paste0("(%1.", digits, "f)%s"), 
                                                   output[grep("!SE", row.names(output)), a], 
                                                   paste(rep(" ", spaces - 1), collapse = ""))
    
    for(b in uss) {
      resp <- c()
      
      for(c in 1:length(regs[[a]]$sumstats[[b]])) {
        if(length(regs[[a]]$sumstats[[b]]) == 0 | all(is.na(regs[[a]]$sumstats[[b]]))) {
          resp <- c(resp, "-")
          next
        }
        
        respa <- regs[[a]]$sumstats[[b]][[c]]
        
        if(class(respa) == "integer") {
          resp <- c(resp, formatC(respa, big.mark = ","))
          next
        }
        
        if(class(respa) == "numeric") {
          resp <- c(resp, sprintf(paste0("%1.", digits, "f"), respa))
          next
        }
        
        resp <- c(resp, as.character(respa))
      }
      
      resp <- paste(resp, collapse = ", ")
      resp <- paste0(resp, paste(rep(" ", spaces), collapse = ""))
      sv[row.names(output) == b] <- resp
    }
    
    sv[is.na(sv) | grepl("\\(NA\\)", sv)] <- ""
    output[[a]] <- sv
  }

  for(a in grep("!SE", row.names(output))) row.names(output)[a] <- paste(rep("\u200B", a), collapse = "")
  
  rn <- sapply(regs, "[[", "depvar")
  names(output) <- paste0(rn, paste(rep(" ", spaces), collapse = ""))
  
  for(a in names(altnames)) {
    names(output) <- sub(a, altnames[a], names(output), perl = TRUE)
    row.names(output) <- sub(a, altnames[a], row.names(output), perl = TRUE)
  }
  
  return(output)
}
