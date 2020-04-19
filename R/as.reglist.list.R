#' @export


as.reglist.list <- function(regs) {
  m <- sapply(regs, function(a) any(c("list", "reglist") %in% class(a)))
  
  while(any(TRUE %in% m)) {
    if(length(m) == 1) {
      regs <- regs[[1]]
      m <- sapply(regs, function(a) any(c("list", "reglist") %in% class(a)))
      next
    }
    
    a <- which(m)[1]
    
    if(a == 1 & a < length(m)) {
      regs <- c(regs[[1]], regs[2:length(regs)])
      m <- sapply(regs, function(a) any(c("list", "reglist") %in% class(a)))
      next
    }
    if(a > 1 & a < length(m)) {
      regs <- c(regs[2:a - 1], regs[[a]], regs[(a + 1):length(regs)])
      m <- sapply(regs, function(a) any(c("list", "reglist") %in% class(a)))
      next
    }
    if(a > 1 & a == length(m)) {
      regs <- c(regs[2:a - 1], regs[[a]])
      m <- sapply(regs, function(a) any(c("list", "reglist") %in% class(a)))
    }
  }
  
  out <- lapply(regs, function(a) as.regexport(a))
  class(out) <- "reglist"
  return(out)
}
