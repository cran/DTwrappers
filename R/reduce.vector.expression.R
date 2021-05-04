#' Takes a numeric vector and produces a statement with a more compact representation.  For instance, c(1,2,3,4) would become '1:4' and c(1:3, 4:6) could become '1:6'.

#' @param x a numeric vector
#' @return None
reduce.vector.expression <- function(x){
  
  x <- sort(x = x, decreasing = FALSE)
  len <- length(x)
  y <- c(1 + x[1], 1 + x[1:(len-1)])
  
  starting.points <- which(x != y)
  num.starting.points <- length(starting.points)
  
  if(num.starting.points == 1){
    ending.points <- len
  }
  if(num.starting.points > 1){
    ending.points <- c(starting.points[2:num.starting.points] - 1, len)
  }
  formatted.starting.points <- ifelse(test = x[starting.points] < 0, yes = sprintf("(%s)", x[starting.points]), no = as.character(x[starting.points]))
  formatted.ending.points <- ifelse(test = x[ending.points] < 0, yes = sprintf("(%s)", x[ending.points]), no = as.character(x[ending.points]))
    
  the.pieces <- sprintf("%s:%s", formatted.starting.points, formatted.ending.points)
  
  the.pieces[formatted.starting.points == formatted.ending.points] <- formatted.starting.points[formatted.starting.points == formatted.ending.points]
  
  
  num.pieces <- length(the.pieces)
  if(num.pieces == 1){
    res <- sprintf("%s", the.pieces)
  }
  if(num.pieces > 1){
    res <- sprintf("c(%s)", paste(the.pieces, collapse = ", "))
  }
  
  return(res)
}
