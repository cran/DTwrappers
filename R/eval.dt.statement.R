# description:  Evaluates data.table coding statements.  The user may select whether the results of the calculation, the code, or both items are returned.  When the coding statement includes programatic designs with the get() or eval() function, an option is provided to optimize the performance of the code through efficient translations using getDTeval().

# the.statement:  a character value or an expression of a data.table coding statement.  For example, a valid character input would be the.statement = "dt[Age < 50, mean(x = Satisfaction, na.rm = TRUE), keyby = "Region"]" as a parameter.  A character input will be parsed into an expression, or the.statement can be directly specified as an expression.  Alternatively, such a statement can be entered as an expression.  For example, use expression(dt[Age < 50, mean(x = Satisfaction, na.rm = TRUE), keyby = "Region"])

# return.as:  a character value stating what should be returned.  When return.as = "result", the calculation is evaluated.  When return.as = "code", then the translated coding statement is provided.  When return.as = "all", then a list is returned that includes both the result and the code.  If the code was translated during evaluation, then both the code and the original.statement are returned.

# eval.type:  a character value stating whether the coding statement should be evaluated in its current form (eval.type = "as.is") or have its called to get() and eval() translated (eval.type = "optimized", the default setting).

# envir:  The environment in which the calculation takes place, with the global environment .GlobalEnv set as the default.

# Note: This function directly evaluates a data.table coding statement.  To construct such a statement, see create.dt.statement.

eval.dt.statement <- function(the.statement, return.as = "result", envir = .GlobalEnv, ...){
  
  value.code <- "code"
  value.all <- "all"
  
  if(return.as == value.code){
    res <- the.statement
  }
  if(return.as != value.code){
    the.result <- eval(expr = parse(text = the.statement), envir = envir, ...) []
    if(return.as == value.all){
      res <- list(result = the.result, code = the.statement)
    }
    if(return.as != value.all){
      res <- the.result
    }
  }
  
  return(res)
}
