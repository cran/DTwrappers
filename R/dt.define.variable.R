#' dt.define.variable
#'
#' @description This method allows a user to add a new variable to an existing data.frame or data.table.  It can also be used to update previously defined variables.  It is built as a wrapper function of data.table's method of defining new variables by reference.  The new values can be stated either through a statement of the calculation or by directly providing a vector of values.  These updates can also be performed on a subset of the data by incorporating a filter.  Options also exist to return a data.table coding statement (result = "code") for educational purposes or both the result and the code together (result = "all").  For examples, please see the vignette.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.  A variable called dat should be referred to with dt.name = "dat" when using the function.
#' @param variable.name  a character value specifying the name of the new column.
#' @param the.values a vector or character value.  When specified as a vector, this should contain the values of the new column.  When specified as a character value, it should include a functional form that specifies how to calculate the new values.  See the specification parameter for more details.
#' @param specification A character value.  When specification = "by.value", the new variable will be defined in terms of the vector the.values.  Otherwise the new variable is specified in a functional form, e.g. the.values = "rnorm(n = 3)".
#' @param the.filter a character value, logical vector, or expression stating the logical operations used to filter the data.  See create.filter.expression for details.  The filtering step will be applied prior to generating the counts.  Defaults to NULL unless otherwise specified.
#' @param grouping.variables  A character or numeric vector specifying the variables to perform the calculations on. For character vectors, the values may be either column names of the data or calculations based upon them (see the vignette for examples).  For numeric vectors, only the values of unique(floor(grouping.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be mapped to the corresponding column names from the data.  When NULL, no grouping will be performed.
#' @param sortby.group  A logical value specifying whether the grouping should be sorted (TRUE, the default value) or as is (FALSE).
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the table of counts.  return.as = "code" provides a data.table coding statement that can generate the table of counts.  return.as = "all" provides both the resulting table and the code.
#' @param envir the environment in which the code would be evaluated; .GlobalEnv by default.
#' @param ... other additional arguments if needed
#' @return Depending on the value of return.as, the output will be a) a character value (return.as = 'code'), b) a coding output, typically a data.table (return.as = 'result'), or c) a list containing both the code and output (return.as = 'all')
#' @note the data.frame dat will be converted to a data.table object to facilitate adding the new column by reference (e.g. efficiently with regard to the usage of memory)
#'
#' @export

#' @import data.table
#' @source DTwrappers::create.dt.statement
#' @source DTwrappers::eval.dt.statement
#' @export
dt.define.variable <-
  function(dt.name,
           variable.name,
           the.values,
           specification = "by.expression",
           the.filter = NULL,
           grouping.variables = NULL,
           sortby.group = TRUE,
           return.as = "result",
           envir = .GlobalEnv, ...) {
  

  is.format.dt <- check.dt.status(dt.name = dt.name, envir = envir)

  all.variable.names <- names(get(x = dt.name, envir = envir))
  if(is.numeric(x = grouping.variables) == TRUE){
    grouping.indices <- unique(floor(grouping.variables))
    grouping.variables <- all.variable.names[grouping.indices]
  }

  grouping.variables <- unique(grouping.variables)

  lhs <- add.backtick(x = variable.name, include.backtick = "as.needed")
  if(specification == "by.value"){
    num.values <- length(the.values)
    quotation.classes <- is.character(the.values) | is.factor(the.values)
    if(quotation.classes == TRUE){
      pasted.values <- paste(sprintf("'%s'", the.values), collapse = ", ")
    }
    if(quotation.classes == FALSE){
      pasted.values <- paste(the.values, collapse = ", ")
    }
    if(num.values == 1){
      rhs <- pasted.values
    }
    if(num.values > 1){
      rhs <- sprintf("c(%s)", pasted.values)
    }
  }
  if(specification != "by.value"){
    rhs <- the.values
  }
  j.statement <- j.statement <- sprintf("%s := %s", lhs, rhs)

  i.statement <- create.i.statement(dt.name = dt.name, the.filter = the.filter)

  the.statement <- create.dt.statement(dt.name = dt.name, i.statement = i.statement, j.statement = j.statement, grouping.variables = grouping.variables, sortby.group = sortby.group)

  res <- eval.dt.statement(the.statement = the.statement, return.as = return.as, envir = envir)

  revise.dt.status(dt.name = dt.name, envir = envir, is.format.dt = is.format.dt)

  return(res)
}

