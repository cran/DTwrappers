#' dt.sort
#'
#' @description This function sorts the rows of a data.frame or data.table based on selected columns.  It is built as a light wrapper function of data.table's setorderv() function.  Options also exist to return a data.table coding statement (result = "code") for educational purposes or both the result and the code together (result = "all").  For examples, please see the vignette.
#'
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.  A variable called dat should be referred to with dt.name = "dat" when using the function.
#' @param sorting.variables A vector specifying the variables that we want to sort by. For character vectors, only values that exist in the names of the data will be used.  For numeric vectors, only the values of unique(floor(sorting.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be used to select column names from the data.  Other values in sorting.variables that do not correspond to a defined column will be excluded from the calculation.  The sorting proceeds in the order that sorting.variables is specified.
#' @param sort.increasing A logical vector or numeric vector specifying whether the sorting should be increasing (TRUE or 1) or decreasing (FALSE or not 1) for each variable in sorting.variables.  A vector such as c(TRUE, FALSE) would sort the first variable in increasing order and the second in decreasing order.  If only a single value is provided (either TRUE or FALSE), then all of the.variables will be sorted in the specified ordering.
#' @param missing.variables a character value of either "first" or "last" specifying where rows with missing values in the.variables should be included.  Using "first" will place those rows at the beginning of the table, while "last" would place them in the end of the table.
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the updated data.  return.as = "code" provides a data.table coding statement.  return.as = "all" provides a list object including both the resulting output and the code.
#' @param envir a specification of the environment in which the data (referenced by dt.name) exists, with the global environment as the default value.
#' @param ... additional arguments if required
#' @return Depending on the value of return.as, the output will be a) a character value (return.as = 'code'), b) a coding output, typically a data.table (return.as = 'result'), or c) a list containing both the code and output (return.as = 'all')
#' @examples
#' n <- nrow(iris)
#' dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = FALSE),])
#' dt.sort(dt.name = "dat", sorting.variables = c("Species", "Sepal.Length"),
#' sort.increasing = TRUE, return.as = "all")
#' @import data.table
#' @export


dt.sort <- function(dt.name, sorting.variables, sort.increasing = TRUE, missing.variables = c("first", "last"), return.as = "result", envir = .GlobalEnv, ...){

  
  is.format.dt <- check.dt.status(dt.name = dt.name, envir = envir)

  na.last <- "FALSE"

  if(is.character(missing.variables) == TRUE & missing.variables[1] == "last"){
    na.last <- "TRUE"
  }
  sorting.variables <- unique(sorting.variables)

  if(is.numeric(sorting.variables)){
    the.indices <- unique(floor(sorting.variables))
    sorting.variables <- names(x = get(x = dt.name, envir = envir))[the.indices[the.indices %in% 1:ncol(x = get(x = dt.name, envir = envir))]]
  }

  cols.statement <- sprintf("%s", paste(sprintf("'%s'", sorting.variables), collapse = ", "))

  num.sorting.variables <- length(sorting.variables)
  if(length(sorting.variables) > 1){
    cols.statement <- sprintf("c(%s)", cols.statement)
  }

  if(is.numeric(sort.increasing)){
    sort.increasing <- (sort.increasing == 1)
  }
  if(is.logical(sort.increasing) == FALSE){
    sort.increasing <- TRUE
  }

  num.unique.sort.increasing <- length(unique(sort.increasing))
  len.sort.increasing <- length(sort.increasing)

  if(len.sort.increasing < length(sorting.variables) | num.unique.sort.increasing == 1){
    sort.increasing <- sort.increasing[1]
  }

  ordering.statement <- sprintf("%s", paste(sprintf("%s", -1 + 2 * as.numeric(sort.increasing)), collapse = ", "))
  if(length(sort.increasing) > 1){
    ordering.statement <- sprintf("c(%s)", ordering.statement)
  }

  the.statement <- sprintf("data.table::setorderv(x = %s, cols = %s, order = %s, na.last = %s)", dt.name, cols.statement, ordering.statement, na.last)

  res <- eval.dt.statement(the.statement = the.statement, return.as = return.as, envir = envir)

  revise.dt.status(dt.name = dt.name, envir = envir, is.format.dt = is.format.dt)

  return(res)
}
