#' dt.first.k.rows
#'
#' @description This function returns the first k rows from the given data.  It is built as a wrapper function of data.table's filter (the i step).  This calculation can be specified either overall or in groups.  A filter can also be applied so that only qualifying values would be considered.  A subset of the variables may also be selected.  Options also exist to return a data.table coding statement (result = "code") for educational purposes or both the result and the code together (result = "all").  For examples, please see the vignette.
#'
#'#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.
#' @param the.filter a character value, logical vector, or expression stating the logical operations used to filter the data.  See create.filter.expression for details.  The filtering step will be applied prior to generating the counts.  Defaults to NULL unless otherwise specified.
#' @param grouping.variables  a character vector specifying the variables to group by in the calculation.  Only variables in the data will be used.  When NULL, no grouping will be performed.
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.  A variable called dat should be referred to with dt.name = "dat" when using the function.
#' @param the.variables A character or numeric vector specifying the variables to perform the calculations on. For character vectors, only values that exist in the names of the data will be used.  For numeric vectors, only the values of unique(floor(sorting.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be used to select column names from the data.  Other values in sorting.variables that do not correspond to a defined column will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#' @param sortby.group  A logical value specifying whether the grouping should be sorted (TRUE, the default value) or as is (FALSE).
#' @param k A numeric variable specifying the number of rows to select
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the resulting table.  return.as = "code" provides a data.table coding statement that can generate the resulting table.  return.as = "all" provides both the resulting table and the code.  If the coding statement was specified using calls to get() or eval(), then both an original.statement and the resulting code (a translated statement from the getDTeval package) will be provided.
#' @param envir the environment in which the code would be evaluated; .GlobalEnv by default.
#' @param ... additional arguments to be passed
#' @return Depending on the value of return.as, the output will be a) a character value (return.as = 'code'), b) a coding output, typically a data.table (return.as = 'result'), or c) a list containing both the code and output (return.as = 'all')
#' @note  Calls dt.choose.cols.R with first.k = k.
#'
#' 
#' @export
#'
#' @examples
#' n <- nrow(iris)
#' dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = FALSE),])
#' dt.first.k.rows(dt.name = "dat", k = 2, the.variables = c("Sepal.Length", "Sepal.Width"),
#' grouping.variables = "Species", return.as = "all")
#'
#'
#' @source DTwrappers::dt.choose.cols
#' @export
dt.first.k.rows <-
  function(dt.name,
           k = NULL,
           the.variables = ".",
           the.filter = NULL,
           grouping.variables = NULL,
           sortby.group = TRUE,
           return.as = "result",
           envir = .GlobalEnv,
           ...) {
    return(
      dt.choose.cols(
        dt.name = dt.name,
        the.variables = the.variables,
        the.filter = the.filter,
        grouping.variables = grouping.variables,
        sortby.group = sortby.group,
        first.k = k, return.as = return.as, envir = envir,
        ...
      )
    )
  }



#' dt.last.k.rows
#'
#' @description This function returns the last k rows from the given data.  It is built as a wrapper function of data.table's filter (the i step).  This calculation can be specified either overall or in groups.  A filter can also be applied so that only qualifying values would be considered.  A subset of the variables may also be selected.  Options also exist to return a data.table coding statement (result = "code") for educational purposes or both the result and the code together (result = "all").  For examples, please see the vignette.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.
#' @param the.filter a character value, logical vector, or expression stating the logical operations used to filter the data.  See create.filter.expression for details.  The filtering step will be applied prior to generating the counts.  Defaults to NULL unless otherwise specified.
#' @param the.variables A character or numeric vector specifying the variables to perform the calculations on. For character vectors, only values that exist in the names of the data will be used.  For numeric vectors, only the values of unique(floor(sorting.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be used to select column names from the data.  Other values in sorting.variables that do not correspond to a defined column will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#' @param sortby.group  A logical value specifying whether the grouping should be sorted (TRUE, the default value) or as is (FALSE).
#' @param k A numeric variable specifying the number of rows to select
#' @param grouping.variables  a character vector specifying the variables to group by in the calculation.  Only variables in the data will be used.  When NULL, no grouping will be performed.
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the resulting table.  return.as = "code" provides a data.table coding statement that can generate the resulting table.  return.as = "all" provides both the resulting table and the code.  If the coding statement was specified using calls to get() or eval(), then both an original.statement and the resulting code (a translated statement from the getDTeval package) will be provided.
#' @param envir the environment in which the code would be evaluated; .GlobalEnv by default.
#' @param  ... additional arguments to be passed
#' @return Depending on the value of return.as, the output will be a) a character value (return.as = 'code'), b) a coding output, typically a data.table (return.as = 'result'), or c) a list containing both the code and output (return.as = 'all')
#' @note  Calls dt.choose.cols.R with last.k = k.
#' 
#' @export
#' @examples
#'
#' n <- nrow(iris)
#' dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = FALSE),])
#'
#' dt.last.k.rows(dt.name = "dat", k = 2, the.variables = c("Sepal.Width"),
#' grouping.variables = "Species", return.as = "all")

#'
#' @source DTwrappers::dt.choose.cols
#' @export
dt.last.k.rows <-
  function(dt.name,
           k = NULL,
           the.variables = ".",
           the.filter = NULL,
           grouping.variables = NULL,
           sortby.group = TRUE,
           return.as = "result",
           envir = .GlobalEnv,
           ...) {
    return(
      dt.choose.cols(
        dt.name = dt.name,
        the.variables = the.variables,
        the.filter = the.filter,
        grouping.variables = grouping.variables,
        sortby.group = sortby.group,
        last.k = k, return.as = return.as, envir = envir,
        ...)
    )
  }
