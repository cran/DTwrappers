#' dt.count.rows
#'
#' @description This function counts the number of qualifying rows in a data.table or data.frame object.  It is built as a wrapper function of data.table's filter (the i step).  These counts may be comprehensive for the entire table or conducted in groups.  The full data can also be filtered for qualifying cases prior to conducting the counts.  This function returns a data.table object that shows the counts in one column along with additional columns for any specified grouping variables.  Options also exist to return a data.table coding statement (result = "code") for educational purposes or both the result and the code together (result = "all").  For examples, please see the vignette.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.  A variable called dat should be referred to with dt.name = "dat" when using the function.
#' @param the.filter  a character value, numeric vector, logical vector, or expression stating the logical operations used to filter the data.  The filtering step will be applied prior to generating the counts.  Defaults to NULL unless otherwise specified.  Character values such as 'Age < 50' or 'c(1:3, 7:10)' may be used.  Numeric vectors such as c(1:3, 7:10) that specify the row indices may be used.  Logical vectors will be converted to a numeric filter, e.g. c(TRUE, TRUE, FALSE) will become 1:2 to signify which rows should be selected.  Expressions may be used to specify a logical operation such as expression(Age < 50) as well.  Defaults to NULL to indicate that no filtering of the data should be applied.
#' @param grouping.variables  A character or numeric vector specifying the variables to perform the calculations on. For character vectors, the values may be either column names of the data or calculations based upon them (see the vignette for examples).  For numeric vectors, only the values of unique(floor(grouping.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be mapped to the corresponding column names from the data.  When NULL, no grouping will be performed.
#' @param sortby.group  a character value specifying whether the table of counts should be sorted by group ("sorted") or as is (any other selected value).  Defaults to "sorted".
#' @param count.name  a character value specifying the name of the column of counts in the resulting table.  This value defaults to "N" unless otherwise specified.
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the table of counts.  return.as = "code" provides a data.table coding statement that can generate the table of counts.  return.as = "all" provides both the resulting table and the code.  If the coding statement was specified using calls to get() or eval(), then both an original.statement and the resulting code (a translated statement from the getDTeval package) will be provided.
#' @param envir the environment in which the code would be evaluated; .GlobalEnv by default.
#' @return Depending on the value of return.as, the output will be a) a character value (return.as = 'code'), b) a coding output, typically a data.table (return.as = 'result'), or c) a list containing both the code and output (return.as = 'all')
#' @note the data.frame dat will be converted to a data.table object to facilitate efficient selection.
#'
#' @export
#' @examples
#' n <- nrow(iris)
#' dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = FALSE),])
#' dt.count.rows(dt.name = "dat", return.as = "all")
#'
#' @source DTwrappers::create.dt.statement
#' @source DTwrappers::eval.dt.statement
#' @import data.table
#' @export
dt.count.rows <-
  function(dt.name,
           the.filter = NULL,
           grouping.variables = NULL,
           sortby.group = TRUE,
           count.name = "N",
           return.as = "result",
           envir = .GlobalEnv){
  

  is.format.dt <- check.dt.status(dt.name = dt.name, envir = envir)

  all.variable.names <- names(get(x = dt.name, envir = envir))
  if(is.numeric(x = grouping.variables) == TRUE){
    grouping.indices <- unique(floor(grouping.variables))
    grouping.variables <- all.variable.names[grouping.indices]
  }

  grouping.variables <- unique(grouping.variables)

  i.statement <- create.i.statement(dt.name = dt.name, the.filter = the.filter)

  if(count.name == "N"){
    j.statement <- ".N"
  }
  if(count.name != "N"){
    j.statement <- sprintf(".(%s = .N)", add.backtick(x = count.name))
  }

  the.statement <- create.dt.statement(dt.name = dt.name, i.statement = i.statement, j.statement = j.statement, grouping.variables = grouping.variables, sortby.group = sortby.group)

  res <- eval.dt.statement(the.statement = the.statement, return.as = return.as, envir = .GlobalEnv)

  revise.dt.status(dt.name = dt.name, envir = envir, is.format.dt = is.format.dt)

  return(res)
}

#' dt.choose.rows
#'
#' @description This function filters the rows of a data.table or data.frame object.  It is built as a wrapper function of data.table's filtering method (the i step).  A series of logical tests on variables within the data may be specified.  Options also exist to return a data.table coding statement (result = "code") for educational purposes or both the result and the code together (result = "all").  For examples, please see the vignette.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.  A variable called dat should be referred to with dt.name = "dat" when using the function.
#' @param the.filter  a character value, numeric vector, logical vector, or expression stating the logical operations used to filter the data.  The filtering step will be applied prior to generating the counts.  Defaults to NULL unless otherwise specified.  Character values such as 'Age < 50' or 'c(1:3, 7:10)' may be used.  Numeric vectors such as c(1:3, 7:10) that specify the row indices may be used.  Logical vectors will be converted to a numeric filter, e.g. c(TRUE, TRUE, FALSE) will become 1:2 to signify which rows should be selected.  Expressions may be used to specify a logical operation such as expression(Age < 50) as well.  Defaults to NULL to indicate that no filtering of the data should be applied.
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the table of counts.  return.as = "code" provides a data.table coding statement that can generate the table of counts.  return.as = "all" provides both the resulting table and the code.  If the coding statement was specified using calls to get() or eval(), then both an original.statement and the resulting code (a translated statement from the getDTeval package) will be provided.
#' @param envir the environment in which the code would be evaluated; .GlobalEnv by default.
#' @note the data.frame dat will be converted to a data.table object to facilitate efficient counting of the rows.
#' @return Depending on the value of return.as, the output will be a) a character value (return.as = 'code'), b) a coding output, typically a data.table (return.as = 'result'), or c) a list containing both the code and output (return.as = 'all')
#'
#' @export
#' @examples
#' n <- nrow(iris)
#' dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = FALSE),])
#' dt.count.rows(dt.name = "dat", count.name = "Total Rows", return.as = "all")

#' @source DTwrappers::create.dt.statement
#' @source DTwrappers::eval.dt.statement
#' @import data.table
#' @export
dt.choose.rows <- function(
  dt.name,
  the.filter = NULL,
  return.as = "result",
  envir = .GlobalEnv) {


  is.format.dt <- check.dt.status(dt.name = dt.name, envir = envir)

  i.statement <- create.i.statement(dt.name = dt.name, the.filter = the.filter)

  the.statement <- create.dt.statement(dt.name = dt.name, i.statement = i.statement)

  res <- eval.dt.statement(the.statement = the.statement, return.as = return.as, envir = .GlobalEnv)

  revise.dt.status(dt.name = dt.name, envir = envir, is.format.dt = is.format.dt)

  return(res)
}

#' dt.choose.cols
#'
#' @description This function selects columns from a data.frame or data.table.  It is built as a wrapper function of data.table's selection step (using .SD in the j step while specifying the .SDcols argument).  Selections may also be supplied to limit the rows to consider, with options for the first or last k rows or a subset based upon a vector like c(3:5, 9:10).  Filtering of the rows (e.g. Age < 50) may also be applied using the.filter.  Grouped operations may be used to make these selections of columns and rows in each category.  Options also exist to return a data.table coding statement (result = "code") for educational purposes or both the result and the code together (result = "all").  For examples, please see the vignette.
#'
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.  A variable called dat should be referred to with dt.name = "dat" when using the function.
#' @param the.variables A character or numeric vector specifying the variables that we want to select.  For character vectors, only values that exist in the names of the data will be used.  For numeric vectors, only the values of unique(floor(sorting.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be used to select column names from the data.  Only values that exist in the names of the data will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all of the variables will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#' @param the.filter  a character value, numeric vector, logical vector, or expression stating the logical operations used to filter the data.  The filtering step will be applied prior to generating the counts.  Defaults to NULL unless otherwise specified.  Character values such as 'Age < 50' or 'c(1:3, 7:10)' may be used.  Numeric vectors such as c(1:3, 7:10) that specify the row indices may be used.  Logical vectors will be converted to a numeric filter, e.g. c(TRUE, TRUE, FALSE) will become 1:2 to signify which rows should be selected.  Expressions may be used to specify a logical operation such as expression(Age < 50) as well.  Defaults to NULL to indicate that no filtering of the data should be applied.
#' @param grouping.variables  A character or numeric vector specifying the variables to perform the calculations on. For character vectors, the values may be either column names of the data or calculations based upon them (see the vignette for examples).  For numeric vectors, only the values of unique(floor(grouping.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be mapped to the corresponding column names from the data.  When NULL, no grouping will be performed.
#' @param sortby.group  A character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.
#' @param first.k An integer indicating how many rows to select starting from the first row.  Note that grouping statements will select up to this number of rows in each group.  Additionally, if first.k is larger than the number of records in a group, then the maximum number of records will be selected.  When non-integer or non-positive values of first.k are selected, the algorithm will select first.k = max(c(1, round(first.k))).  If first.k is not a numeric or integer value, then by default first.k is set to select all of the rows.  Specifying row.indices takes precedence to specifying the parameter first.k; if row.indices is not NULL, then row.indices will be used, and first.k will not. Meanwhile, first.k takes precedence to last.k when both are specified.  See below.
#' @param last.k An integer indicating how many rows to select starting from the last row.  Note that grouping statements will select up to this number of rows in each group.  Additionally, if last.k is larger than the number of records in a group, then the maximum number of records will be selected.  When non-integer or non-positive values of last.k are selected, the algorithm will select last.k = max(c(1, round(last.k))).  If last.k is not a numeric or integer value, then by default last.k is set to select all of the rows.  Specifying row.indices takes precedence to specifying the parameter last.k (see below); if row.indices is not NULL, then it will be used, and last.k will not.  Meanwhile, first.k takes precedence to last.k when both are specified.
#' @param row.indices An integer vector specifying the row indices to return.  When grouping.variables is specified, these indices will be applied to each group.  Note that specifications outside of the range from 1 to the number of rows will be limited to existing rows from the data and group.  Specifying row.indices takes precedence to specifying the parameters first.k and last.k.  If row.indices is not NULL, it will be used.
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the table of counts.  return.as = "code" provides a data.table coding statement that can generate the table of counts.  return.as = "all" provides a list containing both the resulting table and the code.
#' @param envir the environment in which the code would be evaluated; .GlobalEnv by default.
#' @return Depending on the value of return.as, the output will be a) a character value (return.as = 'code'), b) a coding output, typically a data.table (return.as = 'result'), or c) a list containing both the code and output (return.as = 'all')
#' 
#'
#' @export
#'
#' @import data.table
#' @source DTwrappers::create.dt.statement
#' @source DTwrappers::eval.dt.statement
#' @export
dt.choose.cols <-
  function(dt.name,
           the.variables = ".",
           the.filter = NULL,
           grouping.variables = NULL,
           sortby.group = TRUE,
           first.k = NULL,
           last.k = NULL,
           row.indices = NULL,
           return.as = "result",
           envir = .GlobalEnv) {

  

  is.format.dt <- check.dt.status(dt.name = dt.name, envir = envir)
  .N <- NULL

  all.variable.names <- names(x = get(dt.name))

  if(is.numeric(x = grouping.variables) == TRUE){
    grouping.indices <- unique(floor(grouping.variables))
    grouping.variables <- all.variable.names[grouping.indices]
  }

  grouping.variables <- unique(grouping.variables)

  if ("." %in% the.variables) {
    the.variables <- all.variable.names
  }

  if(is.numeric(the.variables) == TRUE){
    the.indices <- unique(floor(the.variables))

    the.variables <- all.variable.names[the.indices[the.indices %in% 1:ncol(x = get(x = dt.name, envir = envir))]]
  }

  the.variables <-
    unique(the.variables[the.variables %in% all.variable.names & !(the.variables %in% grouping.variables)])

  pasted.variables <- sprintf("c(%s)", paste(sprintf("'%s'", the.variables), collapse = ", "))

  SD.statement <- sprintf(", .SDcols = %s", pasted.variables)

  if(length(the.variables) == length(all.variable.names) & mean(the.variables %in% all.variable.names) == 1){
    SD.statement <- ""
  }


  specified.first.k <- !is.null(first.k)
  specified.last.k <- !is.null(last.k)
  specified.row.indices <- !is.null(row.indices)

  if(specified.row.indices == TRUE){
    printed.row.indices <- reduce.vector.expression(x = row.indices)
    j.statement <-
      sprintf(".SD[(%s)[(%s) %%in%% 1:min(.N, %s[, .N])]]%s", printed.row.indices, printed.row.indices, dt.name, SD.statement)
  }
  if (specified.row.indices == FALSE) {
    if (specified.first.k == TRUE & specified.last.k == FALSE) {
      if (!is.numeric(first.k) & !is.integer(first.k)) {
        first.k <- eval(dt.name)[, .N]
      }

      first.k <- max(c(1, round(first.k)))

      j.statement <-
        sprintf(".SD[1:min(.N, %d)]%s", first.k, SD.statement)
    }
    if (specified.first.k == FALSE & specified.last.k == TRUE) {
      if (!is.numeric(last.k) & !is.integer(last.k)) {
        last.k <- eval(dt.name)[, .N]
      }

      last.k <- max(c(1, round(last.k)))

      if(last.k > 1){
        value.to.subtract <- sprintf(" - %d", last.k - 1)
      }
      if(last.k == 1){
        value.to.subtract <- ""
      }

      j.statement <-
        sprintf(".SD[max(1, .N%s):.N]%s", value.to.subtract, SD.statement)
    }
    if (specified.first.k == FALSE & specified.last.k == FALSE) {

      SD.printed <- ".SD"
      if(length(the.variables) == length(all.variable.names) & mean(the.variables %in% all.variable.names) == 1){
        SD.printed <- ""
      }
      j.statement <- sprintf("%s%s", SD.printed, SD.statement)
    }
  }
  i.statement <- create.i.statement(dt.name = dt.name, the.filter = the.filter)

  the.statement <- create.dt.statement(dt.name = dt.name, i.statement = i.statement, j.statement = j.statement, grouping.variables = grouping.variables, sortby.group = sortby.group)

  res <- eval.dt.statement(the.statement = the.statement, return.as = return.as, envir = .GlobalEnv)

  revise.dt.status(dt.name = dt.name, envir = envir, is.format.dt = is.format.dt)

  return(res)
}
