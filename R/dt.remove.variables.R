#'dt.remove.variables
#'
#' @description  A function to remove selected columns from a data.frame or data.table object.
#'
#'
#' @return a `data.table` object.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.  A variable called dat should be referred to with dt.name = "dat" when using the function.
#' @param the.variables  A character or numeric vector specifying the variables that we want to remove. For character vectors, only values that exist in the names of the data will be used.  For numeric vectors, only the values of unique(floor(sorting.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be used to select column names from the data.
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the updated data.  return.as = "code" provides a data.table coding statement.  return.as = "all" provides a list object including both the resulting output and the code.
#' @param envir a specification of the environment in which the data (referenced by dt.name) exists, with the global environment as the default value.#'
#' @param ... additional arguments if required
#' @import data.table
#' @source DTwrappers::create.dt.statement
#' @source DTwrappers::eval.dt.statement
#' @examples
#' n <- nrow(iris)
#' dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = FALSE),])
#' dt.remove.variables(dt.name = "dat", the.variables = c("Category", "setosa_sl_below_5"),
#' return.as = "all")
#'
#'
#' @export
dt.remove.variables <- function(dt.name, the.variables, return.as = "result", envir = .GlobalEnv, ...) {

  
  is.format.dt <- check.dt.status(dt.name = dt.name, envir = envir)

  all.variable.names <- names(x = get(dt.name))

  if(is.numeric(the.variables) == TRUE){
    the.indices <- unique(floor(the.variables))

    the.variables <- all.variable.names[the.indices[the.indices %in% 1:ncol(x = get(x = dt.name, envir = envir))]]
  }

  the.variables <- the.variables[the.variables %in% names(get(x = dt.name, envir = envir))]
  num.variables <- length(the.variables)
  if(num.variables == 0){
    j.statement <- NULL
  }
  if(num.variables == 1){
    j.statement <- sprintf("%s := NULL", add.backtick(x = the.variables))
  }
  if(num.variables > 1){
    j.statement <- sprintf("c(%s) := NULL", paste(sprintf("'%s'", the.variables), collapse = ", "))
  }

  the.statement <- create.dt.statement(dt.name = dt.name, j.statement = j.statement)

  res <- eval.dt.statement(the.statement = the.statement, return.as = return.as, envir = .GlobalEnv)

  revise.dt.status(dt.name = dt.name, envir = envir, is.format.dt = is.format.dt)

  return(res)
}
