#' Add backtick
#'
#' Function that add backticks to the input variables.
#'
#' @param  x  Character value specifying the name of input parameters.
#' @param  include.backtick specifies whether a backtick should be added. Parameter values should be either 'all' or 'as.needed'
#' @param dat the dataset
#' @return None

add.backtick <- function(x, include.backtick = "as.needed", dat = NULL){

  len.x <- length(x)
  if(include.backtick == "all"){
    w <- 1:len.x
  }
  if (include.backtick == "as.needed") {
    if(is.null(dat)){
      w <- which(x != make.names(names = x))
    }
    if(!is.null(dat)){
      #require(data.table)
      setDT(dat)
      requires.backtick <- logical(length = len.x)

      for(i in 1:len.x){
        value.exists <- is.null(tryCatch(expr = dat[, unique(eval(parse(text = x[i])))], error = function(e) return(NULL)))

        if(value.exists == TRUE & x[i] %in% names(dat) & x[i] != make.names(x[i])){
          requires.backtick[i] <- TRUE
        }
      }
      w <- which(requires.backtick == TRUE)
    }

  }
  if (length(w) > 0) {
    x[w] <- sprintf("`%s`", x[w])
  }
  return(x)
}


#' dt.calculate
#'
#' @description  This function allows a user to apply one or more functions to all of the specified variables in a data.frame or data.table object.  It is built as a wrapper function of data.table's method of applying functions to variables while allowing for filtering and grouping steps.  This allows a user to easily calculate many results, e.g. the.functions = c("mean", "median", "sd") on multiple columns, e.g. the.variables = c("Age", "Income") while also filtering and grouping the data.  Options also exist to return a data.table coding statement (result = "code") for educational purposes or both the result and the code together (result = "all").  For examples, please see the vignette.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object to select data from.  A variable called dat should be referred to with dt.name = "dat" when using the function.
#' @param the.functions  A character vector specifying the name of the functions to apply to the.variables.  Each function included in the.functions will be separately applied to each variable in the.variables.
#' @param the.variables  A character or numeric vector specifying the variables to perform the calculations on. For character vectors, only values that exist in the names of the data will be used.  For numeric vectors, only the values of unique(floor(sorting.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be used to select column names from the data.  Other values in sorting.variables that do not correspond to a defined column will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#' @param the.filter  a character value, numeric vector, logical vector, or expression stating the logical operations used to filter the data.  The filtering step will be applied prior to generating the counts.  Defaults to NULL unless otherwise specified.  Logical vectors will be converted to a numeric filter, e.g. c(TRUE, TRUE, FALSE) will become 1:2 to signify which rows should be selected.
#' @param grouping.variables  A character or numeric vector specifying the variables to perform the calculations on. For character vectors, the values may be either column names of the data or calculations based upon them (see the vignette for examples).  For numeric vectors, only the values of unique(floor(grouping.variables)) that are in 1:ncol() of your data will be used.  Then these indices will be mapped to the corresponding column names from the data.  When NULL, no grouping will be performed.
#' @param sortby.group  A logical value specifying whether the grouping should be sorted (TRUE, the default value) or as is (FALSE).
#' @param add.function.name  A logical value specifying whether the name of the function applied should be appended to
#'        the column names in the resulting table.  Only applies if the.functions is of length 1.
#' @param other.params  A character value specifying any additional parameters needed to call the.functions.
#'        For instance, if the.functions = "mean", and you would like to remove missing values, then specifying
#'        other.params = "na.rm = TRUE" as a character would suffice.  Multiple parameters can be specified with comma
#'        separation, e.g. other.params = "trim = 1, na.rm = TRUE".  Note that all of the parameters supplied must
#'        apply to all of the.functions
#' @param table.format specify the format of the table depending on the desired output i.e. "long" or "wide"
#' @param individual.variables a logical variable specifying if variables are grouped or individual
#' @param output.as.table a logical variable to specify if output should be a table or not
#' @param return.as a character value specifying what output should be returned.  return.as = "result" provides the table of counts.  return.as = "code" provides a data.table coding statement that can generate the table of counts.  return.as = "all" provides a list containing both the resulting table and the code.
#' @param envir the environment in which the code would be evaluated; .GlobalEnv by default.
#'
#' @param  ... additional arguments to be passed
#'
#' @source DTwrappers::create.dt.statement
#' @source DTwrappers::eval.dt.statement
#' @source DTwrappers::add.backtick
#' @import data.table
#' @return Depending on the value of return.as, the output will be a) a character value (return.as = 'code'), b) a coding output, typically a data.table (return.as = 'result'), or c) a list containing both the code and output (return.as = 'all')
#' 
#'
#'@examples
#' n <- nrow(iris)
#' dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = FALSE),])
#' dt.calculate(dt.name = "dat", the.variables = c("Sepal.Length"),
#' the.functions = c("mean", "sd"), return.as = "all")
#'

#' @export
dt.calculate <-
  function(dt.name,
           the.functions,
           the.variables = ".",
           the.filter = NULL,
           grouping.variables = NULL,
           sortby.group = TRUE,
           other.params = "",
           table.format = "long",
           add.function.name = TRUE,
           individual.variables = TRUE,
           output.as.table = TRUE,
           return.as = "result",
           envir = .GlobalEnv,
           ...) {
  

  is.format.dt <- check.dt.status(dt.name = dt.name, envir = envir)

  outcome.name <- "Outcome"
  variable.name <- "Variable"
  function.name <- "Function"
  command.name <- "Command"

  value.long <- "long"
  dots.with.comma <- ", ..."
  if(other.params == "" | is.na(other.params) | is.null(other.params)){
    dots.with.comma <- ""
  }

  all.variable.names <- names(get(x = dt.name, envir = envir))

  functions.exist <- sapply(X = the.functions, FUN = "exists", envir = envir)

  the.functions <- names(functions.exist)[functions.exist == TRUE]

  if (length(the.functions) == 0 | is.null(the.functions) | is.na(the.functions[1])){
    stop("Error:  the.functions must be specified with functions that are loaded.  Make sure to call library() for the required packages.")
  }

  if(is.numeric(x = grouping.variables) == TRUE){
    grouping.indices <- unique(floor(grouping.variables))
    grouping.variables <- all.variable.names[grouping.indices]
  }

  grouping.variables <- unique(grouping.variables)

  if ("." %in% the.variables){
    the.variables <- all.variable.names
  }

  if(is.numeric(x = the.variables) == TRUE){
    the.indices <- unique(floor(the.variables))

    the.variables <- all.variable.names[the.indices[the.indices %in% 1:ncol(x = get(x = dt.name, envir = envir))]]
  }

  the.variables <-
    unique(the.variables[the.variables %in% all.variable.names & !(the.variables %in% grouping.variables)])


  non.grouping.variables <- all.variable.names[!(all.variable.names %in% grouping.variables)]

  num.functions <- length(the.functions)
  num.variables <- length(the.variables)

  series.variables <- paste(sprintf("'%s'", the.variables), collapse = ", ")

  calculate.together <- is.logical(individual.variables) & individual.variables[1] == FALSE

  if(calculate.together == TRUE){
    if(num.variables == 1){
      SD.command.name = ""
    }

    if(num.variables > 1){
      SD.command.name <- sprintf(", .SDcols = c(%s)", paste(sprintf("'%s'", the.variables), collapse = ", "))

      if(num.variables == length(non.grouping.variables) & mean(the.variables %in% non.grouping.variables) == 1){
        SD.command.name <- ""
      }
    }

    j.statement <- sprintf(".(%s)%s", paste(sprintf("%s = %s(.SD%s)", add.backtick(the.functions), add.backtick(x = the.functions), dots.with.comma), collapse = ", "), SD.command.name)

    if(num.functions == 1 & ((is.logical(add.function.name) & add.function.name[1] == F) | is.logical(output.as.table) & output.as.table[1] == F)){
      j.statement <- sprintf("%s(.SD%s)", add.backtick(the.functions), dots.with.comma)
    }

  }
  if(calculate.together == FALSE){
    if(num.variables == 1){
      printed.variables <- series.variables
      SD.command.name = ""
    }

    if(num.variables > 1){
      printed.variables <- sprintf("c(%s)", series.variables)
      SD.command.name <- sprintf(", .SDcols = c(%s)", paste(sprintf("'%s'", the.variables), collapse = ", "))

      if(num.variables == length(non.grouping.variables) & mean(the.variables %in% non.grouping.variables) == 1){
        SD.command.name <- ""
      }
    }


    if(num.functions == 1 & add.function.name == FALSE){
      if(num.variables == 1){
        j.statement <- sprintf(".(%s = %s(%s%s))", add.backtick(the.variables), add.backtick(x = the.functions), add.backtick(the.variables), dots.with.comma)
      }
      if(num.variables > 1){
        j.statement <- sprintf("lapply(X = .SD, FUN = '%s'%s)%s", the.functions, dots.with.comma, SD.command.name)
      }
    }

    if(table.format == value.long){

      if(num.variables > 1){
        lapply.statements <- paste(sprintf("%s = lapply(X = .SD, FUN = '%s'%s)", add.backtick(x = the.functions), the.functions, dots.with.comma), collapse = ", ")

        j.statement <- sprintf(".(variable = %s, %s)%s", printed.variables, lapply.statements, SD.command.name)
      }

      if(num.variables == 1){
        lapply.statements <- paste(sprintf("%s = %s(%s%s)", add.backtick(x = the.functions), add.backtick(the.functions), the.variables, dots.with.comma), collapse = ", ")
        j.statement <- sprintf(".(variable = %s, %s)", printed.variables, lapply.statements)
      }
    }
    if(table.format != value.long){
      if(num.functions > 1 | add.function.name == TRUE){
        the.grid <-
          setDT(expand.grid(the.variables, the.functions, stringsAsFactors = FALSE))
        setnames(x = the.grid, old = names(the.grid), new = c(variable.name, function.name))

        if(add.function.name == TRUE){
          the.grid[, eval(outcome.name) := sprintf("%s_%s", get(variable.name), get(function.name))]
        }
        if (add.function.name == FALSE) {
          the.grid[, eval(outcome.name) := get(variable.name)]
        }
        for (i in 1:the.grid[, .N]) {
          the.grid[, eval(command.name) := sprintf("%s = %s(%s%s)", add.backtick(x = get(outcome.name)), add.backtick(x = get(function.name)), add.backtick(x = get(variable.name)), dots.with.comma)]
        }

        j.statement <-
          sprintf(".(%s)", the.grid[, paste(get(command.name), collapse = ", ")])
      }

    }
  }



  i.statement <- create.i.statement(dt.name = dt.name, the.filter = the.filter)

  the.statement <- create.dt.statement(dt.name = dt.name, i.statement = i.statement, j.statement = j.statement, grouping.variables = grouping.variables, sortby.group = sortby.group)

  the.statement <- gsub(pattern = "...", replacement = other.params, x = the.statement, fixed = TRUE)

  res <- eval.dt.statement(the.statement = the.statement, return.as = return.as, envir = .GlobalEnv)

  revise.dt.status(dt.name = dt.name, envir = envir, is.format.dt = is.format.dt)

  return(res)
}
