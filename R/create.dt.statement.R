# Internal Function
# description:  Creates a data.table coding statement as a character string based upon its components -- the i.statement, j.statement, and the list of grouping.variables.

# dt.name:  a character vector including either a) the name of the data.table object on which to compute the given statements, or b) a sequence of chained data.table calls onto which another statement will be chained, such as "dt[Age < 50, .(Gender, Age, Region)]".  Then the i.statement, j.statement, and by.statement can be subsequently chained, e.g. to generate "dt[Age < 50, .(Gender, Age, Region)][, .(mean(Age)), by = c("Gender", "Region")].  The statement can also begin by defining a variable, such as "res = dt[Age < 50, .(Gender, Age, Region)]".

# i.statement:  a character value or expression providing the filtering i step.  Specifying "Age < 50" with dt.name = "dt" would lead to "dt[Age < 50,]".

# j.statement:  a character value or expression providing the data.table j step, which performs calculations.  Specifying "mean(Age)" with dt.name = "dt" would lead to "dt[, mean(Age)]".

# grouping.variables:  a character vector with the names of the variables to group by.

# grouping.type:  a character value indicating whether the resulting table should be sorted (grouping.type = "keyby") or presented as is (grouping.type = "by"), with "keyby" as the default.

# Note:  This function works entirely with character values.  It does not directly evaluate any of these commands.  To do so on a resulting value called the.statement, you can use eval(parse(text = the.statement)) to run the data.table calculation.  Alternatively, you can call eval.dt.statement.

create.dt.statement <- function(dt.name, i.statement = NULL, j.statement = NULL, grouping.variables = NULL, sortby.group = TRUE, envir = .GlobalEnv, ...){
  
  check.dt.exists(dt.name = dt.name, envir = envir)
  
  if(is.null(i.statement) & is.null(j.statement)){
    return(dt.name)
  }
  if(is.null(i.statement) | mean(i.statement == "TRUE") == 1 | mean(i.statement == TRUE) == 1){
    i.statement <- ""
  }
  
  first.space <- " "
  second.comma <- ", "
  
  if(!is.null(grouping.variables)){
#    grouping.variables <-
#      eval(unique(grouping.variables[grouping.variables %in% names(get(dt.name))]))
    
    by.statement <- create.grouping.statement(dt.name = dt.name, grouping.variables = grouping.variables, sortby.group = sortby.group)
  }
  
  if(is.null(grouping.variables)){
    second.comma <- ""
    by.statement <- ""
  }
  if(is.null(j.statement)){
    j.statement <- ""
    by.statement <- ""
  }
  
  if(j.statement == "" & by.statement == ""){
    first.space <- ""
  }
  
  calculated.statement <- sprintf("%s[%s,%s%s%s%s]", dt.name, i.statement, first.space, j.statement, second.comma, by.statement)
  
  return(calculated.statement)
}


# Internal Function

# creates the i step of a data.table statement.
create.i.statement <- function(dt.name, the.filter = NULL, envir = .GlobalEnv){

  if(is.null(x = the.filter) == TRUE){
    return("")
  }
  
  logical.filter <- is.logical(x = the.filter)
  numeric.filter <- is.numeric(x = the.filter) | is.integer(x = the.filter)
  character.or.expression.filter <- is.character(x = the.filter) | is.expression(x = the.filter)
  
  if(logical.filter == F & numeric.filter == F & character.or.expression.filter == F){
    warning(sprintf("The value of the.filter must be either a) a vector that is logical (TRUE/FALSE), numeric, or integer or b) a character value or expression providing a logical test to apply to the data.  Since the.filter was not specified in this way, no filtering will be applied."))
    i.statement <- NULL
  }
  
  if(character.or.expression.filter == TRUE){
    i.statement <- as.character(x = the.filter)
  }
  
  if(logical.filter == TRUE | numeric.filter == TRUE){
    n <- nrow(get(x = dt.name, envir = envir))
    
    if(logical.filter == TRUE){
      len.filter <- length(the.filter)
      if(len.filter == n){
        i.statement <- reduce.vector.expression(x = which(the.filter == TRUE))
      }
      if(len.filter != n){
        warning(sprintf("The values of the.filter were specified as a logical (TRUE/FALSE) vector, but it was not the same length as nrow(%s).  No filtering will be performed.", dt.name))
        i.statement <- NULL
      }
    }
    if(numeric.filter == TRUE){
      included.rows <- the.filter[the.filter %in% 1:n]
      num.included.rows <- length(included.rows)
      
      if(num.included.rows == 0){
        i.statement <- NULL
      }
      if(num.included.rows == 1){
        i.statement <- sprintf("%d", included.rows)
      }
      if(num.included.rows > 1){
        i.statement <- reduce.vector.expression(x = included.rows)
      }
    }
  }
  
  return(i.statement)
}


# Internal Function

# creates the by or keyby step of a data.table statement.

create.grouping.statement <- function(dt.name, grouping.variables, sortby.group = TRUE){
  check.dt.exists(dt.name = dt.name)
  all.dt.names <- names(get(dt.name))
  
  statement.type <- "by"
  if(sortby.group == TRUE){
    statement.type <- "keyby"
  }
  
  gv <- grouping.variables
  
  num.gv <- length(gv)
  
  if(length(gv) == 1 & gv[1] %in% all.dt.names){
    grouping.statement <- sprintf("%s = %s", statement.type, gv)
  }
  if(length(gv) > 1 | !(gv[1] %in% all.dt.names)){
    all.gv.in.names <- mean(grouping.variables %in% all.dt.names) < 1
    if(all.gv.in.names == T){
      use.case <- "list"
      gv[gv %in% all.dt.names] <- add.backtick(x = gv[gv %in% all.dt.names])
    }
    if(all.gv.in.names == F){
      use.case <- "c"
      gv <- sprintf("'%s'", gv)
    }
    grouping.statement <- sprintf("%s = %s(%s)", statement.type, use.case, paste(gv, collapse = ", "))
  }
  
  return(grouping.statement)
  
}
