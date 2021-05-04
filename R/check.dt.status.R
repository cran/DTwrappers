# Internal function to perform the following steps:
# 1) checks that dt.name refers to an object that exists
# 2) checks that dt.name refers to an object that is a data.frame or data.table
# 3) Converts the object to a data.table if it is not already.
# 4) Returns a status of TRUE if the data was originally a data.table and FALSE if it was a data.frame.  This allows us to later reconvert the data to its original format using revise.dt.status().
check.dt.status <- function(dt.name, envir = .GlobalEnv){
  check.dt.exists(dt.name = dt.name, envir = envir)
  
  is.format.df <- is.data.frame(x = get(x = dt.name, envir = envir))
  
  if(is.format.df == FALSE){
    stop("Error:  dt.name must refer to a variable that is a data.frame or data.table object.")
  }
  
  is.format.dt <- is.data.table(x = get(x = dt.name, envir = envir))
  
  if(is.format.dt == FALSE){
    data.table::setDT(get(x = dt.name, envir = envir))
  }
  
  return(is.format.dt)
}

# Internal function:  converts a data.table to a data.frame object if that was its original format (is.format.dt == FALSE).
revise.dt.status <- function(dt.name, envir, is.format.dt){
  if(is.format.dt == FALSE){
    data.table::setDF(get(x = dt.name, envir = envir))
  }
}
