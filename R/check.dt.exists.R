check.dt.exists <- function(dt.name, envir = .GlobalEnv){
  if(exists(x = dt.name, envir = envir) == FALSE){
    stop(sprintf("There is no variable with the name '%s' in the selected environment.  Please set dt.name to a character value with the name of a data.frame variable that is loaded in memory.", dt.name))
  }
}
