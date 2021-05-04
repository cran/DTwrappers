## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = TRUE
)

## ----setup--------------------------------------------------------------------
library(DTwrappers)
data(iris)
n <- nrow(iris)
dt.name <- "dat"

RNGversion(vstr = 3.6)
set.seed(seed = 921)

# Randomizes the order of the rows.
dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = F),])
head(dat)

## ----display first 3 rows of iris dataset-------------------------------------
dt.choose.rows(dt.name = "dat", the.filter = 1:3)

## ----display first 3 rows of iris dataset with filter-------------------------
dt.choose.rows(dt.name = "dat", the.filter = "1:3")

## ----display first 3 rows of iris dataset with code---------------------------
dt.choose.rows(dt.name = "dat", the.filter = "1:3", return.as = "code")

## ----display first 3 rows of iris dataset with code and filter----------------
dt.choose.rows(dt.name = "dat", the.filter = "1:3", return.as = "all")

## ----chose rows when the sepal length is less than 4.4------------------------
dt.choose.rows(dt.name = "dat", the.filter = "Sepal.Length < 4.4", return.as = "all")

## ----choose rows with filter as expression------------------------------------
dt.choose.rows(dt.name = "dat", the.filter = expression(Sepal.Length < 4.4), return.as = "all")

## ----choose rows more complex filtering---------------------------------------
dt.choose.rows(dt.name = "dat", the.filter = "Sepal.Width >= 3 & Sepal.Length < 4.8 & Species == 'setosa'", return.as = "all")

## ----chose all columns for iris with first 5 entries--------------------------
dt.choose.cols(dt.name = "dat", the.variables = ".", the.filter = "1:5", return.as = "all")

## ----chose species and sepal length for first 3 entries-----------------------
dt.choose.cols(dt.name = "dat", the.variables = c("Species", "Sepal.Length"), the.filter = "1:3", return.as = "all")

## ----chose first 2 rows of data for each species and print sepal length and sepal width for the flowers----
dt.choose.cols(dt.name = "dat", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species", first.k = 2, return.as = "all")

## ----first2_per_group---------------------------------------------------------
dt.first.k.rows(dt.name = "dat", k = 2, the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species", return.as = "all")

## ----chose last 2 rows of data for each species and print sepal length and sepal width for the flowers----
dt.choose.cols(dt.name = "dat", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species", last.k = 2, return.as = "all")

## ----dt.last.k.rows-----------------------------------------------------------
dt.last.k.rows(dt.name = "dat", k = 2, the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species", return.as = "all")

## ----row indices--------------------------------------------------------------
dt.choose.cols(dt.name = "dat", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species", row.indices = c(3,5,7), return.as = "all")

## ----Count the number of rows in the dataset iris-----------------------------
dt.count.rows(dt.name = "dat", return.as = "all")

## ----Count number of rows where species is "Setosa"---------------------------
dt.count.rows(dt.name = "dat", the.filter = "Species == 'setosa'", return.as = "all")

## ----Count in subgroups-------------------------------------------------------
dt.count.rows(dt.name = "dat", the.filter = "Petal.Length > 1.7", grouping.variables = "Species", return.as = "all")

## ----Count in subgroups with new name-----------------------------------------
dt.count.rows(dt.name = "dat", the.filter = "Petal.Length > 1.7", grouping.variables = "Species", count.name = "Total Qualifying Rows", return.as = "all")

## ----define category as a new column with iris as its value-------------------
dt.define.variable(dt.name = "dat", variable.name = "Category", the.values = "Flower: Iris", return.as = "all", specification = "by.value")

## ----define max sepal length species variable---------------------------------
dt.define.variable(dt.name = "dat", variable.name = "Max_Sepal_Length_Species", the.values = "max(Sepal.Length)", specification = "by.expression", grouping.variables = "Species", sortby.group = TRUE, return.as = "all")

## ----define.variable.in.subset------------------------------------------------
dt.define.variable(dt.name = "dat", variable.name = "setosa_sl_below_5", the.values = "Sepal.Length < 5", specification = "by.expression", the.filter = "Species == 'setosa'", return.as = "all")

## ----removing category as a variable------------------------------------------
dt.remove.variables(dt.name = "dat", the.variables = c("Category", "setosa_sl_below_5"), return.as = "all")

## ----sort species and sepal length in increasing order------------------------
dt.sort(dt.name = "dat", sorting.variables = c("Species", "Sepal.Length"), sort.increasing = TRUE, return.as = "all")

## ----sort species and sepal length in decreasing order------------------------
dt.sort(dt.name = "dat", sorting.variables = c("Species", "Sepal.Length"), sort.increasing = FALSE, return.as = "all")

## ----species in increasing order and sepal length in decreasing order---------
dt.sort(dt.name = "dat", sorting.variables = c("Species", "Sepal.Length"), sort.increasing = c(T, F), return.as = "all")

## ----calculate_one_fn_one_variable--------------------------------------------
dt.calculate(dt.name = "dat", the.functions = "mean", the.variables = "Sepal.Length", return.as = "all")

## ----calculate_multiple_fn----------------------------------------------------
dt.calculate(dt.name = "dat", the.functions = c("mean", "median", "sd"), the.variables = "Sepal.Length", return.as = "all")

## ----dt.calculate.parameters--------------------------------------------------
dt.calculate(dt.name = "dat", the.functions = c("mean", "median", "sd"), the.variables = "Sepal.Length", other.params = "na.rm = T", return.as = "all")

## ----dt.calculate.filter.group------------------------------------------------
dt.calculate(dt.name = "dat", the.functions = c("mean", "median", "sd"), the.variables = "Sepal.Length", the.filter = "Sepal.Length > 3.5", grouping.variables = "Species", return.as = "all")

## ----dt.calculate.filter.group.mult.variables---------------------------------
dt.calculate(dt.name = "dat", the.functions = c("mean", "median", "sd"), the.variables = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), the.filter = "Sepal.Length > 3.5", grouping.variables = "Species", return.as = "all")

## ----dt.calculate.filter.group.mult.variables.wide----------------------------
#dt.calculate(dt.name = "dat", the.functions = c("mean", "median", "sd"), the.variables = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), the.filter = "Sepal.Length > 3.5", grouping.variables = "Species", table.format = "wide", return.as = "all")

## ----adding triple mean as a function in dt.calculate-------------------------
`triple mean` <- function(x, na.rm = T){
  return(3 * mean(x = x, na.rm = na.rm))
}

dt.calculate(dt.name = "dat", the.variables = c("Sepal.Length", "Sepal.Width"), the.functions = c("mean", "sd", "triple mean"), grouping.variables = "Species", table.format = "long", return.as = "all")

## ----rowSums------------------------------------------------------------------
dt.calculate(dt.name = "dat", the.functions = "rowSums", the.variables = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), the.filter = 1:5, individual.variables = FALSE, return.as = "all")

## ----get.lm.coefs-------------------------------------------------------------
get.lm.coefs <- function(data, formula){
  require(data.table)
  mod <- lm(formula = formula, data = data)
  the.coefs <- as.data.table(x = summary(mod)$coefficients, keep.rownames = TRUE)
  setnames(x = the.coefs, old = "rn", new = "Variable")
  return(the.coefs)
}

## ----linear regression using dt.calculate-------------------------------------
## linear regression 
dt.calculate(dt.name = "dat", the.functions = "get.lm.coefs", grouping.variables = "Species", other.params = "formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width", return.as = "all", individual.variables = F, add.function.name = F)

