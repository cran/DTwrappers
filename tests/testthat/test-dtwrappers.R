context('DTwrappers')

library(data.table)

n <- nrow(iris)
dat <- data.table::as.data.table(x = iris[sample(x = 1:n, size = n, replace = F),])


#1 choose rows check

chosefirstthree = setDT(iris[1:3,])
firstthree_dtwrappers=dt.choose.rows(dt.name = 'iris', the.filter = 1:3)

test_that('select first 3 rows of dataset', {
  expect_equal(firstthree_dtwrappers, chosefirstthree)
  
})

#2 choose cols check

chose_first_5_allcolumns=setDT((iris[1:5,]))
chose_first5_dtwrappers=dt.choose.cols(dt.name = "iris", the.variables = ".", the.filter = "1:5")

test_that('select first 5 entries of dataset with all columns', {
  expect_equal(chose_first_5_allcolumns,chose_first5_dtwrappers )
  
})

#3 count rows check
countrows=dat[, .N]
countrows_dtwrappers=dt.count.rows(dt.name = "iris")

test_that('count rows in dataset', {
  expect_equal(countrows,countrows_dtwrappers)
  
})

#4 dt.calculate check

dt.calculate.orig=dat[, .(variable = 'Sepal.Length', mean = mean(Sepal.Length))]
dt.calculate.dtwrappers=dt.calculate(dt.name = "iris", the.functions = "mean", the.variables = "Sepal.Length")

test_that('check dt calculate', {
  expect_equal(dt.calculate.orig,dt.calculate.dtwrappers)
  
})





