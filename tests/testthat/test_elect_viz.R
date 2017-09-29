context("elect_viz")
library(readxl)
data <- read_excel("../../data/2014_riksdagsval_per_valdistrikt.xls")

test_that("elect_viz rejects bad data", {
  expect_error(viz <- linreg$new(data=iris))
})

test_that("class is correct", {
  viz <- elect_viz$new(data=data)
  expect_true(class(viz)[1] == "elect_viz")
})

test_that("Setting and getting county works", {
  viz <- elect_viz$new(data=data)
  expect_equal(round(unname(viz$get_mean_p_vals()),2),
               c(31.54, 4.91, 6.77, 5.24, 25.20, 4.90, 7.80, 9.86, 2.71, 1.05))
})

test_that("get_county() gives a correct output", {
  viz <- elect_viz$new(data=data)
  expect_equal(viz$get_county(), "Stockholms län")
})

test_that("set_county() with wrong input doesnt work", {
  viz <- elect_viz$new(data=data)
  expect_error(viz$set_county("My backyard"))
})

test_that("Setting and getting county works", {
  viz <- elect_viz$new(data=data)
  test_county <- viz$get_county()
  viz$set_county("Uppsala län")
  expect_true(test_county != viz$get_county())
})

test_that("County list is correct", {
  expect_equal(viz$get_counties()[c(1,5,7)],
               c("Stockholms län", "Östergötlands län", "Kronobergs län"))
})

test_that("set_county() rejects erronous input", {
  viz <- elect_viz$new(data=data)
  expect_error(viz$set_county(123))
})

test_that("Setting county variable to wrong value and then run get_mean_p_vals()", {
  viz <- elect_viz$new(data=data)
  viz$county <- "Wrong input"
  expect_error(viz$get_mean_p_vals())
})

test_that("Setting county variable to wrong value and then run get_county()", {
  viz <- elect_viz$new(data=data)
  viz$county <- "Wrong input"
  expect_error(viz$get_county())
})

test_that("Setting county variable to wrong value and then run get_county()", {
  viz <- elect_viz$new(data=data)
  viz$county_list <- replicate(29, "asd")
  expect_error(viz$get_counties())
})
#
# test_that("print() method works", {
#   linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#   expect_output(linreg_mod$print(),"linreg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
#   expect_output(linreg_mod$print()," \\(Intercept\\)   Sepal\\.Width  Sepal\\.Length")
# })
#
# test_that("pred() method works", {
#   linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#   expect_equal(round(unname(linreg_mod$pred()[c(1,5,7)]),2), c(1.85, 1.53, 1.09))
# })
#
# test_that("resid() method works", {
#   linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#   expect_equal(round(unname(linreg_mod$resid()[c(7,13,27)]),2), c(0.31, -0.58, -0.20))
# })
#
# test_that("coef() method works", {
#   linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#   expect_true(all(round(unname(linreg_mod$coef()),2) %in% c(-2.52, -1.34, 1.78)))
# })
#
#
# test_that("summary() method works", {
#   linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#
#   expect_output(linreg_mod$summary(), "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*")
#   expect_output(linreg_mod$summary(), "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*")
#   expect_output(linreg_mod$summary(), "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*")
#   expect_output(linreg_mod$summary(), "Residual standard error: 0.6[0-9]* on 147 degrees of freedom")
# })
