context("elect_viz")
library(readxl)
#data <- read_excel("../../data/2014_riksdagsval_per_valdistrikt.xls")
path <- "2014_riksdagsval_per_valdistrikt.xls"
test_that("elect_viz rejects bad data", {
  expect_error(viz <- linreg$new(path=iris))
})

test_that("class is correct", {
  viz <- elect_viz$new(path=path)
  expect_true(class(viz)[1] == "elect_viz")
})

test_that("Setting and getting county works", {
  viz <- elect_viz$new(path=path)
  expect_equal(round(unname(viz$get_mean_p_vals()),2),
               c(31.54, 4.91, 6.77, 5.24, 25.20, 4.90, 7.80, 9.86, 2.71, 1.05))
})

test_that("get_county() gives a correct output", {
  viz <- elect_viz$new(path=path)
  expect_equal(viz$get_county(), "Stockholms län")
})

test_that("set_county() with wrong input doesnt work", {
  viz <- elect_viz$new(path=path)
  expect_error(viz$set_county("My backyard"))
})

test_that("Setting and getting county works", {
  viz <- elect_viz$new(path=path)
  test_county <- viz$get_county()
  viz$set_county("Uppsala län")
  expect_true(test_county != viz$get_county())
})

test_that("County list is correct", {
  viz <- elect_viz$new(path=path)
  expect_equal(viz$get_counties()[c(1,5,7)],
               c("Stockholms län", "Östergötlands län", "Kronobergs län"))
})

test_that("set_county() rejects erronous input", {
  viz <- elect_viz$new(path=path)
  expect_error(viz$set_county(123))
})

test_that("Setting county variable to wrong value and then run get_mean_p_vals()", {
  viz <- elect_viz$new(path=path)
  viz$county <- "Wrong input"
  expect_error(viz$get_mean_p_vals())
})

test_that("Setting county variable to wrong value and then run get_county()", {
  viz <- elect_viz$new(path=path)
  viz$county <- "Wrong input"
  expect_error(viz$get_county())
})

test_that("Setting county variable to wrong value and then run get_county()", {
  viz <- elect_viz$new(path=path)
  viz$county_list <- replicate(29, "asd")
  expect_error(viz$get_counties())
})
