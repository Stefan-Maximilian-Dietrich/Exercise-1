# tests/testthat/test_student.R
library(testthat)
source("student.R")

`%||%` <- function(x, y) if (is.null(x)) y else x
norm <- function(x) sort(unique(toupper(x)))

expected_fn <- function(Xa, Xb) {
  prop.test(c(Xa, Xb), c(66, 137), alternative = "less")
}

cases <- list(
  list(Xa = 20, Xb = 40),
  list(Xa = 33, Xb = 55),
  list(Xa = 10, Xb = 15),
  list(Xa = 0,  Xb = 0),
  list(Xa = 66, Xb = 137)
)

test_that("test() entspricht prop.test(..., alternative='less')", {
  for (cs in cases) {
    got <- test(cs$Xa, cs$Xb)
    exp <- expected_fn(cs$Xa, cs$Xb)
    
    expect_true(inherits(got, "htest"))
    expect_equal(got$parameter, exp$parameter, tolerance = 1e-10)
    expect_equal(got$statistic, exp$statistic, tolerance = 1e-10)
    expect_equal(got$p.value,   exp$p.value,   tolerance = 1e-12)
    expect_length(got$conf.int, 2)
    expect_equal(length(got$estimate), length(exp$estimate))
  }
})

test_that("alternative='less' ist wirklich gesetzt", {
  Xa <- 5; Xb <- 30
  got <- test(Xa, Xb)
  exp_less <- prop.test(c(Xa, Xb), c(66, 137), alternative = "less")
  exp_two  <- prop.test(c(Xa, Xb), c(66, 137), alternative = "two.sided")
  expect_equal(got$p.value, exp_less$p.value, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(got$p.value, exp_two$p.value, tolerance = 1e-12)))
})
