# =========================================================
# Tests for ridgereg() - 732A94 Advanced R Programming
# =========================================================
# These tests check the correctness and behavior of your ridge regression
# implementation based on the lab’s specification:
#   - Only X is normalized (xnorm = (x - mean(x)) / sqrt(var(x)))
#   - y is not centered
#   - Ridge penalty shrinks coefficients with increasing lambda
# =========================================================

library(testthat)

test_that("ridgereg() returns correct structure and output types", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                    data = iris, lambda = 1)

  # Check basic structure
  expect_s3_class(model, "ridgereg")
  expect_true(is.numeric(coef(model)))
  expect_named(coef(model))
  preds <- predict(model)
  expect_type(preds, "double")
  expect_equal(length(preds), nrow(iris))
})


test_that("ridge coefficients shrink as lambda increases", {
  data(iris)
  model_small <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                          data = iris, lambda = 0.1)
  model_large <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                          data = iris, lambda = 100)

  # Ridge penalty should shrink slope magnitudes (exclude intercept)
  expect_true(all(abs(coef(model_large)[-1]) < abs(coef(model_small)[-1])))
})


test_that("ridge with lambda = 0 approximates OLS", {
  data(iris)
  model_ridge <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                          data = iris, lambda = 0)
  model_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                 data = iris)

  cf_ridge <- coef(model_ridge)[-1]
  cf_lm <- coef(model_lm)[-1]

  # Coefficients should have same direction and high correlation
  expect_equal(sign(cf_ridge), sign(cf_lm))
  expect_true(cor(cf_ridge, cf_lm) > 0.9)
})


test_that("model predictions are reasonable", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                    data = iris, lambda = 1)
  preds <- predict(model)

  rmse_model <- sqrt(mean((iris$Sepal.Length - preds)^2))
  rmse_naive <- sqrt(mean((iris$Sepal.Length - mean(iris$Sepal.Length))^2))

  # Ridge regression should perform better than predicting the mean
  expect_true(rmse_model < rmse_naive)
})


test_that("ridgereg and MASS::lm.ridge give similar coefficient direction", {
  library(MASS)
  data(iris)
  model_my <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                       data = iris, lambda = 1)
  model_mass <- MASS::lm.ridge(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                               data = iris, lambda = 1)

  cf_my <- coef(model_my)[-1]   # exclude intercept
  cf_mass <- coef(model_mass)

  # Should have same signs and strong positive correlation
  expect_equal(sign(cf_my), sign(cf_mass))
  expect_true(cor(cf_my, cf_mass) > 0.9)
})


test_that("print() displays coefficients and call", {
  data(iris)
  model <- ridgereg(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
                    data = iris, lambda = 1)
  expect_output(print(model), "Coefficients")
})
