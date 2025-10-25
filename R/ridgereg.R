#' Ridge Regression Function
#'
#' Fits a ridge regression model using the closed-form solution
#' \eqn{(X'X + λI)^{-1}X'y}, with an option to center \code{y}
#' and standardize predictors conditionally.
#'
#' @param formula A formula specifying the model (e.g., \code{y ~ x1 + x2}).
#' @param data A data frame containing the variables in the model.
#' @param lambda A non-negative numeric value specifying the ridge penalty.
#'   When \code{lambda = 0}, the model reduces to ordinary least squares (OLS).
#' @param center_y Logical; if \code{TRUE}, the response variable \code{y}
#'   is centered to have mean zero. Default is \code{FALSE}.
#'
#' @details
#' The function standardizes the predictor variables (excluding the intercept)
#' only if \code{lambda != 0}. The intercept term is excluded from penalization.
#'
#' The ridge regression coefficients are computed as:
#' \deqn{\hat{\beta}_{ridge} = (X'X + λI)^{-1}X'y}
#' where \eqn{I} is an identity matrix with the (1,1) element set to 0.
#'
#' @return
#' A list of class \code{"ridgereg"} containing:
#' \item{beta_ridge}{Estimated ridge regression coefficients.}
#' \item{y_hat}{Predicted values of the response variable.}
#' \item{X}{Model matrix used for fitting.}
#' \item{y}{Response vector.}
#' \item{lambda}{The ridge penalty used.}
#' \item{call}{The matched function call.}
#'
#' @examples
#' data(mtcars)
#' model <- ridgereg(mpg ~ wt + hp, data = mtcars, lambda = 1)
#' model$beta_ridge
#'
#' @export

ridgereg <- function(formula, data, lambda, center_y = FALSE){
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  tt <- stats::terms(mf)
  X  <- stats::model.matrix(tt, mf)
  y  <- as.numeric(stats::model.response(mf))

  # Conditional normalization: only normalize X if lambda ≠ 0
  if (lambda != 0) {
    X_no_intercept <- X[, -1, drop = FALSE]
    X_no_intercept <- scale(X_no_intercept)
    X <- cbind("(Intercept)" = 1, X_no_intercept)

    if (center_y) {
      y <- y - mean(y)
    }
  }

  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  I <- diag(ncol(X))
  I[1, 1] <- 0  # Don't penalize intercept

  beta_ridge <- as.numeric(solve(XtX + lambda * I, Xty))
  names(beta_ridge) <- colnames(X)

  y_hat <- as.vector(X %*% beta_ridge)

  ridgereg <- list(
    beta_ridge = beta_ridge,
    y_hat = y_hat,
    X = X,
    y = y,
    lambda = lambda,
    call = match.call(),
    terms = tt
  )
  class(ridgereg) <- "ridgereg"
  return(ridgereg)
}

#' Print Method for Ridge Regression Objects
#'
#' Displays the function call and the estimated ridge regression coefficients.
#'
#' @param x An object of class \code{"ridgereg"} returned by \code{\link{ridgereg}}.
#'
#' @return
#' The function returns \code{x} invisibly after printing a summary of the model.
#'
#' @method print ridgereg
#' @export

print.ridgereg <- function(x){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$beta_ridge)
  invisible(x)
}


#' Predict Method for Ridge Regression Objects
#'
#' Returns the fitted values from a ridge regression model.
#'
#' @param x An object of class \code{"ridgereg"} returned by \code{\link{ridgereg}}.
#'
#' @return
#' A numeric vector of predicted (fitted) values.
#'
#' @method predict ridgereg
#' @export
#' Predict Method for Ridge Regression Objects (caret-safe)
#'
#' @param object A "ridgereg" model object.
#' @param newdata New data for prediction (data frame or matrix).
#' @param ... Additional arguments.
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$y_hat)
  }

  # Always work with a data frame
  newdata <- as.data.frame(newdata)

  # Identify predictors from training
  predictors <- setdiff(colnames(object$X), "(Intercept)")

  # Subset and align predictors
  X_new <- newdata[, predictors, drop = FALSE]

  # Replace any missing columns with zeros
  missing_cols <- setdiff(predictors, colnames(newdata))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) X_new[, col] <- 0
  }

  # Reorder to match training matrix
  X_new <- X_new[, predictors, drop = FALSE]

  # Add intercept column
  X_new <- cbind("(Intercept)" = 1, X_new)

  as.vector(as.matrix(X_new) %*% object$beta_ridge)
}


#' Coefficients Method for Ridge Regression Objects
#'
#' Extracts the estimated ridge regression coefficients.
#'
#' @param x An object of class \code{"ridgereg"} returned by \code{\link{ridgereg}}.
#'
#' @return
#' A named numeric vector of ridge regression coefficients.
#'
#' @method coef ridgereg
#' @export
coef.ridgereg <- function(x, ...){
  return(x$beta_ridge)
}


