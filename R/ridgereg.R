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

ridgereg <- function(formula, data, lambda, center_y = TRUE){
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  tt <- stats::terms(mf)
  X <- stats::model.matrix(tt, mf)
  y <- as.numeric(stats::model.response(mf))

  # --- Store Scaling Metadata ---
  x_scale_params <- NULL
  y_center_mean <- 0

  # Conditional normalization
  if (lambda != 0) {
    # 1. Scale Features (X)
    X_no_intercept <- X[, -1, drop = FALSE]

    # Scale and store the attributes (center and scale) for consistent prediction
    X_scaled <- scale(X_no_intercept)
    x_scale_params <- list(
      center = attr(X_scaled, "scaled:center"),
      scale = attr(X_scaled, "scaled:scale")
    )

    # Reconstruct X with the intercept and scaled features
    X <- cbind("(Intercept)" = 1, X_scaled)

    # 2. Center Response (y)
    if (center_y) {
      y_center_mean <- mean(y) # Store the mean
      y <- y - y_center_mean
    }
  }

  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  I <- diag(ncol(X))
  I[1, 1] <- 0 # Don't penalize intercept

  beta_ridge <- as.numeric(solve(XtX + lambda * I, Xty))
  names(beta_ridge) <- colnames(X)

  # Calculate predictions for the centered y
  y_hat_centered <- as.vector(X %*% beta_ridge)

  # Bug Fix 1: UN-CENTER the predictions before storing (crucial for testing RMSE)
  y_hat <- y_hat_centered + y_center_mean

  ridgereg <- list(
    beta_ridge = beta_ridge,
    y_hat = y_hat, # Now stored on the original scale
    X = X,
    y = y,         # Still stores the centered/scaled X/y for debugging/internal consistency
    lambda = lambda,
    call = match.call(),
    terms = tt,
    # New metadata fields
    x_scale_params = x_scale_params,
    y_center_mean = y_center_mean
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
#' @param object An object of class \code{"ridgereg"} returned by \code{\link{ridgereg}}.
#' @param newdata New data for prediction (data frame or matrix).
#' @param ... Additional arguments.
#'
#' @return A numeric vector of predicted (fitted) values.
#'
#' @method predict ridgereg
#' @export
#' @examples
#' model <- ridgereg(y ~ x1 + x2, data = mydata, lambda = 1)
#' preds <- predict(model, newdata = mydata)
#'
# predict.ridgereg <- function(object, newdata = NULL, ...) {
#   if (is.null(newdata)) {
#     return(object$y_hat)
#   }
#
#   # Always work with a data frame
#   newdata <- as.data.frame(newdata)
#
#   # Identify predictors from training
#   predictors <- setdiff(colnames(object$X), "(Intercept)")
#
#   # Subset and align predictors
#   X_new <- newdata[, predictors, drop = FALSE]
#
#   # Replace any missing columns with zeros
#   missing_cols <- setdiff(predictors, colnames(newdata))
#   if (length(missing_cols) > 0) {
#     for (col in missing_cols) X_new[, col] <- 0
#   }
#
#   # Reorder to match training matrix
#   X_new <- X_new[, predictors, drop = FALSE]
#
#   # Add intercept column
#   X_new <- cbind("(Intercept)" = 1, X_new)
#
#   as.vector(as.matrix(X_new) %*% object$beta_ridge)
# }


# predict.ridgereg <- function(object, newdata, ...) {
#   if (missing(newdata)) {
#     # If no new data, return fitted values
#     return(object$y_hat)
#   }
#
#   # Ensure newdata is a data.frame
#   if (!is.data.frame(newdata)) {
#     newdata <- as.data.frame(newdata)
#   }
#
#   # Recreate model matrix using the same terms as in training
#   X_new <- model.matrix(delete.response(object$terms), newdata)
#
#   # Multiply by coefficients (ensure names align)
#   preds <- as.vector(X_new %*% object$beta_ridge)
#
#   return(preds)
# }


predict.ridgereg <- function(object, newdata, ...) {
  # If newdata is missing, return the pre-calculated, un-centered y_hat
  if (missing(newdata)) return(object$y_hat)

  if (!is.data.frame(newdata)) newdata <- as.data.frame(newdata)

  # Build model matrix using the stored terms (without response)
  X_new <- model.matrix(delete.response(object$terms), newdata)

  # Bug Fix 2: Apply the same scaling used in training to the new features
  if (!is.null(object$x_scale_params)) {
    # Isolate features (excluding intercept)
    X_no_intercept <- X_new[, -1, drop = FALSE]

    # Apply stored center and scale
    scaled_features <- sweep(X_no_intercept, 2, object$x_scale_params$center, "-")
    scaled_features <- sweep(scaled_features, 2, object$x_scale_params$scale, "/")

    # Reconstruct X_new
    X_new <- cbind("(Intercept)" = 1, scaled_features)
  }

  # --- FIX: Handle subscript out of bounds error ---
  # Find columns present in training (object$X) but missing in new data (X_new)
  missing_cols <- setdiff(colnames(object$X), colnames(X_new))
  if (length(missing_cols) > 0) {
    # Add missing columns filled with zeros. This is essential for factors that
    # might be absent in the new data but present in the training data.
    add <- matrix(0, nrow = nrow(X_new), ncol = length(missing_cols))
    colnames(add) <- missing_cols
    X_new <- cbind(X_new, add)
  }

  # Final step: Reorder and subset X_new to exactly match the columns and order of object$X.
  # This line will now safely execute because all required columns are present.
  X_new <- X_new[, colnames(object$X), drop = FALSE]

  # Compute predictions (these are for the centered y)
  y_hat_centered <- as.vector(X_new %*% object$beta_ridge)

  # Bug Fix 1: UN-CENTER the predictions before returning (essential for accurate results)
  return(y_hat_centered + object$y_center_mean)
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


