data(iris)

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
    call = match.call()
  )
  class(ridgereg) <- "ridgereg"
  return(ridgereg)
}

# Methods
print.ridgereg <- function(x){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$beta_ridge)
  invisible(x)
}

predict.ridgereg <- function(x){
  return(x$y_hat)
}

coef.ridgereg <- function(x, ...){
  return(x$beta_ridge)
}
