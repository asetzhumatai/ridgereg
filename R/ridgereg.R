data(iris)

ridgereg <- function(formula, data, lambda){
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  tt <- stats::terms(mf)
  X  <- stats::model.matrix(tt, mf)
  y  <- as.numeric(stats::model.response(mf))

  # Normalize numeric columns (exclude intercept)
  X_no_intercept <- X[, -1, drop = FALSE]
  cols_to_scale <- apply(X_no_intercept, 2, function(col) length(unique(col)) > 2)

  X_no_intercept[, cols_to_scale] <- apply(X_no_intercept[, cols_to_scale, drop = FALSE], 2,
                                           function(x) (x - mean(x)) / sqrt(var(x)))

  X_norm <- cbind("(Intercept)" = 1, X_no_intercept)
  X <- X_norm

  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  I <- diag(ncol(X))
  I[1, 1] <- 0

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

coef.ridgereg <- function(x){
  return(x$beta_ridge)
}
