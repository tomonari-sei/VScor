#' @title SK2
#' @description Sinkhorn-Knopp algorithm for square matrix
#' @param A a square matrix
#' @param eps tolerance
#' @param max_iter integer, maximum iteration
#' @return a matrix with uniform marginals
#' @export
#' @examples
#' A = matrix(runif(10*10), 10, 10)
#' SK2(A)

SK2 = function(A, eps=1e-10, max_iter=1e4){
  n = nrow(A)
  if(ncol(A) != n) stop("not square matrix.")
  d1 = d2 = rep(1/n, n) # initial value
  err = Inf
  iter = 0
  while(err > eps && iter < max_iter){
    d1_pre = d1
    d2 = 1 / n / c(t(A) %*% d1)
    d1 = 1 / n / c(A %*% d2)
    d1 = d1 / sum(d1)  # normalize
    err = max(abs(d1_pre - d1))
    iter = iter + 1
    # if(is.nan(err)) browser()
  }
  if(iter >= max_iter) stop("not converged.")
  diag(d1) %*% A %*% diag(d2)
}
