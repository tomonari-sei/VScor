#' @title PF2
#' @description Finding the largest singular value of a positive square matrix
#' @param Q a square matrix
#' @param eps tolerance
#' @param max_iter integer, maximum iteration
#' @param detail logical, return singular vectors if TRUE
#' @return the largest singular value
#' @export
#' @examples
#' Q = matrix(runif(10*10), 10, 10)
#' PF2(Q)

PF2 = function(Q, eps=1e-10, max_iter=1e4, detail=FALSE){
  n = nrow(Q)
  if(ncol(Q) != n) stop("not square matrix.")
  beta = rep(1, n) / sqrt(n)
  gamma = rep(1, n) / sqrt(n)
  err = Inf
  iter = 0
  beta_1 = Q %*% gamma
  while(err > eps && iter < max_iter){
    lambda_1 = sqrt(sum(beta_1^2))
    beta = beta_1 / lambda_1
    gamma_1 = t(Q) %*% beta
    lambda = sqrt(sum(gamma_1^2))
    gamma = gamma_1 / lambda
    beta_1 = Q %*% gamma
    err = max(abs(lambda * beta - beta_1))
    iter = iter + 1
  }
  if(iter >= max_iter) stop("not converged.")
  if(detail){
    list(lambda = lambda, beta = beta, gamma = gamma)
  }else{
    lambda
  }
}
