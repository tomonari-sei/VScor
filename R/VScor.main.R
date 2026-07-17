#' @title VScor.main
#' @description main function for Variance-Stabilizable correlation
#' @param A a square matrix
#' @param ... options for internal functions
#' @return the singular value of square root of scaled matrix
#' @export
#' @examples
#' A = matrix(runif(10*10), 10, 10)
#' VScor.main(A)

VScor.main = function(A, ...){
  # A = exp(H) is a matrix of size n by n, where H specifies dependence.
  B = SK2(A, ...)
  C = sqrt(B)
  PF2(C, ...)
}
