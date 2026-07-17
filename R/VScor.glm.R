#' @title VScor.glm
#' @description Variance-Stabilizable correlation for glm outputs
#' @param object An output of glm
#' @param ... options for internal functions
#' @return a list of Riemannian distance (with standard error), correlation and squared correlation
#' @export
#' @examples
#' glm_1 = glm(dist ~ speed, data=cars)
#' VScor.glm(glm_1)

VScor.glm = function(object, ...){
  # object is assumed to be an output of glm with canonical link
  th = object$linear.predictors
  if(object$family$family == "gaussian"){
    th = th / summary(object)$dispersion
  }
  y = object$y
  n = length(y)
  A = matrix(0, n, n)
  y_th = outer(y, th)
  y_th = y_th - max(y_th)  # subtract the maximum for numerical stability
  A = exp(y_th)
  lambda = VScor.main(A, ...)  # the largest singular value of sqrt of density ratio
  RD = asin(sqrt(1-lambda^2)) * 2  # Variance-stabilizing transformation
  z = 2 * atanh(sqrt(1-lambda^2))
  VScor = tanh(z)  # Variance-stabilizable correlation
  VScor_squared = VScor^2
  list(distance = RD, distance_se = 1/sqrt(n), VScor = VScor, VScor_squared = VScor_squared)
}
