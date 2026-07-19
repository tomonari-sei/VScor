#' @title VScor.table
#' @description Variance-Stabilizable correlation for table data
#' @param x a table data
#' @param ... options for internal functions
#' @return list of Riemannian distance (with standard error), variance-stabilizable correlation and squared correlation
#' @export
#' @examples
#' N = matrix(c(rpois(2,10), rpois(2,5), rpois(2,10)), 2, 3, byrow=TRUE)
#' VScor.table(N)

VScor.table = function(x, ...){
  n = sum(x)
  phat = x / n
  if(nrow(x) == 2 && ncol(x) == 2){ # 2 by 2 table
    a = phat[1,1]; b = phat[1,2]; c = phat[2,1]; d = phat[2,2]
    delta = 2 * (sqrt(a * d) - sqrt(b * c))
    delta = max(-1, min(1, delta))  # avoiding numerical issues
    s = asin(delta)  # signed Riemannian distance (variance-stabilizing transformation)
    RD = abs(s)      # Riemannian distance
  }else{
    lambda = min(max(0, svd(sqrt(phat), 1, 1)$d[1]), 1)
    RD = asin(sqrt(1-lambda^2)) * 2  # Riemannian distance
  }
  z = 2 * atanh(sin(RD / 2))
  VScor = tanh(z)  # Variance-stabilizable correlation
  VScor_squared = VScor^2
  if(nrow(x) == 2 && ncol(x) == 2){
    return(list(signed = s, distance = RD, distance_se = 1/sqrt(n), VScor = VScor, VScor_squared = VScor_squared))
  }else{
    return(list(distance = RD, distance_se = 1/sqrt(n), VScor = VScor, VScor_squared = VScor_squared))
  }
}
