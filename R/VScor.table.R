#' @title VScor.table
#' @description Variance-Stabilizable association for table data
#' @param x a table data
#' @param ... options for internal functions
#' @return list of variance-stabilized association (with standard error), correlation and squared correlation
#' @export
#' @examples
#' N = matrix(c(rpois(2,10), rpois(2,5), rpois(2,10)), 2, 3, byrow=TRUE)
#' VScor.table(N)

VScor.table = function(x, ...){
  if(nrow(x) == 2 && ncol(x) == 2){ # 2 by 2 table
    n = sum(x)
    phat = x / n
    a = phat[1,1]; b = phat[1,2]; c = phat[2,1]; d = phat[2,2]
    delta = 2 * (sqrt(a * d) - sqrt(b * c))
    s = asin(delta) / pi  # signed distance divided by pi
    assoc = max(0, min(abs(s), 1))  # Variance-stabilized association coefficient
    z = 2 * atanh(pi/2*sin(assoc))
    VScor = tanh(z)  # Variance-stabilizable correlation
    VScor_squared = VScor^2
    return(list(signed = s, assoc = assoc, VScor = VScor, VScor_squared = VScor_squared))
  }
  n = sum(x)
  phat = x / n
  lambda = min(max(0, svd(sqrt(phat), 1, 1)$d[1]), 1)
  assoc = asin(sqrt(1-lambda^2)) * 2 / pi  # Variance-stabilized association coefficient
  z = 2 * atanh(sqrt(1-lambda^2))
  VScor = tanh(z)  # Variance-stabilizable correlation
  VScor_squared = VScor^2
  list(assoc = assoc, assoc_se = 1/sqrt(n)/pi, VScor = VScor, VScor_squared = VScor_squared)
}
