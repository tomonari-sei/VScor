#' @title VScor.table
#' @description Variance-Stabilizable correlation for table data
#' @param x a table data
#' @param ... options for internal functions
#' @return list of Riemannian distance (with standard error), variance-stabilizable correlation and squared correlation.
#' If the size of the table is 2 by 2, the signed Riemannian distance is also returned.
#' If the option draw is TRUE, confidence intervals are plotted.
#' @export
#' @examples
#' N = matrix(c(rpois(2,10), rpois(2,5), rpois(2,10)), 2, 3, byrow=TRUE)
#' VScor.table(N)

VScor.table = function(x, ..., draw=FALSE, conf.level=0.95){
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
  VScor = 2 * sin(RD / 2) / (1 + sin(RD / 2)^2)  # Variance-stabilizable correlation
  VScor_squared = VScor^2
  if(draw){
    f = function(x) 2 * sin(x / 2) / (1 + sin(x / 2)^2)
    curve(f(x), xlim=c(0, pi), xlab="Riemannian distance", ylab="VS correlation")
    points(RD, VScor, col="red")
    segments(RD, 0, RD, VScor, lty=3)
    segments(0, VScor, RD, VScor, lty=3)
    z = qnorm((1 + conf.level) / 2) / sqrt(n)
    arrows(RD - z, 0, RD + z, 0, length=0.05, angle=90, code=3)
    arrows(0, f(RD - z), 0, f(RD + z), length=0.05, angle=90, code=3)
    b = sqrt(qchisq(conf.level, (nrow(x) - 1) * (ncol(x) - 1)) / n)
    segments(b, 0, b, 1, lty=2)
    segments(0, f(b), pi, f(b), lty=2)
  }
  if(nrow(x) == 2 && ncol(x) == 2){
    return(list(signed = s, distance = RD, distance_se = 1/sqrt(n), VScor = VScor, VScor_squared = VScor_squared))
  }else{
    return(list(distance = RD, distance_se = 1/sqrt(n), VScor = VScor, VScor_squared = VScor_squared))
  }
}

