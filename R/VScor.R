#' @title VScor
#' @description
#' The package VScor computes the variance-stabilizable (VS) correlation coefficient of table data or output of glm.
#' The method is based on the Riemannian distance from the independence model.
#' The Riemannian distance has the standard error 1/sqrt(n), where n is the sample size.
#' The VS correlation is a transformation of the Riemannian distance that coincides with the Pearson correlation coefficient when the population is bivariate Gaussian.
#' If the input is a 2 by 2 table, VScor also returns a signed Riemannian distance.
#'
#' The function VScor() is a wrapper of VScor.glm and VScor.table
#' @param object An output of glm
#' @param ... options for internal functions
#' @return output of specific functions
#' @export
#' @examples
#' x = as.table(matrix(c(289, 23, 7, 37), 2, 2))
#' VScor(x)
#'
#' glm_1 = glm(dist ~ speed, data=cars)
#' VScor(glm_1)

VScor = function(object, ...){
  if(any(class(object)=="glm")){
    return(VScor.glm(object, ...))
  }
  if(any(class(object)=="table")){
    return(VScor.table(object, ...))
  }
  stop("class should be glm or table.")
}
