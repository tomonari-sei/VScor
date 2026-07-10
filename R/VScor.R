#' @title VScor
#' @description a wrapper of Variance-Stabilizable association
#' @param object An output of glm
#' @param ... options for internal functions
#' @return a list of distance, correlation and squared correlation
#' @export
#' @examples
#' glm_1 = glm(dist ~ speed, data=cars)
#' VScor(glm_1)

VScor = function(object, ...){
  if(any(class(object)=="glm")){
    VScor.glm(object, ...)
  }
}
