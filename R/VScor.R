#' @title VScor
#' @description a wrapper of Variance-Stabilizable association
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
