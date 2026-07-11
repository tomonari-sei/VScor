To install the package,  
> devtools::install_github("tomonari-sei/VScor")

Example:  
> library(VScor)  
> x = as.table(matrix(c(289, 23, 7, 37), 2, 2))  
> VScor(x)  
  
> glm_1 = glm(dist ~ speed, data = cars)  
> VScor(glm_1)  
