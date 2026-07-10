To install the package,  
> devtools::install_github("tomonari-sei/VScor")

Example:  
> library(VScor)  
> glm_1 = glm(dist ~ speed, data = cars)  
> VScor(glm_1)
