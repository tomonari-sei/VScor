The R package VScor computes the variance-stabilized (VS) association coefficient of table data or output of glm.
The VS correlation is a transformation of the VS association coefficient that coincides with the Pearson correlation coefficient when the population is bivariate Gaussian.
If the input is a 2 by 2 table, VScor also returns a signed association coefficient.  
  
To install the package,  
> devtools::install_github("tomonari-sei/VScor")

Example:  
> library(VScor)  
> x = as.table(matrix(c(289, 23, 7, 37), 2, 2))  
> VScor(x)  
  
> glm_1 = glm(dist ~ speed, data = cars)  
> VScor(glm_1)  
