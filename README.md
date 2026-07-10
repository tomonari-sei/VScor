Install the package by
> devtools::install_github("tomonari-sei/VScor")
on R.

Example:
> library(VScor)
> glm_1 = glm(dist ~ speed, data = cars)
> VScor(glm_1)
