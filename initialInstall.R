#initialInstall.R
install.packages(c("Rcpp", "devtools"), dependencies=TRUE)
require(devtools)
library(Rcpp)
install_github("awalker89/openxlsx")