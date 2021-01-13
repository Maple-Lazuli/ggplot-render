## main.R ##
## ------ ##
library(plumber)
r <- plumb("/home/ada/Desktop/functions.R")
r$run(port=8000)

