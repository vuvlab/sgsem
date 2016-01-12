## Install necessary packages they are not installed already.

list.of.packages <- c("devtools", "roxygen2", "knitr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages, new.packages)

## If one of the above package is not available by default, you need to install them manually. 1. Download it from github or bitbucket, and run the following line.
## install.packages("roxygen2_4.0.0.tgz", repos = NULL)

## Load the libraries
library("devtools", "roxygen2", "knitr")

## Create the package directory skeleton
# create("package")

## Add all documented functions in "./semi-gsem/R" MANUALLY.
## (optional) create a folder "data" under "./semi-gsem/" then add your data file "name.RData" in there to be used in examples.
setwd("/Users/yifanxu/gdrive/aarepos/sgsem/packages/sgsem")

## Run the following to generate documentation. NOTE: One function per R file. Each function needs to be documented according to roxygen2 standard. It is not hard to find an example. 
roxygen2::roxygenise("package")

devtools::document("package")

## exit R. Build a package in Linux command line "R CMD build rnmf"
## OR run it in R as follows
system("R CMD build package")
## The package is now a tar ball.
system("R CMD check sgSEM_0.4.2.tar.gz")
## Install as follows:
install.packages("sgSEM_0.4.2.tar.gz", repos = NULL, type = "source")

## load
library(sgSEM)

## Test
?sgSEM
?sgSEMp1
example(sgSEMp1)
?sgSEMp2
example(sgSEMp2)
