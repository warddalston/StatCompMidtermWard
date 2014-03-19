#############################################################
## R Midterm - Development File - Dalston Ward ##
#############################################################

## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("/Users/clockbob1/Documents/WashU 2nd Year/Applied Stats Programming/StatCompMidtermWard") #This will need to be changed to match your directory

## This is run once when the package strcuture is first created
create(path="./BMAPack", check=FALSE)

## This can be run many times as the code is updates
current.code <- as.package("BMAPack")
load_all(current.code)
document(current.code)

## Install the package
install(pkg=current.code, local=TRUE)

#see what things may be wrong...
check(current.code)
