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

#code to test 
library(BMAPack)
library(doMC)
library(multicore)
library(foreach)

#set up parallel
registerDoMC(cores=4)
myX <- matrix(rnorm(1000),ncol=10)
myY <- myX[,1]*2+myX[,3]*10+rnorm(100)
toy <- fitBMA(myX,myY,g=3,parallel=TRUE)
summary(toy)
plot(toy)
getBMAinput(toy)
getBMAout(toy)
getBMAcoefs(toy)
