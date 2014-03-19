\docType{methods}
\name{plot,BMA-method}
\alias{plot}
\alias{plot,BMA-method}
\title{Plotting BMA objects}
\usage{
plot(x,y,...)
}
\arguments{
  \item{x}{an object of class `BMA'}

  \item{y}{NOT USED IN THIS METHOD}

  \item{...}{arguements passed on to other functions.}
}
\description{
Plots the distributions of coefficient estimates from BMA
objects.
}
\details{
The plot method for objects of class `BMA' plots the
distribution of coefficient estimates for every
coefficient.  It plots these on graphical devices of
dimensions 3 by 3; therefore, there are 9 coefficient
distribution plots per graphical pages.  it also plots as a
vertical line at zero the probability that the coefficient
is non-zero.  For coefficient estimates which do not have
any density at or below zero, the limits of the x-axis are
adjusted to include zero.  Each plot is titled according to
the variable name.
}
\examples{
set.seed(1801)
myX <- matrix(rpois(n=60,lambda=15),ncol=4)
myY <- sample(1:100,15,replace=TRUE)
myBMA <- fitBMA(myX, myY)
plot(myBMA)
}
\author{
Dalston G. Ward \email{ward.dalston@gmail.com}
}
\seealso{
\code{\link{BMA-class}}

\code{\link{summary,BMA-method}}

\code{\link{fitBMA}}
}

