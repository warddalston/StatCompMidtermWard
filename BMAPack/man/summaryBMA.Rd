\docType{methods}
\name{summary,BMA-method}
\alias{summary}
\alias{summary,BMA-method}
\title{Summary method for class "BMA"}
\usage{
summary(object,...)
}
\arguments{
  \item{object}{An object of class BMA}

  \item{...}{arguements passed to other functions}
}
\value{
A list containing the following elements \itemize{
\item{MetaData}{ A length 4 numeric vector containing the
number of observations in the BMA analysis, the number of
regressions ran, and the value of g, the hyper prior, and
the variables included in the most likely model, along with
the model odds for that model.} \item{ExpectedBetas}{ A
numeric vector with the expected values for the
coefficients} \item{BetaNonZeroProbs}{ A numeric vector
with the probability of being non-zero for each
coefficient.} }
}
\description{
Invisibly creates a summary of important information about
the coefficients in a BMA object, and prints this summary.
}
\note{
This function prints to the console some meta deta about a
BMA analysis, and then the expected value and probability
of being non zero for each coefficient.  It also silently
returns this same information as a list.  If the most
likely mode includes output is simply "Vars:" this means
that the null model is most likely.
}
\examples{
set.seed(1801)
myX <- matrix(rpois(n=150,lambda=15),ncol=10)
myY <- sample(1:100,15,replace=TRUE)
obj <- fitBMA(myX, myY,g=3,parallel=FALSE)
summary(obj)
}
\author{
Dalston G. Ward \email{ward.dalston@gmail.com}
}
\seealso{
\code{\link{fitBMA}}

\code{\link{getBMAinput}}

\code{\link{getBMAcoefs}}

\code{\link{getBMAout}}

\code{\link{BMA-class}}

\code{\link{plot,BMA-method}}
}

