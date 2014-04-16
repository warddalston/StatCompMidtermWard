\name{fitBMA}
\alias{fitBMA}
\alias{fitBMA,ANY-method}
\title{Perform a BMA analysis}
\arguments{
  \item{X}{A numeric matrix object of covariates with no
  missing values}

  \item{y}{A numeric object with the same number of
  elements as \code{X} has rows.}

  \item{g}{A scalar to represent a hyper prior.  Defaults
  to 3.  An alternative could be 4.}

  \item{parallel}{A logical determining whether or not to
  run in parallel.  The user is responible for initializing
  his/her own instances.  Defaults to FALSE.}
}
\value{
  An object of class BMA containing \itemize{
  \item{coefficients}{ A n by 2^n matrix of coefficient
  values, where n is the number of columns in \code{X}.
  Each column in this object represeents the output of a
  single model, each row represents a given variable in
  \code{X}.} \item{R2}{ A numeric vector of length 2^n of
  R^2 values, where n is the number of columns in
  \code{X}.} \item{PostMO}{ A vector of length 2^n of the
  posterior model odds for each model.} \item{PostEB}{ A
  vector of length n of posterior expected values for each
  of the coefficients.} \item{PostBetaNonZero}{ A vector of
  length n of posterior probabalities that the true value
  each coefficinet is non-zero} \item{X}{ The first object
  input, with each variable standardized by subtracting by
  the mean and dividing by the standard deviation}
  \item{y}{ The second object input, with values
  standardized by subtracting by the mean and dividing by
  the standard deviation} \item{g}{ The third object input,
  which is the hyper prior used for calculating model odds
  and expected values of betas.} }
}
\description{
  Regresses all possible combinations of a covariates on an
  outcome variable and then calculates several odds and
  probablities for use in making inferences about the
  effects of covariates on the outcome variable.
}
\note{
  The input objects are standardized in order to make the
  inclusion of an intercept unneccessary. This function can
  take a VERY VERY long time to run as the number of
  covariates increases.  With 10 covariates, it takes about
  a second to run in parallel with 4 cores.  With 14
  covariates, it takes about 45 seconds.  The time to run
  the funciton increases RAPIDLY with each additional
  covariate included in X.
}
\examples{
set.seed(1801)
myX <- matrix(rpois(n=150,lambda=15),ncol=10)
myY <- sample(1:100,15,replace=TRUE)
fitBMA(myX, myY,g=3,parallel=FALSE)
}
\author{
  Dalston G. Ward \email{ward.dalston@gmail.com}
}
\seealso{
  \code{\link{BMA-class}}

  \code{\link{plot,BMA-method}}

  \code{\link{summary,BMA-method}}

  \code{\link{getBMAinput}}

  \code{\link{getBMAcoefs}}

  \code{\link{getBMAout}}
}

