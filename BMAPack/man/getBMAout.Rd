\name{getBMAout}
\alias{getBMAout}
\alias{getBMAout,BMA-method}
\title{Get the inferential information BMA object}
\arguments{
  \item{object}{An object of class BMA}
}
\description{
  Returns the vectors of information about model fit and
  expected coefficent values from a BMA object
}
\details{
  An object of the class `BMA' has the following slots:
  \itemize{ \item \code{coefficients} A matrix of the
  regression coefficients created by regressions of all
  possible combinations of the covaraites in \code{X} on
  \code{y} \item \code{R2} A vector of the R^2 value from
  each model \item \code{PostMO} A vector of the posterior
  model odds for each model. \item \code{PostEB} A vector
  of posterior expected values for each of the coefficients
  \item \code{PostBetaNonZero} A vector of posterior
  probabalities that each coefficinet is non-zero \item
  \code{X} The first input, a matrix of covariate values
  (after standardization) \item \code{y} the second input,
  a numeric vector of the length as the number of rows of
  \code{X} (also after standardization) \item \code{g} The
  third object input, which is the hyper prior used for
  calculating model odds and expected values of betas. }
}
\note{
  This function returns the four vectors which are helpful
  in making inferences about the effects of covariates on
  the outcome variable.
}
\author{
  Dalston G. Ward \email{ward.dalston@gmail.com}
}
\seealso{
  \code{\link{getBMAinput}}

  \code{\link{getBMAcoefs}}

  \code{\link{BMA-class}}
}

