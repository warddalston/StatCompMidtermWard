\docType{class}
\name{BMA-class}
\alias{BMA-class}
\alias{initialize,BMA-method}
\title{A Bayesian Model Averaging object}
\usage{
\S4method{initialize}{BMA}(.Object, coefficients = matrix(0, nrow = 1, ncol =
  1), R2 = numeric(0), PostMO = numeric(0), PostEB = numeric(0),
  PostBetaNonZero = numeric(0), X = matrix(0, nrow = 1, ncol = 1),
  y = numeric(0), g = numeric(0))
}
\arguments{
  \item{coefficients}{the default coefficient matrix for a
  BMA class object}

  \item{R2}{the default vector of R^2 values for a BMA
  class object}

  \item{PostMO}{the default vector of posterior model odds
  for a BMA class object}

  \item{PostEB}{the default posterior expected coefficient
  values for a BMA class object}

  \item{PostBetaNonZero}{the default vector of posterior
  probabilities of coefficients not being zero for a BMA
  class object}

  \item{X}{the default covariate matrix for a BMA class
  object}

  \item{y}{the default outcome vector for a BMA class
  object}

  \item{g}{the default hyper-prior value for a BMA class
  object}

  \item{.Object}{ An object: see the Details section.}
}
\description{
Objects of class \code{BMA} are created by the
\code{\link{fitBMA}} function
}
\details{
An object of the class `BMA' has the following slots:
\itemize{ \item \code{coefficients} A matrix of the
regression coefficients created by regressions of all
possible combinations of the covaraites in \code{X} on
\code{y} \item \code{R2} A vector of the R^2 value from
each model \item \code{PostMO} A vector of the posterior
model odds for each model. \item \code{PostEB} A vector of
posterior expected values for each of the coefficients
\item \code{PostBetaNonZero} A vector of posterior
probabalities that each coefficinet is non-zero \item
\code{X} The first input, a matrix of covariate values
(after standardization) \item \code{y} the second input, a
numeric vector of the length as the number of rows of
\code{X} (also after standardization) \item \code{g} The
third object input, which is the hyper prior used for
calculating model odds and expected values of betas. }
}
\author{
Dalston G. Ward \email{ward.dalston@gmail.com}
}
\seealso{
\code{\link{getBMAinput}}

\code{\link{getBMAcoefs}}

\code{\link{getBMAout}}
}

