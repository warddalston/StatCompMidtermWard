#' Get the input of a BMA object
#' 
#' Returns the matrix of covariates and the vector of outcome values used in a BMA analysis.
#'
#' @param object An object of class BMA  
#' 
#' @details An object of the class `BMA' has the following slots:
#' \itemize{
#' \item \code{coefficients} A matrix of the regression coefficients created by regressions of all possible combinations of the covaraites in \code{X} on \code{y}
#' \item \code{R2} A vector of the R^2 value from each model
#' \item \code{PostMO} A vector of the posterior model odds for each model. 
#' \item \code{PostEB} A vector of posterior expected values for each of the coefficients
#' \item \code{PostBetaNonZero} A vector of posterior probabalities that each coefficinet is non-zero
#' \item \code{X} The first input, a matrix of covariate values (after standardization)
#' \item \code{y} the second input, a numeric vector of the length as the number of rows of \code{X} (also after standardization) 
#' \item \code{g} The third object input, which is the hyper prior used for calculating model odds and expected values of betas.
#' }
#' @note This function simply returns the input of the analysis, and none of the estimated coefficients, odds, or probabilities. 
#' @author Dalston G. Ward \email{ward.dalston@@gmail.com}
#' @seealso \code{\link{getBMAcoefs}}
#' @seealso \code{\link{getBMAout}}
#' @seealso \code{\link{BMA-class}}
#' @aliases getBMAinput,BMA-method 
#' @rdname getBMAinput
#' @export
setGeneric("getBMAinput", #This function is used to get the input of a BMA object out. 
           function(object)  {
             standardGeneric("getBMAinput")
           }
)

#' @rdname getBMAinput
#' @export
setMethod(f="getBMAinput", 
          signature="BMA",  
          definition=function(object){ 
            return(list(X=object@X,y=object@y,g=object@g))
          }
)