#' Get the coefficient matrix of a BMA object
#' 
#' Returns the coefficinet matrix used in a BMA analysis.
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
#' }
#' @note This function simply returns the coefficient matrix of a BMA analysis.  WARNING: This object will often be very large: its dimensions will be the number of columns in \code{X} by 2^\code{X}. 
#' @author Dalston G. Ward \email{ward.dalston@@gmail.com}
#' @seealso \code{\link{getBMAinput}}
#' @seealso \code{\link{getBMAout}}
#' @seealso \code{\link{BMA}}
#' @aliases BMA-method getBMAcoefs,BMA-method 
#' @rdname getBMAcoefs
#' @export
setGeneric("getBMAcoefs", #This function is used to get the coefficient matrix of a BMA object. 
           function(object)  {
             standardGeneric("getBMAcoefs")
           }
)

#' @rdname getBMAcoefs
#' @export
setMethod(f="getBMAcoefs", 
          signature="BMA",  
          definition=function(object){ 
            return(object@coefficients)
          }
)