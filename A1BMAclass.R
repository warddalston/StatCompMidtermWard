#' A Bayesian Model Averaging object 
#' 
#' Objects of class \code{BMA} are created by the \code{\link{fitBMA}} function
#'
#' 
#' An object of the class `BMA' has the following slots:
#' \itemize{
#' \item \code{coefficients} A matrix of the regression coefficients created by regressions of all possible combinations of the covaraites in \code{X} on \code{y}
#' \item \code{R2} A vector of the R^2 value from each model
#' \item \code{PostMO} A vector of the posterior model odds for each model. 
#' \item \code{PostEB} A vector of posterior expected values for each of the coefficients
#' \item \code{PostBetaNonZero} A vector of posterior probabalities that each coefficinet is non-zero
#' \item \code{X} The first input, a matrix of covariate values (after standardization)
#' \item \code{y} the second input, a numeric vector of the length as the number of rows of \code{X} (also after standardization) 
#' }
#' @author Dalston G. Ward \email{ward.dalston@@gmail.com}
#' @seealso \code{\link{getBMAinput}}
#' @seealso \code{\link{getBMAcoefs}}
#' @seealso \code{\link{getBMAout}}
#' @aliases BMA-class initialize,BMA-method getBMA,BMA-method 
#' @rdname BMA
#' @export
setClass(Class="BMA", 
         slots = list(
           coefficients = "matrix",
           R2 = "numeric",
           PostMO = "numeric",
           PostEB = "numeric",
           PostBetaNonZero = "numeric",
           X = "matrix",
           y = "numeric"
         ),
         prototype = prototype(
           coefficients = matrix(),
           R2 = numeric(),
           PostMO = numeric(),
           PostEB = numeric(),
           PostBetaNonZero = numeric(),
           X = matrix(),
           y = numeric()
         )
)

#' @rdname BMA
#' @export
setMethod("initialize", "BMA", 
          function(.Object, coefficients=matrix(0,nrow=1,ncol=1), R2=numeric(0),PostMO=numeric(0),PostEB=numeric(0),PostBetaNonZero=numeric(0),X=matrix(0,nrow=1,ncol=1),y=numeric(0)){ #these are the default values for an object of class BMA.
            
            #below are three basic checks to make sure that the user is giving us the proper information for a BMA analysis. 
            if(any(is.na(X))){
              stop("The function does not accept covariate matrices with missing values")
            }
            if(any(is.na(y))){
              stop("The function does not accept outcome vectors with missing values")
            }
            if(!length(y)==0 & length(y)!=nrow(X)){ #the first part of this logical makes the default BMA object not trigger this if loop.  
              stop("The length of y and the number of rows of X must be equal")
            }
            .Object@coefficients <- coefficients
            .Object@R2 <- R2
            .Object@PostMO <- PostMO
            .Object@PostEB <- PostEB
            .Object@PostBetaNonZero <- PostBetaNonZero
            .Object@X <- X
            .Object@y <- y
            .Object
          }
) 

