#' Summary method for class "BMA"
#' 
#' Invisibly creates a summary of important information about the coefficients in a BMA object, and prints this summary.  
#'
#' @param object An object of class BMA  
#' 
 #' @return A list containing the following elements
#' \itemize{
#'  \item{MetaData}{ A length 3 numeric vector containing the number of observations in the BMA analysis, the number of regressions ran, and the value of g, the hyper prior.}
#'  \item{ExpectedBetas}{ A numeric vector with the expected values for the coefficients}
#' \item{BetaNonZeroProbs}{ A numeric vector with the probability of being non-zero for each coefficient.} 
#'  }
#' 
#' @note This function prints to the console some meta deta about a BMA analysis, and then the expected value and probability of being non zero for each coefficient.  It also silently returns this same information as a list.
#' @author Dalston G. Ward \email{ward.dalston@@gmail.com}
#' @seealso \code{\link{fitBMA}}
#' @seealso \code{\link{getBMAinput}}
#' @seealso \code{\link{getBMAcoefs}}
#' @seealso \code{\link{BMA}}
#' @aliases ANY
#' @rdname summaryBMA
#' @export
setMethod(f="summary",
          signature="BMA",
          definition=function(object,...){
            cat("Summary of", substitute(object) , "\n")
            cat("****************************** \n")
            cat("N observations:",length(object@y),"\n")
            cat("N regressions:", ncol(object@coefficients),"\n")
            cat("g hyper-prior:", object@g,"\n")
            cat("****************************** \n")
            cat(paste(format(c("Variable",rownames(object@coefficients)),width=10,justify="left"),format(c("E(B)",round(object@PostEB,3)),width=8,justify="right"), format(c("Pr(B!=0)",round(object@PostBetaNonZero,3)),width=10,justify="right"),"\n"),sep="")
            cat("****************************** \n")
            MetaData <- c(length(object@y),ncol(object@coefficients),object@g)
            names(MetaData) <- c("N Observations","N Regressions","g hyper-prior")
            invisible(list(MetaData=MetaData,ExpectedBetas=object@PostEB,BetaNonZeroProbs=object@PostBetaNonZero))
          }
)
