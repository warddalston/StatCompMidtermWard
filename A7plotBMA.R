#' Plotting BMA objects
#' 
#' Plots the distributions of coefficient estimates from BMA objects.
#' 
#' @usage plot(x,...)
#'
#' @param x an object of class `BMA'
#' @param ... arguements passed on to other functions.  
#' 
#' @details The plot method for objects of class `BMA' plots the distribution of coefficient estimates for every coefficient.  It plots these on graphical devices of dimensions 3 by 3; therefore, there are 9 coefficient distribution plots per graphical pages.  it also plots as a vertical line at zero the probability that the coefficient is non-zero.  For coefficient estimates which do not have any density at or below zero, the limits of the x-axis are adjusted to include zero.  Each plot is titled according to the variable name.      
#'
#' @examples
#' 
#' set.seed(1801)
#' myX <- matrix(rpois(n=60,lambda=15),ncol=4)
#' myY <- sample(1:100,15,replace=TRUE) 
#' myBMA <- fitBMA(myX, myY)
#' plot(myBMA)
#' @author Dalston G. Ward \email{ward.dalston@@gmail.com}
#' @seealso \code{\link{BMA-class}}
#' @rdname plotBMA
#' @export
setMethod(f="plot",
          signature="BMA",
          definition=function(x,...){
            par(mfrow=c(3,3),mar=c(2,3,3,2))
            sapply(1:ncol(x@X),function(z){
              dens <- density(x@coefficients[z,],na.rm=TRUE)
              if(min(dens$x)>0){
                plot(dens,main=rownames(x@coefficients)[z],xlim=c(-.1,max(dens$x)),...)
              } else {
              plot(dens,main=rownames(x@coefficients)[z],...)
              }
              segments(x0=0,y0=0,x1=0,y1=x@PostBetaNonZero[z],lwd=3)
            })
            invisible(NULL)
          }
)
