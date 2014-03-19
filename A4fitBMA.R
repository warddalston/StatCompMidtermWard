#' Perform a BMA analysis
#'
#' Regresses all possible combinations of a covariates on an outcome variable and then calculates several odds and probablities for use in making inferences about the effects of covariates on the outcome variable. 
#'
#' @param X A numeric matrix object of covariates with no missing values 
#' @param y A numeric object with the same number of elements as \code{X} has rows.
#'
#' @return An object of class BMA containing
#' \itemize{
#'  \item{coefficients}{ A n by 2^n matrix of coefficient values, where n is the number of columns in \code{X}.  Each column in this object represeents the output of a single model, each row represents a given variable in \code{X}.}
#'  \item{R2}{ A numeric vector of length 2^n of R^2 values, where n is the number of columns in \code{X}.}
#' \item{PostMO}{ A vector of length 2^n of the posterior model odds for each model.} 
#' \item{PostEB}{ A vector of length n of posterior expected values for each of the coefficients.}
#' \item{PostBetaNonZero}{ A vector of length n of posterior probabalities that the true value each coefficinet is non-zero}
#'  \item{X}{ The first object input, with each variable standardized by subtracting by the mean and dividing by the standard deviation} 
#'  \item{y}{ The second object input, with values standardized by subtracting by the mean and dividing by the standard deviation}
#'  }
#'  
#' @note The input objects are standardized in order to make the inclusion of an intercept unneccessary.  
#' @examples
#' 
 set.seed(1801)
 X <- matrix(rpois(n=150,lambda=15),ncol=10)
 y <- sample(1:100,15,replace=TRUE) 
#' fitRegCombin(myX, myY)
#' @author Dalston G. Ward \email{ward.dalston@@gmail.com}
#' @rdname fitBMA
#' @aliases fitBMA,ANY-method
#' @export
setGeneric(name="fitBMA",
           def=function(X,y)
           {standardGeneric("fitBMA")}
)

#' @export
setMethod(f="fitBMA",
          definition=function(X, y, g=3, parallel=TRUE){
            
            #Start by standardizing the input objects
            standX <- scale(X) #the scale function subtracts the mean from each column of X and then divides by the standard deviation.  Using system time, I found that it was marginally faster than using apply.  
            standY <- as.numeric(scale(y)) #the output of scale is a matrix.  I coerce it back into a numeric.  
            
            #The method first creates a logical matrix specifying which variables are in each of the combinations.  There are 2^n possible combinations of covariates. This can be
            selectorList <- alply(.data=0:ncol(X),.margins=1, .fun=function(x){ 
              apply(combn(1:ncol(X),x), 2, function(z){
                CurrentCombn <- logical(length=ncol(X))
                CurrentCombn[z] <- TRUE
                return(CurrentCombn)
              }) #close the apply
            },.parallel=parallel) #close the alply
            selectorMatrix <- matrix(unlist(selectorList),nrow=ncol(X)) #turn the list output in a matrix.  
            
            
            #The next section creates the objects that will be in the output.  They have informative names and are sized based on parameter inputs.   
            Coefficients <- matrix(nrow=ncol(X),ncol=2^ncol(X))
            R.squareds <- vector(length=2^ncol(X))
            Post.MO <- vector(length=2^ncol(X))
            Post.EB <- vector(length=ncol(X))
            Post.BetaNonZero <- vector(length=ncol(X))
            #colnames(Coefficients) <- names(R.squareds) <- names(Post.MO) <- apply(selectorMatrix,2,function(x){
            #  gsub("0","Int",paste("Vars:",paste(which(x==TRUE)-1,collapse=",")))
            #})
            #rownames(Coefficients) <- c("Int",paste("Variable", 1:(ncol(X)-1), sep=" "))
            
            #This section of this function does the actual regression fitting.  Using a_ply it fits a regression for each column of the selectorMatrix where the covariates are the based on the "TRUE" values for a given column.  Then, it writes the coefficent values and R^2 value to the appropriate element of the output objects using "<<-" to force writing these to the first variable matching the specified name in a parent environment.  This is basically R-cheating.  
           oops <-  alply(.data=2:ncol(selectorMatrix),.margins=1,.fun=function(z){
              mod <- lm(standY~standX[,selectorMatrix[,z]]-1)
              Coefficients[selectorMatrix[,z],z] <- mod[[1]] 
              R.squareds[z] <- summary(mod)[["r.squared"]]
              return(list(Coefficients=Coefficients[,z],R.Squareds=R.squareds))
            },.parallel=TRUE)
           
           l_ply(.data=1:length(oops),.fun=function(x){
             Coefficients[,x+1] <<- oops[[x]][[1]]
           })
            #Outside of the apply, I put in the intercept coefficient for the null model in the first column/slot of each item.
            Coefficients <- rbind(Coefficients,c(lm(standY~1)[[1]],rep(NA,ncol(Coefficients)-1))) 
            R.squareds[1] <- summary(lm(standY~1))[["r.squared"]]
            
            
            
            #it returns the output generated above as the input of the slots of an S4 object of "RegCombin" class
            return(new("BMA", coefficients=Coefficients,R2=R.squareds,X=X[,2:ncol(X)],y=y))
          }
)