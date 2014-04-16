#' Perform a BMA analysis
#'
#' Regresses all possible combinations of a covariates on an outcome variable and then calculates several odds and probablities for use in making inferences about the effects of covariates on the outcome variable. ### This is probably too vague to be useful
#'
#' @param X A numeric matrix object of covariates with no missing values 
#' @param y A numeric object with the same number of elements as \code{X} has rows.
#' @param g A scalar to represent a hyper prior.  Defaults to 3.  An alternative could be 4.
#' @param parallel A logical determining whether or not to run in parallel.  The user is responible for initializing his/her own instances.  Defaults to FALSE. 
#'
#' @return An object of class BMA containing
#' \itemize{
#'  \item{coefficients}{ A n by 2^n matrix of coefficient values, where n is the number of columns in \code{X}.  Each column in this object represents the output of a single model, each row represents a given variable in \code{X}.}
#'  \item{R2}{ A numeric vector of length 2^n of R^2 values, where n is the number of columns in \code{X}.}
#' \item{PostMO}{ A vector of length 2^n of the posterior model odds for each model.} 
#' \item{PostEB}{ A vector of length n of posterior expected values for each of the coefficients.}
#' \item{PostBetaNonZero}{ A vector of length n of posterior probabalities that the true value each coefficinet is non-zero}
#'  \item{X}{ The first object input, with each variable standardized by subtracting by the mean and dividing by the standard deviation} 
#'  \item{y}{ The second object input, with values standardized by subtracting by the mean and dividing by the standard deviation}
#'  \item{g}{ The third object input, which is the hyper prior used for calculating model odds and expected values of betas.}
#'  }
#'  
#' @note The input objects are standardized in order to make the inclusion of an intercept unneccessary. This function can take a VERY VERY long time to run as the number of covariates increases.  With 10 covariates, it takes about a second to run in parallel with 4 cores.  With 14 covariates, it takes about 45 seconds.  The time to run the function increases RAPIDLY with each additional covariate included in X.    
#' @examples
#' 
#' set.seed(1801)
#' myX <- matrix(rpois(n=150,lambda=15),ncol=10)
#' myY <- sample(1:100,15,replace=TRUE) 
#' fitBMA(myX, myY,g=3,parallel=FALSE)
#' @author Dalston G. Ward \email{ward.dalston@@gmail.com}
#' @seealso \code{\link{BMA-class}}
#' @seealso \code{\link{plot,BMA-method}}
#' @seealso \code{\link{summary,BMA-method}}
#' @seealso \code{\link{getBMAinput}}
#' @seealso \code{\link{getBMAcoefs}}
#' @seealso \code{\link{getBMAout}}
#' @rdname fitBMA
#' @aliases fitBMA,ANY-method
#' @export
setGeneric(name="fitBMA",
           def=function(X,y,g=3,parallel=FALSE)
           {standardGeneric("fitBMA")}
)

#' @export
setMethod(f="fitBMA",
          definition=function(X, y, g=3, parallel=FALSE){
            
            #Start by standardizing the input objects
            standX <- scale(X) #the scale function subtracts the mean from each column of X and then divides by the standard deviation.  Using system time, I found that it was marginally faster than using apply.  
            standY <- as.numeric(scale(y)) #the output of scale is a matrix.  I coerce it back into a numeric.  
            
            #The method first creates a logical matrix specifying which variables are in each of the combinations.  There are 2^n possible combinations of covariates.
            selectorList <- alply(.data=0:ncol(X),.margins=1, .fun=function(x){ 
              apply(combn(1:ncol(X),x), 2, function(z){
                CurrentCombn <- logical(length=ncol(X))
                CurrentCombn[z] <- TRUE
                return(CurrentCombn)
              }) #close the apply
            },.parallel=parallel) #close the alply
            selectorMatrix <- matrix(unlist(selectorList),nrow=ncol(X)) #turn the list output in a matrix.  
## FYI: See the expand.grid() function
            
            #The next section creates the objects that will be in the output.  They have informative names and are sized based on parameter inputs.   
            
            Coefficients <- matrix(nrow=ncol(X),ncol=2^ncol(X))
            R.squareds <- vector(length=2^ncol(X))
            colnames(Coefficients) <- names(R.squareds) <- apply(selectorMatrix,2,function(x){
            paste("Vars:",paste(which(x==TRUE),collapse=","))
            })
            rownames(Coefficients) <- c(paste("Variable", 1:(ncol(X)), sep=" "))
            
            #This section of this function does the actual regression fitting.  Using alply it fits a regression for each column of the selectorMatrix where the covariates are the based on the "TRUE" values for a given column. It stores the coefficients and R^2 values from these regressions in a large "list of lists".  Then a call to l_ply using the <<- function puts all of these values together into a useful output.  
            
            Coefs <-  alply(.data=2:ncol(selectorMatrix),.margins=1,.fun=function(z){
              mod <- lm(standY~standX[,selectorMatrix[,z]]-1) #fit the regression, no intercept.
              Coefficients[selectorMatrix[,z],z] <- mod[[1]]  #store coefficients, preserving NA's where there is no coeffficinet.
              R.squareds[z] <- summary(mod)[["r.squared"]]
              return(list(Coefficients=Coefficients[,z],R.Squareds=R.squareds[z])) #return the appropriate coefficeint values and R squared. 
            },.parallel=parallel) #does parallel.  
           
           l_ply(.data=1:length(Coefs),.fun=function(x){
             Coefficients[,x+1] <<- Coefs[[x]][[1]] #these next two lines use "<<-" which force R to write to the first object which matches the name it finds in a parent environment.  Doesn't work as nicely in parallel, hence this and the code above being separate. 
             R.squareds[x+1] <<- Coefs[[x]][[2]]
           })

## This all seems a bit clunky.  Why not have two separate apply functions return an object organized correctly to then output?  
            
            #Outside of the apply, I put in the R^2 for the null model into the vector of R squared values. 
            R.squareds[1] <- summary(lm(standY~1))[["r.squared"]]
            
           #Now I move on to writing code to create the inference objects.  I begin by calculating the B[M_k:M_0] values, as described on slide 25. 
            BMkM0 <- sapply(1:ncol(Coefficients),function(x){
              (1+g)^(nrow(X)-sum(!is.na(Coefficients[1:ncol(X),x]))-1)*(1+g*(1-R.squareds[x]))^(-1*(nrow(X)-1)/2)
            }) #the sum(!is.na... piece of code counts the number of variables!
           #The Mean posterior model odds is simply this divided by the sum of all the BMkM0s
           Post.MO <- BMkM0/sum(BMkM0)
          
           #This section calculates the posterior expected value for each coefficient 
           EBkMk <- (g/(g+1))*Coefficients #this is the expected beta given the model (from slide 3)
          Post.EB <- apply(EBkMk[1:ncol(X),],1,function(x){
            missing <- which(is.na(x)) #pick out the right models by dropping all models where a given covariate is not included.
            t(Post.MO[-missing]%*%x[-missing]) #matrix multiplication performs the multiplication and summation in one step.  Nice.  
          } #close the function
          ) #close the apply 

# good.
            
           #This section calculates the posterior probability that the coefficient is non zero
          Post.NonZero <- apply(Coefficients[1:ncol(X),],1,function(x){
            sum(Post.MO[-which(is.na(x))])
          } #close the funciton.
          ) #close apply
          
            #it returns the output generated above as the input of the slots of an S4 object of "BMA class
            return(new("BMA", coefficients=Coefficients,R2=R.squareds,PostMO=Post.MO,PostEB=Post.EB,PostBetaNonZero=Post.NonZero,X=standX,y=standY,g=g))
            # good.
          }
)
