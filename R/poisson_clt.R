#' Multidimenstional simulation
#'
#' @description Poisson Central Limit Thereom function from lab 8
#'
#' @param iter number of iterations
#' @param n number of trials
#' @param lambda Parameter
#'
#' @return 3 panel graphs for the poisson distribution
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot curve hist layout lines polygon
#' @importFrom stats density dnorm dpois pnorm rpois
#'
#' @examples
#' \dontrun{mycltp(n = 2,iter = 10000,lambda=4)}

mycltp=function(n = 10,iter = 10000,lambda=10){
  #random poisson samples
  y=rpois(n*iter,lambda=lambda)

  #put the data inn a matrix
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)

  #take the mean of the columns
  w=apply(data,2,mean)

  #store the values that will be put into the histogram
  param=hist(w,plot=FALSE)

  #find the maxium density
  ymax=max(param$density)

  #add 10% of the maximum to itself to account for variation
  ymax=1.1*ymax

  #Make a suitable layout for graphing
  layout(matrix(c(1,1,2,3),nrow=2,ncol=2, byrow=TRUE))

  #plot histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
       main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
       xlab="Sample mean")

  #add a sample density curve and plot
  lines(density(w),col="Blue",lwd=3)

  #add the theoretical normal curve
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3)

  #store the values to be in the barplot
  barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )

  #set the bounds of the barplot
  x=0:max(y)

  #creats a barplot of the means
  plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
       main="Probability function for Poisson", ylab="Probability",xlab="y")
}
