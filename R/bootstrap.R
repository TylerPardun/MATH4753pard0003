#' bootstrap function
#'
#' @description Function from lab 9 that bootstraps samples and calculates confidence intervals
#'
#' @param iter number of iterations performed
#' @param x sample size
#' @param fun Type of calculation performed
#' @param alpha Confidence interval wanted
#'
#' @return Plot of the bootstrapped sample with labled bounds of the specified confidence interval
#' @export
#'
#' @importFrom graphics abline segments text
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{set.seed(68); sam=rnorm(20,mean=10,sd=4); myboot2(x=sam,col="yellow")}
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05){

  #definnes sample size
  n=length(x)

  #creates samples and calculates the confidence inntervals of them
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))

  #creates a historgam of densities not frequencies
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""))

  #matrix organized by row containinng the data
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  # function to make poinint estimates and add the line segments to the graph
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=1.5)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=1.5)

  # plots the estimate half way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=1.5)

  #returns confidence interval, function, and sample
  return(list(ci=ci,fun=fun,x=x))
}
