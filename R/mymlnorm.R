#' Multidimenstional simulation
#'
#' @description Poisson Central Limit Thereom function from lab 8
#'
#' @param x vector of data
#' @param mu mean of the sample
#' @param sig standard deviation of the sample
#' @param length iterations
#' @param color Color of plot
#'
#' @return 3 panel graphs for the poisson distribution
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot curve hist layout lines polygon contour
#' @importFrom stats density dnorm dpois pnorm rpois qt sd
#'
#' @examples
#' \dontrun{y=c(10,12,13,15,12,11,10); mymlnorm(y)}
#'
mymlnorm=function(x,mu=seq(10,15,length=1000),sig=seq(0.1,4,length=1000),color='green'){

  #number of values in the mean
  nmu=length(mu)
  nsig=length(sig)
  n=length(x)
  zz=c()

  #log likelihood of
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))
  for(j in 1:nsig){
    z=outer(x,mu,lfun,p=sig[j]) # z a matrix

    #sum of the means
    y=apply(z,2,sum)

    #creates a vector of the sum of means
    zz=cbind(zz,y)
    }

  #find the maximum value and coordinates of the max value
  maxl=max(exp(zz))
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)

  #contour the values
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")),col = color)
  mlx=round(mean(x),2)
  mly=round(sqrt((n-1)/n)*sd(x),2)

  #plot the axes
  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

  #estimate the coordinate values and plots it
  muest=mu[coord[1]]
  sigest=sig[coord[2]]
  abline(v=muest, h=sigest)
  obj <- list(x=x,coord=coord,maxl=maxl)
}
