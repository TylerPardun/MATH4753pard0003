#' Bootstrap function with graph display
#'
#' @description Function from lab 12 that displayus a histogram and accepted and rejected regions for a given sample of data
#'
#' @param x sample
#' @param conf.level confidence level
#' @param iter total number of iterations
#' @param mu0 mean if the first sample if two are present
#' @param test the kind of test being done, either one or two with a default of two
#'
#' @return histogram of accepted and rejected regions for a given mean and sample.
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot curve hist layout lines polygon contour title
#' @importFrom stats density dnorm dpois pnorm rpois qt sd
#'
#' @examples
#' \dontrun{set.seed(55);x1=rnorm(30,mean=25, sd=5),bootpval(x=x1,mu=25,test="two")}
#'
#bootstrap function
bootpval<-function(x,conf.level=0.95,iter=3000,mu0=0, test="two"){

  # number of elements in the sample
  n=length(x)

  #centers the data around mu
  y=x-mean(x)+mu0

  #initalizes arrays to be filled later
  rs.mat<-c()
  xrs.mat<-c()

  #loops iter number of times to fill the matrices with samples and binds the columns together
  for(i in 1:iter){ # for loop - the loop will go around iter times
    rs.mat<-cbind(rs.mat,sample(y,n,replace=TRUE))
    xrs.mat<-cbind(xrs.mat,sample(x,n,replace=TRUE))
  }

  #finds the t statistic when the null is true
  tstat<-function(z){
    sqrt(n)*(mean(z)-mu0)/sd(z)
  }

  #tcalc
  tcalc=tstat(x)

  #re-samples y values
  ytstat=apply(rs.mat,2,tstat)

  #find mean of new samples
  xstat=apply(xrs.mat,2,mean)

  #confidence level
  alpha=1-conf.level

  #calculate the confidence interval
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  pvalue=ifelse(test=="two",length(ytstat[ytstat>abs(tcalc) | ytstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(ytstat[ytstat>tcalc])/iter,
                       length(ytstat[ytstat<xstat])/iter))

  #find values for the histogram
  h=hist(ytstat,plot=FALSE)

  #mid point
  mid=h$mid

  #test null hypothesis
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }

  #upper tail
  if(test=="upper"){
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Gray",length(mid)-ncolr),rep("Green",ncolr))
  }

  #lower tail
  if(test=="lower"){
    ncoll=length(mid[mid<=  -abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll))
  }

  #plot histogram
  hist(ytstat,col=col,freq=FALSE,las=1,main="",xlab=expression(T[stat]))

  #calculate p value
  pround=round(pvalue,4)

  title(substitute(paste(P[value],"=",pround)))
  return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}
