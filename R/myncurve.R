#' myncurve simulation
#'
#' @description Function from lab 6 that calculates probability from -inf to point a
#'
#' @param a point on the curve
#' @param mu mean of the distribution
#' @param sigma standard deviation
#'
#' @return curve of shaded polygon and probabiities
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot curve hist layout lines polygon
#' @importFrom stats density dnorm dpois pnorm rpois
#'
#' @examples
#' \dontrun{myncurve(a=10,mu=3,sigma=1)}
myncurve = function(a=10, mu=3, sigma=1){
  x = c(1,2,3,4,5,6,7,8,9,10)
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma),main='Distribution')
  #x-coords
  xcurve = seq(mu-3*sigma,a,length=1000)
  #y-coords
  ycurve = dnorm(xcurve,mean=mu,sd=sigma)
  #polygon
  polygon((c(mu-3*sigma,xcurve, a)),c(0,ycurve,0),col ='orange')
  #calculate prob
  prob = round(pnorm(a,mean=mu,sd=sigma),4)
  prob
}


