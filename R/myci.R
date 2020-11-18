#' myncurve simulation
#'
#' @description Function from lab 11 that calculates the 95% confidence interval
#'
#' @param x random sample
#'
#' @return left and right bounds of the 95% confidence interval about the mean
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot curve hist layout lines polygon contour
#' @importFrom stats density dnorm dpois pnorm rpois qt sd
#'
#' @examples
#' \dontrun{myci(x=rnorm(30,mean=10,sd=3))}
myci = function(x){
  #Get sample size
  n = length(x)

  #Plus or minus vector
  mp = c(-1,1)

  #95% confidence interval
  alpha = 0.05
  #t - multiplier
  t = qt(1-(alpha/2),n-1)
  #Find the mean and calculate the margin of error
  mean(x) + (mp*t*sd(x))/sqrt(n)
}
