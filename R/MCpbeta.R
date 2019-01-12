#' @title A Monte Carlo estimate of the Beta distribution cdf
#' @description A Monte Carlo estimate of the Beta distribution cdf.
#' @param x Value of quantiles.
#' @param p,q Non-negative integer parameters of the Beta distribution.
#' @param m Sampling times
#' @return A Monte Carlo estimate.
#' @examples
#' \dontrun{
#' x<-numeric(9)
#' for (i in 1:9) x[i]<-pbeta.MC(i/10,3,3)
#' plot(1:9/10,x,type='l')
#' }
#' @export

pbeta.MC<-function(x,p,q,m=10000){
  if(p %% 1==0&p %% 1==0){
    y <- runif(m, min=0, max=x)
    F.hat <- mean(beta(p,q)*x*y^(p-1)*(1-y)^(q-1))
    return(F.hat)
  }
  else stop("the parameters of the Beta distribution must be integers")
}
