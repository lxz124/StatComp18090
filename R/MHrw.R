#' @title A Metropolis-Hastings sampler from a standard Cauchy distribution using R
#' @description A Metropolis-Hastings sampler from a standard Cauchy distribution using R.
#' @param N The number of samples.
#' @param sigma The standard deviation of Metropolis-Hastings sampler.
#' @param x0 The initial value of Metropolis-Hastings sampler.
#' @return A random sample of size \code{n}.
#' @examples
#' \dontrun{
#' rnR=rw.Metropolis(2,10,2000)
#' plot(rnR$x,type='l')
#' }
#' @export

rw.Metropolis <- function(sigma=2, x0=10, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= ((1+x[i-1]^2)/(1+y^2)))
      x[i] <- y
    else {
      x[i] <- x[i-1]
      k <- k + 1
    }
  }
  return(list(x=x, k=k))
}
