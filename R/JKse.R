#' @title A Jackknife estimate of the standard error
#' @description A Jackknife estimate of the standard error of the correlation statistic.
#' @param x,y Numeric vectors or data frames.
#' @return A Jackknife estimate of the standard error.
#' @examples
#' \dontrun{
#' data(law)
#' attach(law)
#' x <- law$LSAT
#' y <- law$GPA
#' se.Jackknife(x,y)
#' }
#' @export

se.Jackknife <- function(x, y) {
  if(length(x)==length(y)){
    n<-length(x)
    R.jack <- numeric(n)
    for (i in 1:n)
      R.jack[i] <- cor(x[-i],y[-i])
    se <- sqrt((n-1) * mean((R.jack - mean(R.jack))^2))
    return(se)
  }
  else stop("all arguments must have the same length")
}
