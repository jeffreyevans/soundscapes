#' @title acoustic summary
#' @description Calculates the Thiel-Sen slope for linear rate of change 
#'
#' @param x            An ordered numeric vector representing the time-series 
#' @param conf.level   Confidence level to test (default 0.95)
#' @param na.rm       (FALSE/TRUE) Remove missing (NA) values
#'
#' @return A list object with: slope, z.score, p.value, n, conf.int.  
#' 
#' @notes Pair-wise linear slopes are derived following: d(k) = (x(j) - x(i)) / (j - i)
#'          where; d is the slope and i, j are indices of x. The slope is then derived 
#'          using: median(d(k)) 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and Tim Boucher <tboucher@@tnc.org>
#'
#' @references Sen, P.K. (1968) Estimates of the regression coefficient based on Kendall's tau. 
#'               Journal of the American Statistical Association 63:1379–1389.  
#'
#' @export 
ThielSen.slope <- function (x, conf.level = 0.95, na.rm = FALSE) {
  if (!is.numeric(x)) stop("'x' must be a numeric vector")
  if(na.rm) {
    x <- x[!is.na(x)]
	  warning("Observations have been dropped due to NA's")
  } else {  
    na.fail(x)
  }
    n <- length(x)
    f <- table(x)
      names(f) <- NULL
    varS <- (n * (n-1) * (2 * n + 5) - (sum(f * (f - 1) * 
	        (2 * f + 5)))) / 18
	k <- 0
    d <- rep(NA, n * (n - 1)/2)
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          k <- k + 1
          d[k] <- (x[j] - x[i])/(j - i)
        }
      }
    b.sen <- median(d, na.rm = TRUE)
      C <- qnorm(1 - (1 - conf.level)/2) * sqrt(varS)
        rank.up <- round((k + C)/2 + 1)
        rank.lo <- round((k - C)/2)
        rank.d <- sort(d)
      lo <- rank.d[rank.lo]
      up <- rank.d[rank.up]
      S <- 0.0   
    for(j in 1:n) { S <- S + sum(sign(x[j] - x[1:j]))}
	  sg <- sign(S)
      z <- sg * (abs(S) - 1)/sqrt(varS)
        pval <- 2 * min(0.5, pnorm(abs(z), lower.tail = FALSE))
        cint <- c(lo, up)
    tss <- list(slope = b.sen,
                z.score = z,
                p.value = pval,
                n = n,
                conf.int = cint)
    return(tss)
}
