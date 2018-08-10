#' @title Area Under the Curve
#' @description Calculates the Area Under the Curve for acoustic metrics  
#'
#' @param x     NxN matrix of Power (POW) values (must match dimensions of bgn)
#' @param type  Use "rollmean" or "trapezoidal" integration for AUC 
#'
#' @return A vector equal to nrow(xs) 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and Tim Boucher <tboucher@@tnc.org>
#'
#' @examples
#' POW <- matrix(runif(2000, 0, 15), nrow = 100, ncol = 20)
#' auc(POW, type = "trapezoidal")
#'
#' @export
auc <- function(x, type = c("rollmean", "trapezoidal")) {
  if(missing(x)) 
    stop("Must provide matrix")
  if(class(x) != "data.frame" & class(x) != "matrix")
    stop("Must be data.frame or Matrix class object")  
  # AUC rolling mean
  rollmean.fun <- function(a) { 
    x <- 1:length(a)
      id <- order(x) 
    return( sum(diff(x[id]) * zoo::rollmean(abs(a[id]), 2)) )
  }
  # AUC by numerical integration using trapezoidal rule
  trapz.fun <- function(x, y) {
    cumtrapz <- function(x, y) {
      if (missing(y)) {
        if (length(x) == 0) return(0)
          y <- x
          x <- 1:length(x)
      }
      if (length(x) == 0) return(0)
      if (!(is.numeric(x) || is.complex(x)) ||
          !(is.numeric(y) || is.complex(y)))
          stop("Arguments 'x' and 'y' must be real or complex.")
      x <- as.matrix(c(x))
        m <- length(x)
      if (is.vector(y)) y <- as.matrix(y)
      if (nrow(y) != m)
          stop("Arguments 'x' and 'y' are not compatible: nrow(y) != length(x).")
        n  <- ncol(y)
          dt <- repmat(diff(x)/2, 1, n)
        ct <- apply(dt * (y[1:(m-1), ] + y[2:m, ]), 2, cumsum)
      return(rbind(zeros(1, n), ct))
    }
    trapzfun <- function(f, a, b, maxit = 25, tol = 1e-07, ...) {
        stopifnot(is.numeric(a), length(a) == 1, is.finite(a),
                  is.numeric(b), length(b) == 1, is.finite(b))
        fun <- match.fun(f)
        f <- function(x) fun(x, ...)
    
        if (a == b) return(list(area = 0.0, iter = 0, error = 0))
        n <- 1
        h <- b - a
        T <- h * (f(a) + f(b)) / 2.0
        for (i in 1:maxit) {
            M <- 0
            for (j in 0:(n-1)) {
                M <- M + f(a + (j + 0.5) * h)
            }
              M <- h * M
                T <- (T + M) / 2.0
                  h <- h / 2.0
                n <- 2 * n
              err <- abs(T - M)
            if (err < tol) break
        }
        return(list(value = T, iter = i, rel.err = err))
    }
  
      if (missing(y)) {
          if (length(x) == 0) return(0)
          y <- x
          x <- seq(along=x)
      }
      if (length(x) == 0 && length(y) == 0) return(0)
      if (!(is.numeric(x) || is.complex(x)) ||
          !(is.numeric(y) || is.complex(y)) )
            stop("Arguments 'x' and 'y' must be real or complex vectors.")
      m <- length(x)
      if (length(y) != m)
        stop("Arguments 'x', 'y' must be vectors of the same length.")
      if (m <= 1) return(0.0)
        xp <- c(x, x[m:1])
          yp <- c(numeric(m), y[m:1])
            n <- 2*m
          p1 <- sum(xp[1:(n-1)]*yp[2:n]) + xp[n]*yp[1]
        p2 <- sum(xp[2:n]*yp[1:(n-1)]) + xp[1]*yp[n]
    return(0.5*(p1-p2))
  } 
  if( type == "rollmean") {  
    return( apply(x, MARGIN=1, FUN=rollmean.fun) )
  } else if(type == "trapezoidal") {
    return( apply(x, MARGIN=1, FUN=trapz.fun) )
  } else {
    stop("Not a valid option for type")
  }  
}

