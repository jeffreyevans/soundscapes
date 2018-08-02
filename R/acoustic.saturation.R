#' @title Acoustic saturation
#' @description Calculates the acoustic saturation index (saturation per minute/row).   
#'
#' @param pow          NxN matrix of Power (POW) values (must match dimensions of bgn)
#' @param bgn          NxN matrix of Background Noise (BGN) values (must match dimensions of pow)
#' @param min.db       Minimum decibels (default is 0)
#' @param max.db       Maximum decibels (default is 6.5)
#' @param p            Threshold percentile for bgn, default is p=0.90 (90th percentile)
#' @param noise        Remove noise in the BGN. Default is null and valid options are mean or median)
#' @param decibels     (FALSE/TRUE) If TRUE, transform the data, assuming Hz, to decibels using Db=20*log(x) 
#' @param raw.freq     (FALSE/TRUE) Return raw frequencies rather than normalized 
#' @param probs        (FALSE/TRUE) Return a vector of row-wise probabilities indicating saturation
#' @param smooth       (FALSE/TRUE) Smooth the raw or normalized frequencies 
#' @param window       Size of window to use in smoothing 
#'
#' @return A vector equal to nrow(pow & bgn) 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and Tim Boucher <tboucher@@tnc.org>
#'
#' @references Burivalova, Z., M. Towsey, T. Boucher, A. Truskinger, P. Roe & E.T. Game (2018) Using 
#'               soundscapes to detect variable degrees of human influence on tropical forests in 
#'               Papua New Guinea. Conservation Biology 329(1):205-215 
#'
#' @examples
#'  # Small example
#'  POW <- matrix(runif(2000, 0, 15), nrow = 100, ncol = 20)
#'  BGN <- matrix(runif(2000, 0, 15), nrow = 100, ncol = 20)
#' 
#'  # Normalized (raw.freq = FALSE) and smoothed (smooth=TRUE)	
#'  summary( sat <- acoustic.saturation(POW, BGN, smooth = TRUE) )
#'
#'  # Return probability of saturation	
#'  ( sat <- acoustic.saturation(POW, BGN, probs = TRUE) )
#' 
#'  # Large example
#'  rtnorm <- function(n, mean, sd, a = -Inf, b = Inf){
#'      qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd)
#'  }
#'  POW <- matrix(rtnorm(368640, mean = 1.60741, sd = 1.66311, a = 0, b = 23.4575), 
#'                nrow = 1440, ncol = 256) 
#'  BGN <- matrix(rtnorm(368640, mean = 1.60741, sd = 1.66311, a = 0, b = 23.4575), 
#'                nrow = 1440, ncol = 256) 
#'  
#'  # Normalized (raw.freq = FALSE) and smoothed (smooth=TRUE)			  
#'  summary( sat <- acoustic.saturation(POW, BGN, smooth = TRUE) )
#'  
#'  # Not normalized (raw.freq = TRUE) and not smoothed (smooth=FALSE)		
#'  summary( sat <- acoustic.saturation(POW, BGN, raw.freq = TRUE, smooth = FALSE) )
#' 
#' @seealso \code{\link[smooth]{sma}} for details on ARIMA smoothing 
#'
#' @export acoustic.saturation 	
acoustic.saturation <- function(pow, bgn, min.db = -90, max.db = 6.5, p = 0.90, noise = NULL,   
                                decibels = FALSE, raw.freq = FALSE, probs = FALSE, 
								smooth = FALSE, window = 5) {
  if( ncol(pow) != ncol(bgn) )
    stop("Column dimensions of pow and bgn do not match")
  if( nrow(pow) != nrow(bgn) )
    stop("Row dimensions of pow and bgn do not match")
  if(decibels) {
    message("Converting data to decibels, assuming input units in Hz")
      pow <- 20 * log(as.matrix(pow))   
      bgn <- 20 * log(as.matrix(bgn))   
  }	
  if(!is.null(noise)) {
    message("Partialling out noise by subtracting centered value")
	  if( noise == "mean") {
        bgn <- ( as.matrix(bgn) - mean(as.matrix(bgn)) )
	  } else {
        bgn <- ( as.matrix(bgn) - stats::median(as.matrix(bgn)) )	  
	  }
  }  
  pct <- stats::quantile(as.matrix(bgn), p = p)
	amf <- matrix( mapply( function(x ,y, p = pct, min.Db = min.db, max.Db = max.db) { 
	                               ifelse( x > max.Db & x > min.Db, 1, ifelse(y > pct, 1, 0))
								   }, pow, bgn), nrow=nrow(pow), ncol=ncol(pow), byrow = TRUE)
	if(probs == TRUE) {
	  Sm <- apply(amf, MARGIN = 1, FUN = function(x) prop.table(table(x))[2] )	  
	  } else {							   
	  Sm = apply(amf, MARGIN = 1, FUN = sum)								 
        if(raw.freq == FALSE) {									 
          Sm <- Sm / length(Sm)
        }	 
        if(smooth) {
          Sm <- as.numeric(smooth::sma(Sm, h = window)$fitted)  
        }
	}
  return(Sm)  
}
