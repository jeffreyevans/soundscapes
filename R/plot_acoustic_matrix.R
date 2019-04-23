#' @title Plot acoustic matrix
#'
#' @param x          An acoustic matrix eg., time, bin, amplitude (dB)
#' @param add.time   Should time (minutes) be added to x axis 
#' @param time.int   How many time intervals
#' @param ncbks      Number of colors for amplitude 
#' @param cols       Custom color ramp eg., c("black", "yellow", "red")
#' @param transpose  Should the matrix be transposed
#'
#' @examples
#' \dontrun{
#'   setwd("C:/evans/myanmar/processed_data/PMN")
#'   filenames <- list.files(getwd(), pattern="csv$", full.names=TRUE)
#'     filenames <- filenames[grep("PMN", filenames)]
#'   i = 50
#'   pmn <- read.csv(filenames[i])[,-1]
#'
#'   plot_acoustic_matrix(pmn, add.time=TRUE, time.int=20)
#' }
#'
#' @export plot_acoustic_matrix
plot_acoustic_matrix <- function(x, add.time = FALSE, time.int = 20, ncbks = 20,
                                 start.time = NULL, end.time = NULL,  
                                 cols = NULL, transpose = FALSE, ...) {
	if(class(x) != "matrix") x <- as.matrix(x)
	  n = nrow(x)
	    dates <- as.POSIXct(strptime("2000-01-01 24:00", "%Y-%m-%d %H:%M"))
        mins <- seq.POSIXt(dates, (dates + n*60), by = "1 min")[1:n]
	    hr.min <- as.character(format(mins, "%H:%M"))
	if(is.null(start.time) & !is.null(end.time)) {
      message("Start time not defined, defaulting to 24:00")	
	    start.time = "24:00" 
	}
	  if(!is.null(start.time) & is.null(end.time)) {
	    message(paste0("Sub-setting to defined start time: ", start.time))
	      sub.idx <- seq(grep(start.time,hr.min)[1], n, by=1)
	        x <- x[sub.idx,]
	          dates <- dates[sub.idx] 
              mins <- mins[sub.idx]
	          hr.min <- hr.min[sub.idx]
	  } else if(!is.null(start.time) & !is.null(end.time)) {
	    message(paste0("Sub-setting to defined start-stop time: ", start.time, " - ", end.time))
	      sub.idx <- seq(grep(start.time, hr.min)[1], grep(end.time, hr.min)[1], by=1) 
	        x <- x[sub.idx,]
	          dates <- dates[sub.idx] 
              mins <- mins[sub.idx]
	          hr.min <- hr.min[sub.idx]
	  } else {
	    stop("Not a valid time sub-set")
	  }
	if(n > 1440) {
      warning("Greater than 24 hr sample with one-minute intervals, truncating")
      x <- x[1:1440,]
      }
	if(n < 1440 & any(is.null(start.time),  is.null(end.time) )) 
      warning("Fewer than 24 hr sample with one-minute intervals")
    if(is.null(cols)) {	  
      cfun <- colorRampPalette(c("black","red","green","blue"), interpolate = "linear")
	    message("Defaulting to (black, red, green, blue) color scheme")
    } else {
      cfun <- colorRampPalette(cols, interpolate = "linear")  
    }
  if(add.time) {  
    tidx <- which(as.character(mins) %in% as.character(levels(cut(mins, time.int)))) + time.int
      axis.labs <- as.character(format(mins, "%H:%M"))[tidx[-length(tidx)]]
	  axis.labs[length(axis.labs)+1] <- as.character(format(mins[n], "%H:%M"))
    axis.idx <- (tidx/max(tidx))
    }  
    plot.am <- function(x, at=TRUE, ...) {
      dots <- as.list(match.call(expand.dots = TRUE)[-1])
          dots[["x"]] <- x
      if (is.null(dots[["useRaster"]]) & "useRaster" %in% names(dots) == FALSE) dots[["useRaster"]] <- TRUE
	  if (is.null(dots[["col"]]) & "col" %in% names(dots) == FALSE) dots[["col"]] <- cfun(ncbks)
	  if(at) { if(is.null(dots[["xaxt"]])) dots[["xaxt"]] <-  "n"  } 
        do.call("image", dots)
	  if(at) {	  
        axis(1, axis.labs, at=axis.idx, cex.axis = .7)
      }	  
    }
	rotate <- function(x) t(apply(x, 2, rev))
      if(transpose) {
         am <- rotate(as.matrix(x))
	   } else {
         am <- as.matrix(x)	
	   }  
  plot.am(am, ...)	
}
