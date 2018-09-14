#' @title acoustic summary
#' @description Statistical summary of acoustic matrix by defined time-periods
#'
#' @param x           data.frame or matrix
#' @param dates       Column or vector containing date-time corresponding to number of rows in x
#' @param breaks      Breaks in dates note; use Date or POSIX format)       
#' @param splits      Split based on left (default) or right breaks
#' 
#' @return data.frame with columns for start and end time for summaries and summary statistics 
#'   see \code{\link[spatialEco]{moments}} for details on summary statistics  
#'
#' @note breaks are character in a "00:00:00" (ie., HH:MM:SS) format (see example).  
#'  
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#'   m <- read.csv("site1_20171115_PMN.csv")
#'   hr <- seq.POSIXt(as.POSIXct("2018-11-15"), 
#'                   (as.POSIXct("2018-11-15") + nrow(m)*60), 
#'                    by = "1 min")[-1441] 
#'  cat("Using default", "6am-10am,", "10am-2pm,", "2pm-6pm,", 
#'      "6pm-10pm,", "10pm-2am", "and 2am-6am breaks", "\n")
#'  ( am <- acoustic.summary(m[,-1], dates = hr) )
#'  am <- am[-1,]
#'  par(mfrow=c(2,2))
#'	  plot(am$start, am$max, type="b", xlab="start time", 
#'	       ylab="maximum", main="maximum")
#'      plot(am$start, am$median, type="b", xlab="start time", 
#'	       ylab="median", main="median")
#'	  plot(am$start, am$var, type="b", xlab="start time", 
#'	       ylab="variance",  main="variance")
#'	  plot(am$start, am$nmodes, type="b", xlab="start time", 
#'	       ylab="n-modes",  main="number of modes")
#'
#' @seealso \code{\link[spatialEco]{moments}} for details on summary statistics 
#'
#' @export
acoustic.summary <- function(x, dates, breaks = c("02:00:00", "06:00:00", "10:00:00", 
                             "14:00:00", "17:00:00", "21:00:00"), splits = "left") {
    if(!class(x) == "data.frame" & !class(x) == "matrix")
      stop("x must be data.frame or matrix")	
      idx <- list()
        for(i in 1:length(breaks)) {
    	  if(i == 1) {
    	    start = "00:00:00"
    	  } else {
    	    start = breaks[i]
    	  }
    	  if(i == length(breaks)) {  
            end = dates[length(dates)]  
          } else {
    	    end = breaks[i+1]
    	  }
    	  idx.values <- c(grep(start, dates):grep(end, dates))
            if(splits == "left") {
              idx.values <- idx.values[-length(idx.values)]
    		} else {
    		  idx.values <- idx.values[-1]
    		}
    	  idx[[i]] <- idx.values  
    	}
    d <- data.frame(start=dates[1], end=dates[length(dates)], 
	                t(spatialEco::moments(as.numeric(x.sub))))
	for(i in 1:length(idx)) {
      x.sub <- as.matrix(x[idx[[i]],])
	    if( nrow(x.sub) > 2) {
	      d <- rbind(d, data.frame(start=dates[min(idx[[i]])], 
	                 end=dates[max(idx[[i]])], 
	                 t(spatialEco::moments(as.numeric(x.sub)))))
		  } else {
	        d[nrow(d)+1,][1] <- dates[min(idx[[i]])]
			d[nrow(d),][2] <- dates[max(idx[[i]])]
		  }
	}
  return(d)	
} 
