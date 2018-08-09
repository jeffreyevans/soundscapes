#' @title remove time-period(s)
#' @description Removes rows in data.frame/matrix based on range(s) in date-time column. 
#'
#' @param x           data.frame 
#' @param date.col    Column in x data.frame containing date-time
#' @param start       Start time(s)       
#' @param end         End time(s) corresponding to each start time 
#' 
#' @return data.frame with rows corresponding to specified time period removed 
#'
#' @note start and end times are character in a "00:00:00" (ie., HH:MM:SS) format (see example).  
#'  
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#'   hr <- seq.POSIXt(as.POSIXct("2018-08-16"), (as.POSIXct("2018-08-16") + 86400), 
#'                    by = "1 min")
#'   head( my.dat <- data.frame(date = hr, y = runif(length(hr))) ) 
#'     nrow(my.dat)
#'   
#'   start.time = c("00:50:00", "02:00:00") 
#'   end.time = c("01:05:00", "03:10:00") 
#'   
#'   my.dat.sub <- rm.timeperiods(my.dat, "date", start.time, end.time)
#'     nrow(my.dat.sub)
#'
#' @export
rm.timeperiods <- function(x, date.col, start, end) {
  if( class(x) != "data.frame" && class(x) != "matrix")
    stop("Data must be a data.frame or Matrix class")
  if(missing(date.col)) stop("Must define date column")
    if(missing(start)) stop("Must define start time in HH:MM:SS format")
      if(missing(end)) stop("Must define end time in HH:MM:SS format")
        if (is.na(charmatch(date.col, colnames(x)))) 
	      stop(date.col, " is not one of the columns")
  if(length(start) != length(end)) 
    stop("start and end times must be equal")
  if( length(start) > 1 ) {
    rm.idx <- vector()  
      for(i in length(start)) {
	    rm.idx <- append(rm.idx, c(grep(start[i], 
		                 x[,date.col]):grep(end[i], 
						 x[,date.col])) )
	  }
  } else {
    rm.idx <- c(grep(start[1],x[,date.col]):grep(end[1],x[,date.col]))
  }
      if(length(rm.idx) > 0 ) { x <- x[-rm.idx,] } 
    cat(length(rm.idx), "rows removed", "\n")
  return(x)  
}  
