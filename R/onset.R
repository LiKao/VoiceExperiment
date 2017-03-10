# Functions to calculate Onsets
#
# Copyright (C) 2016 Tillmann Nett for FernUni Hagen
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#
###############################################################################

#' Calculate (Physical) Onset Times from an Object containing Time Series Data
#' 
#' This function calculates onset times for any kind of object containing time
#' series data. The onsets are calculated by comparing the overall energy of 
#' each part of the time series with the rest of the time series. Positions
#' where the energy is above a portion of \code{limit}, compared to the overal
#' energy, are assumed to contain actual data.
#' 
#' @param ts 				The object containing the time series data.
#' 
#' @param limit 			Parameters used for detecting onsets. See 'Details'
#' 
#' @param limit.type 		Type of limit, which is used to detect on- and 
#' 							offsets. If set to \code{absolute} (default) then
#' 							the limit value is used directly. If set to
#' 							\code{relative} then the limit is determined
#' 		  					as the energy in the i-th percentile, where i is
#' 							given by the limit parameter.
#' 
#' @param min.duration		Minimum duration of onsets to be detected. Onsets
#' 							that have less duration than this value (in seconds)
#' 							will be assumed to be false positives and be discarded.
#' 							Set to \code{NULL} or \code{0} to disable filtering of 
#' 							short onsets.
#' 
#' @param ... Object specific parameters  
#' 
#' @details
#' 
#' The onset detection mechanism used a hysteresis filter by default. If energy goes above
#' a level defined by the first element of the \code{limit} parameter, this is detected as
#' a start of a signal block (i.e. an onset). For the signal block to end, the energy has
#' to go below the level of the second element of the \code{limit} parameter. This way
#' the energy limit for offset detection can be much lower than the energy for onset 
#' detection, giving much more reliable onset and offset values.
#' 
#' If no hysteresis is desired, simply pass one single value as limit parameter (or the same
#' value twice). 
#' 
#' @export
onsets <- function(ts, limit = c(0.1,0.01), limit.type=c("absolute","relative"), 
				   min.duration=0.1, ... ) {
	if(length(ts)==1 && is.na(ts)) {
		warning("onset detection called with NA")
		r <- NA
		class(r) <- "onsetData"
		attr(r,"Error") <- attr(ts,"Error")
		return(r)
	}
	UseMethod("onsets")
}

#' Extracts Onsets from an energyDensity Object
#' 
#' @inheritParams onsets
#' @param ... ignored
#'  
#' @export
onsets.energyDensity <- function(ts, limit = c(0.1,0.01), limit.type=c("absolute","relative"), 
								 min.duration=0.1, ... ) {
	limit.type = match.arg(limit.type)
	
	if(length(limit)>1) {
		limit.high <- limit[1]
		limit.low  <- limit[2]
		
		if(limit.low <= 0 ) {
			stop("Illegal lower limit value: ", limit.low)
		}
		
		if(limit.high >= 1 ) {
			stop("Illegal higher limit value: ", limit.high)
		}
		
		if(limit.high <= limit.low ) {
			stop("Illegal limit value: High limit ", limit.high, " is below low limit ", limit.low)
		}
		
	} 
	else {
		if(limit <= 0 || limit >= 1) {
			stop("Illegal limit value: ", limit)
		}
		
		limit.high <- limit
		limit.low  <- limit
	}
	
	e.limit.high <- switch( limit.type,
					   		absolute = limit.high,
			           		relative = quantile(ts,c(limit.high)))
					
	e.limit.low <- switch( limit.type,
					  	   absolute = limit.low,
						   relative = quantile(ts,c(limit.low)))
			   
	gated.upper <- ifelse(ts > e.limit.high, 1, 0)
	gated.lower <- ifelse(ts > e.limit.low,  1, 0)
	
	
	change.rise <- c(gated.upper,0) - c(0,gated.upper)
	change.fall <- c(gated.lower,0) - c(0,gated.lower) 
	
	times <- time(ts)
	# The end times are shifted by 1.
	starts <- times[which(change.rise== 1)]
	ends   <- times[which(change.fall==-1)-1]
	
	r <- list()
	
	sidx <- 1
	eidx <- 1

	while(sidx <= length(starts) ) {

		start <- starts[sidx]
		# Find first end point after the current start
		while(eidx < length(ends) && ends[eidx] <= start) {
			eidx <- eidx + 1
		}
		
		end <- ends[eidx]
		samples <- window(ts, start=start, end=end)
			
		onset <- list(start=start,end=end, energy.total=sum(samples), energy.avg=mean(samples) )
		class(onset) <- append(class(onset), "onset");	
		if(is.null(min.duration) || min.duration < 0 || duration(onset) >= min.duration ) {
			r <- c(r,list(onset))
		}
		
		# Skip all starts, which are before the current end
		while(sidx <= length(starts) && starts[sidx] <= end) {
			sidx <- sidx + 1
		}
	}
		
	class(r) <- append(class(r), "onsetData")
	attr(r,"params") <- list(limit=limit, limit.type=limit.type) 
	attr(r,"dataType") <- "energyDensity"
	r
}

#' Specialized Method for Extracting Onsets from WaveData objects
#' 
#' This method first extracts the energy density, using the \code{\link{energyDensity}} method, and then
#' calculates onsets based on the energy density.
#' 
#' @inheritParams onsets
#' @param energy.params Additional Parameters passed to \code{\link{energyDensity}}.
#' @param ... ignored
#' 
#' @export
onsets.WaveData <- function( ts, limit = c(0.1,0.01), limit.type=c("absolute","relative"),
							 min.duration=0.1, energy.params = list(), ... ) {
	# Parameter testing done in called functions
	
	e <- do.call(energyDensity.WaveData, c(list(ts=ts), energy.params) )
	r <- onsets.energyDensity(e, limit = limit, limit.type=limit.type)
	p1 <- attr(r,"params")
	p2 <- attr(e,"params")
	p1$dataType = "WaveData"	
	attr(r,"params") <- c(p1,energy.params=list(p2)) 
	r
}

#' @export
duration.onset <- function(x, ...) {
	x$end-x$start
}

#' @export
print.onset <- function(x, ...) {
	cat("Onset Block:")
	cat(paste("\n\tStart:", 			x$start,sep="\t\t\t\t"))
	cat(paste("\n\tEnd:", 				x$end, sep="\t\t\t\t"))
	cat(paste("\n\tDuration:", 			formatC(duration(x), digits=2), sep="\t\t\t"))
	cat(paste("\n\tTotal Energy:", 		formatC(x$energy.total, digits=2), sep="\t\t"))
	cat(paste("\n\tAverage Energy:",	formatC(x$energy.avg, digits=2), sep="\t\t"))
	cat("\n")
}

#' @export
as.matrix.onset <- function(x, ... ) {
	r <- c(x$start, x$end, x$end-x$start, x$energy.total, x$energy.avg)
	names(r) <- c("Start", "End", "Duration", "TotalEnergy", "AverageEnergy")
	r
}

fixup.names.onsetData <- function(x, i) {
	v<-as.matrix.onset(x[[i]]); 
	names(v) <- paste(paste("Onset",i,sep=""),names(v),sep=".");
	v
}

#' @export
as.matrix.onsetData <- function(x, padding = NULL, ...) {
	if(length(x) == 1 && is.na(x)) {
		if(is.null(padding)) {
			return(NA)
		}
		l <- 0
		r <- NULL
	} else {
		l <- length(x)
		r <- do.call(c,lapply( X=1:l, FUN=function(i){fixup.names.onsetData(x, i)}))
	}
	if(!is.null(padding) && l < padding) {
		o <- paste("Onset",(l+1):padding,sep="")
		n <- do.call(c,lapply(o,FUN=function(s){paste(s,c("Start", "End", "Duration", "TotalEnergy", "AverageEnergy"),sep=".")}))
		p <- rep(NA,length(n))
		names(p) <- n
		r <- c(r,p)
	}
	t(as.matrix(r))
}
