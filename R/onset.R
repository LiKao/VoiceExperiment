# Functions to calculate Onsets
# 
# Author: till
###############################################################################

#' Calculate (Physical) Onset Times from an Object containing Time Series Data
#' 
#' This function calculates onset times for any kind of object containing time
#' series data. The onsets are calculated by comparing the overall energy of 
#' each part of the time series with the rest of the time series. Positions
#' where the energy is above a portion of \code{limit}, compared to the overal
#' energy, are assumed to contain actual data.
#' 
#' @param ts The object containing the time series data.
#' 
#' @param limit Parameter used for filtering silence. Energy below this level
#' (compared to all other energy levels in term of percentiles) is ignored.
#' 
#' @param ... Object specific parameters  
#' 
#' @export
onsets <- function(ts, limit = 0.1, ... ) {
	UseMethod("onsets")
}

#' Extracts Onsets from an energyDensity Object
#' 
#' @inheritParams onsets
#' @param ... ignored
#'  
#' @export
onsets.energyDensity <- function(ts, limit = 0.1, ... ) {
	e.limit <- quantile(ts$energy,c(limit))
	gated <- ifelse(ts$energy > e.limit, 1, 0)
	changes <- c(gated,0) - c(0,gated)
	# Indexing starts at 1 in R, hence the first sample is at time 0 (index-1).
	# Furthermore the end times are shifted by 1, hence we must subtract 2 here.
	r <- matrix(c(which(changes==1)-1,which(changes==-1)-2),ncol=2)/frequency(ts$energy)
	r <- cbind(r,apply(X=r,1,FUN=function(s){mean(window(ts$energy, start=s[1], end=s[2]))}))
	apply(X=r, 1, FUN=function(s){l<-list(start=s[1], end=s[2], energy.avg=s[3]); class(l) <- append(class(l), "onset"); l})
}

#' Specialized Method for Extracting Onsets from WaveData objects
#' 
#' This method first extracts the energy density, using the \code{\link{energyDensity}} method, and then
#' calculates onsets based on the energy density.
#' 
#' @inheritParams onsets
#' @inheritParams energyDensity
#' @param ... ignored
#' 
#' @export
onsets.WaveData <- function(ts, limit = 0.1, window.width=10, stepsize=5, window.function=signal::boxcar, ... ) {
	e <- energyDensity.WaveData(ts)
	onsets.energyDensity(e)
}

#' @export
print.onset <- function(x, ...) {
	cat("Onset Block:")
	cat(paste("\n\tStart:", x$start,sep="\t\t\t\t"))
	cat(paste("\n\tEnd:", x$end, sep="\t\t\t\t"))
	cat(paste("\n\tAverage Energy:", formatC(x$energy.avg,digits=2), sep="\t\t"))
}