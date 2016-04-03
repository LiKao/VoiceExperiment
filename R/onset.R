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

as.onset <- function(v) {
	r <- list(start=v[1], end=v[2], energy.total=v[3], energy.avg=v[4]); 
	class(r) <- append(class(r), "onset");
	r
} 

#' Extracts Onsets from an energyDensity Object
#' 
#' @inheritParams onsets
#' @param ... ignored
#'  
#' @export
onsets.energyDensity <- function(ts, limit = 0.1, ... ) {
	if(limit <= 0 || limit >= 1) {
		stop("Illegal limit value: ", limit)
	}
	
	e.limit <- quantile(ts$energy,c(limit))
	gated <- ifelse(ts$energy > e.limit, 1, 0)
	changes <- c(gated,0) - c(0,gated)
	
	times <- time(ts)
	# The end times are shifted by 1.
	starts <- times[which(changes== 1)]
	ends   <- times[which(changes==-1)-1]
		
	r <- matrix( c(starts,  ends), ncol=2)
	r <- cbind(r, apply(X=r,1,FUN=function(s){sum(  window(ts, start=s[1], end=s[2]))}))
	r <- cbind(r, apply(X=r,1,FUN=function(s){mean( window(ts, start=s[1], end=s[2]))}))
	r <- apply(X=r, 1, FUN=as.onset)
	class(r) <- append(class(r), "onsetData")
	attr(r,"params") <- list(limit=limit) 
	attr(r,"dataType") <- "energyDensity"
	r
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
onsets.WaveData <- function(ts, limit = 0.1, window.width=10, stepsize=5, normalize=0.9, window.function=signal::hanning, ... ) {
	# Parameter testing done in called functions
	
	e <- energyDensity.WaveData(ts, window.width=window.width, stepsize=stepsize, normalize=normalize, window.function=window.function)
	r <- onsets.energyDensity(e, limit = limit)
	p1 <- attr(r,"params")
	p2 <- attr(e,"params")
	attr(r,"params") <- append(p1,p2)
	attr(r,"dataType") <- "WaveData"
	r
}

#' @export
print.onset <- function(x, ...) {
	cat("Onset Block:")
	cat(paste("\n\tStart:", 			x$start,sep="\t\t\t\t"))
	cat(paste("\n\tEnd:", 				x$end, sep="\t\t\t\t"))
	cat(paste("\n\tDuration:", 			formatC(x$end-x$start, digits=2), sep="\t\t\t"))
	cat(paste("\n\tTotal Energy:", 		formatC(x$energy.total, digits=2), sep="\t\t"))
	cat(paste("\n\tAverage Energy:",	formatC(x$energy.avg, digits=2), sep="\t\t"))
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
	l <- length(x)
	r <- do.call(c,lapply( X=1:l, FUN=function(i){fixup.names.onsetData(x, i)}))
	if(!is.null(padding) && l < padding) {
		o <- paste("Onset",(l+1):padding,sep="")
		n <- do.call(c,lapply(o,FUN=function(s){paste(s,c("Start", "End", "Duration", "TotalEnergy", "AverageEnergy"),sep=".")}))
		p <- rep(NA,length(n))
		names(p) <- n
		r <- c(r,p)
	}
	r
}
