# Calculation of Energy Density from a WaveFile Object
#
# Methods implemented as generic methods to allow easier 
# switch of implementation.
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

#' Generic Method to Calculate the Energy Density of an Object
#' 
#' This method calculates the energy density of any kind of object, which
#' can be interpreted as time series data. 
#' 
#' @param ts The object that contains the time series data
#' 
#' @param window.width The windowing width that is used to estimate the density (in ms). The time
#' 		  series data is chopped into windows of this length, and the energy density is calculated
#' 		  for each of these windows seperately. Defaults to 10ms.
#' 
#' @param stepsize The stepsize to be used for windowing. Windows are advanced this many ms for each
#' 		  calculation. Can be used to create overlapping windows. Defaults to 5ms.
#' 
#' @param normalize Normalization level for energy density values. Maximum energy density will be set to this
#' 		  level to simplify comparison between data files. Set to 0 to deactivate normalization. 
#' 		  Defaults to 0.9. Maximum normalization to 1.
#' 
#' @param window.function The windowing function to be used. This argument can be used to give
#'        different weigth to different samples within the window. Several usable windowing functions
#' 		  are defined in the signal package. Defaults to \code{\link[signal]{boxcar}}, i.e. equal weight
#' 		  for all samples.
#' @param ... Further object specific arguments.
#' 
#' @export
energyDensity <- function(ts, window.width=10, stepsize=5, normalize=1, window.function=signal::hanning, ... ) {
	UseMethod("energyDensity")
}

#' energyDensity implementation for WaveData objects
#' 
#' @inheritParams energyDensity
#' @param ... ignored
#' @export
energyDensity.WaveData <- function(ts, window.width=10, stepsize=5, normalize=1, window.function=signal::hanning, ...) {
	
	if(window.width<1) {
		stop("Illegal window width: ", window.width)
	}
	
	if(stepsize<1) {
		stop("Illegal stepsize: ", stepsize)
	}
	
	if(stepsize > window.width) {
		warning("Stepsize ",stepsize," is larger than window width ",window.width)
	}
	
	if(normalize < 0 || normalize > 1) {
		stop("Illegal normalization value: ", normalize)
	}

	
	f <- frequency(ts)
	
	l <- window.width/1000*f
	w <- window.function(l)
	w <- w / sum(w)
	w <- w^2
	
	m <- slice(ts, window.width=window.width, stepsize=stepsize)
	
	energy <- colSums((m * w)^2)
	
	# Normalization, if activated
	if(normalize > 0) {
		m <- max(energy)
		f <- normalize / m
		energy <- energy * f
	}
	
	r <- stats::ts(energy, start=0, frequency=1000/stepsize)
	attr(r,"duration") <- (tail(attr(m,"starts"),n=1)+window.width)/1000
	class(r) <- append("energyDensity",class(r))
	attr(r,"params") <- list(window.width=window.width, stepsize=stepsize, normalize=normalize)
	r
}

##### 
# S3 generic methods
#####

#' @export
duration.energyDensity <- function(x, ... ) {
	attr(x, "duration")
}

#' @importFrom stats window
#' @export
window.energyDensity <- function( x, start=NULL, end=NULL, frequency=NULL, deltat=NULL, extend=FALSE, ... ) {
	r <- NextMethod("window", x, start=start, end=end, frequency=frequency, deltat=deltat, extend=extend, ... )
	d <- length(r)/frequency(r)
	attr(r,"duration") <- d
	class(r) <- append("energyDensity",class(r))
	r
}

#####
# Output
#####

#' @export
print.energyDensity <- function(x, ... ) {
	cat("EnergyDensity Object:")
	cat(paste("\n\tNumber of Samples:", length(x), sep="\t\t"))
	cat(paste("\n\tSampling Frequency:", frequency(x),sep="\t\t"))
	cat(paste("\n\tDuration (seconds):", duration(x), sep="\t\t"))
	cat("\n")
}

#' Plotting of EnergyDensity Objects
#' 
#' @param x Energy density object to be plotted
#' @param db Decibel indicator positions in plot (set to \code{NULL} to deactivate)
#' @param limit Position of limit indicator (set to \code{NULL} to deactivate)
#' @param limit.lty Line type of limit indicator 
#' @param ... Additional arguments passed to \link{plot}
#' 
#' @export
plot.energyDensity <- function(x, db=c(0,-1,-2,-3,-4,-5,-6,-10,-15,-Inf), limit=c(0.1,0.01), limit.lty=3, ... ) {
	if(!is.null(db)) {
		mar <- par()$mar
		par(mar=mar+c(0,0,0,2))
	}
	NextMethod("plot",x, ylab="Energy", xlab="Time (s)", ...)
	if(!is.null(limit)) {
		if(length(limit>1)) {
			lines(x=time(x),y=rep(limit[1],length(x)),type="l",lty=limit.lty)
			lines(x=time(x),y=rep(limit[2],length(x)),type="l",lty=limit.lty)
		} 
		else {
			lines(x=time(x),y=rep(limit,length(x)),type="l",lty=limit.lty)
		}
	}
	if(!is.null(db)) {
		pw <- 10^(db/10)
		axis(4,at=pw,labels=paste(db,"dB",sep=""),las=2)
		par(mar=mar)
	}
}
