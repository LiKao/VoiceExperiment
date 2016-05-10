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

require(methods)

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

	duration.ms <- ts$duration*1000
	f <- frequency(ts)
	# We need to generate only up to the last full window (incomplete windows 
	# at end are discarded)
	starts <- (seq(from=window.width, to=duration.ms, by=stepsize)-window.width)/1000
	starts.samples <- round(starts*f)
	
	l <- window.width/1000*f
	w <- window.function(l)
	w <- w / sum(w)
	w <- w^2
	
	i <- do.call(c,lapply(1:length(starts.samples),FUN=function(v){rep(v,l)}))
	j <- do.call(c,lapply(starts.samples,FUN=function(v){seq(v,v+l-1)+1}))
	
	m <- Matrix::sparseMatrix(i=i,j=j,x=w)
	
	energy <- as.vector(m %*% ((as.matrix(ts)[1:dim(m)[2]])^2))
	
	# Normalization, if activated
	if(normalize > 0) {
		m <- max(energy)
		f <- normalize / m
		energy <- energy * f
	}
	
	energy <- stats::ts(energy, start=0, frequency=1000/stepsize)
	r <- list(energy=energy,
			  duration=(tail(starts,n=1)+window.width)/1000)
	class(r) <- append(class(r),"energyDensity")
	attr(r,"params") <- list(window.width=window.width, stepsize=stepsize, normalize=normalize)
	r
}

# TODO: Simplify representation as a vector with additional attributes (like ts objects, will simplify construction, mean and sum)


##### 
# S3 generic methods
#####

#####
# Basic methods
#####

#' @export
length.energyDensity <- function(x, ...) {
	length(x$energy)
}

#' @export
sum.energyDensity <- function(x, ... ) {
	sum(as.ts(x))
}

#' @export
mean.energyDensity <- function(x, ... ) {
	mean(as.ts(x))
}

#####
# Usage as timeseries data
#####

#' @importFrom stats as.ts
#' @export
as.ts.energyDensity <- function(x, ...) {
	x$energy
}

#' @importFrom stats frequency
#' @export
frequency.energyDensity <- function(x, ... ) {
	frequency(as.ts(x))
}

#' @importFrom stats time
#' @export
time.energyDensity <- function(x, ... ) {
	time(as.ts(x))
}

#' @importFrom stats cycle
#' @export
cycle.energyDensity <- function(x, ... ) {
	cycle(as.ts(x))
}

#' @importFrom stats window
#' @export
window.energyDensity <- function( x, start=NULL, end=NULL, frequency=NULL, deltat=NULL, extend=FALSE, ... ) {
	s <- window( as.ts(x), start=start, end=end, frequency=frequency, deltat=deltat, extend=extend, ... )
	d <- length(s)/frequency(s)
	r <- list( energy = s, duration = d)
	class(r) <- append(class(r),"energyDensity")
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
	cat(paste("\n\tDuration (seconds):", x$duration, sep="\t\t"))
}

#' Plotting of EnergyDensity Objects
#' 
#' @param x Energy density object to be plotted
#' @param db Decibel indicator positions in plot (set to \code{NULL} to deactivate)
#' @param ... Additional arguments passed to \link{plot}
#' 
#' @export
plot.energyDensity <- function(x, db=c(0,-1,-2,-3,-4,-5,-6,-10,-15,-Inf), ... ) {
	if(!is.null(db)) {
		mar <- par()$mar
		par(mar=mar+c(0,0,0,2))
	}
	plot(as.ts(x), ylab="Energy", xlab="Time (s)", ...)
	if(!is.null(db)) {
		pw <- 10^(db/10)
		axis(4,at=pw,labels=paste(db,"dB",sep=""),las=2)
		par(mar=mar)
	}
}

#' @export
lines.energyDensity <- function(x, ... ) {
	lines(as.ts(x), ...)
}
