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
energyDensity <- function(ts, window.width=10, stepsize=5, normalize=0.9, window.function=signal::hanning, ... ) {
	UseMethod("energyDensity")
}

#' Calculate the Energy Density at a Specific Position of a Standard R ts Object
#' 
#' @param ts 				The time series object.
#' @param start 			The starting position at which the energy density should be calculated.
#' @param end 				The end position until which the energy density should be calculated.
#' @param window.function   A windowing function to weight the samples.
#' 
#' TODO: Make this a polymorphic function (cannot assume this works for anything but WaveData)
#' 
#' @export
energyDensityAt <- function(ts, start, end, window.function=signal::hanning ) {
	s <- window(ts, start/1000, end/1000)^2
	l <- length(s)
	w <- window.function(l)
	w <- w / sum(w)
	sum(s*w)
}

#' energyDensity implementation for WaveData objects
#' 
#' @inheritParams energyDensity
#' @param ... ignored
#' @export
energyDensity.WaveData <- function(ts, window.width=10, stepsize=5, normalize=0.9, window.function=signal::hanning, ...) {
	
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
	# We need to generate only up to the last full window (incomplete windows 
	# at end are discarded)
	end <- seq(from=window.width, to=duration.ms, by=stepsize)
	energy <- vapply(X=end, FUN=function(e){energyDensityAt(ts$samples,e-window.width,e,window.function)}, FUN.VALUE=0)
	
	# Normalization, if activated
	if(normalize > 0) {
		m <- max(energy)
		f <- normalize / m
		energy <- energy * f
	}
	
	energy <- ts(energy, start=0, frequency=1000/stepsize)
	r <- list(energy=energy,
			  duration=tail(end,n=1)/1000)
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

length.energyDensity <- function(x, ...) {
	length(x$energy)
}

sum.energyDensity <- function(x, ... ) {
	sum(as.ts(x))
}

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
	s <- window( as.ts(x), start, end, frequency, deltat, extend, ... )
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

#' @export
plot.energyDensity <- function(x, ... ) {
	plot(as.ts(x), ylab="Energy", xlab="Time (s)", ...)
}

#' @export
lines.energyDensity <- function(x, ... ) {
	lines(as.ts(x), ...)
}
