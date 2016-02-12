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
#' @param window.function The windowing function to be used. This argument can be used to give
#'        different weigth to different samples within the window. Several usable windowing functions
#' 		  are defined in the signal package. Defaults to \code{\link[signal]{boxcar}}, i.e. equal weight
#' 		  for all samples.
#' @param ... Further object specific arguments.
#' 
#' @export
energyDensity <- function(ts, window.width=10, stepsize=5, window.function=signal::boxcar, ... ) {
	UseMethod("energyDensity")
}

#' Calculate the Energy Density at a Specific Position of a Standard R ts Object
#' 
#' @param ts 				The time series object.
#' @param start 			The starting position at which the energy density should be calculated.
#' @param end 				The end position until which the energy density should be calculated.
#' @param window.function   A windowing function to weight the samples.
#' 
#' @export
energyDensityAt <- function(ts, start, end, window.function=signal::boxcar ) {
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
energyDensity.WaveData <- function(ts, window.width=10, stepsize=5, window.function=signal::boxcar, ...) {
	
	duration.ms <- ts$duration*1000
	# We need to generate only up to the last full window (incomplete windows 
	# at end are discarded)
	end <- seq(from=window.width, to=duration.ms, by=stepsize)
	energy <- vapply(X=end, FUN=function(e){energyDensityAt(ts$samples,e-window.width,e,window.function)}, FUN.VALUE=0)
	energy <- ts(energy, start=0, frequency=1000/stepsize)
	r <- list(energy=energy,
			  duration=tail(end,n=1)/1000)
	class(r) <- append(class(r),"energyDensity")
	r
}

#' @export
print.energyDensity <- function(x, ... ) {
	cat("EnergyDensity Object:")
	cat(paste("\n\tNumber of Samples:", length(x$energy), sep="\t\t"))
	cat(paste("\n\tSampling Frequency:", frequency(x$energy),sep="\t\t"))
	cat(paste("\n\tDuration (seconds):", x$duration, sep="\t\t"))
}

#' @export
plot.energyDensity <- function(x, ... ) {
	plot(x$energy, ylab="Energy", xlab="Time (s)", ...)
}
