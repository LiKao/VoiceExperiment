# Class implementation of WaveData S3 Class
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

# TODO: simplify representation as a vector with additional attributes (similar to ts objects)

#####
# Generic usefull R functions
#####

#' @export
length.WaveData <- function(x, ...) {
	length(x$samples)
}

#####
# Usage as timeseries data
#####

#' @importFrom stats as.ts
#' @export
as.ts.WaveData <- function(x, ...) {
	x$samples
}

#' @importFrom stats frequency
#' @export
frequency.WaveData <- function(x, ... ) {
	frequency(as.ts(x))
}

#' @importFrom stats time
#' @export
time.WaveData <- function(x, ... ) {
	time(as.ts(x))
}

#' @importFrom stats cycle
#' @export
cycle.WaveData <- function(x, ... ) {
	cycle(as.ts(x))
}

#' @importFrom stats window
#' @export
window.WaveData <- function( x, start=NULL, end=NULL, frequency=NULL, deltat=NULL, extend=FALSE, ... ) {
	s <- window( as.ts(x), start, end, frequency, deltat, extend, ... )
	d <- length(s)/frequency(s)
	r <- list( samples = s, duration = d)
	class(r) <- append(class(r),"WaveData")
	r
}

	
#####
# Output Methods
#####
	
#' @export
print.WaveData <- function(x, ...) {
	cat("WaveData Object:")
	cat(paste("\n\tNumber of Samples:", length(x), sep="\t\t"))
	cat(paste("\n\tSampling Frequency:", frequency(x),sep="\t\t"))
	cat(paste("\n\tDuration (seconds):", x$duration, sep="\t\t"))
	if( !is.null(x$filename) ) {
		cat(paste("\n\tOriginal Filename:", x$filename, sep="\t\t"))	
	}
}

#' @export
summary.WaveData <- function(object, ...) {
	print(object)
} 

#' @export
plot.WaveData <- function(x, ... ) {
	plot( as.ts(x), ylab="Intensity", xlab="Time (s)", ...)
}

#' @export
lines.WaveData <- function(x, ... ) {
	lines( as.ts(x), ... )
}


	