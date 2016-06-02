# Additional usefull methods for working with timeseries
# which seem to be missing in the original package
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

#' Generic function to extract the duration of any kind of time series object
#' 
#' @param x 	The time series for which the duration is requested
#' @param ... 	Extra arguments for future methods
#' 
#' @export 
duration <- function(x, ...) {
	UseMethod("duration")
}

#' Generic function to partition a time series into windows with overlap
#' 
#' @param x					Time series to be sliced
#' @param window.width 		Width of each partition window (in ms)
#' @param stepsize			Stepsize for the windowing (in ms)
#' @param window.function	Windowing function to be used during extraction (default: hanning window)
#' @param ...				Extra arguments for future methods
#'  
#' @return A matrix with one window per column
#' 
#' @export
slice <- function(x, window.width, stepsize, window.function=signal::hanning, ... ) {
	UseMethod("slice")
}

###
# S3 Generic Methods
###

#' @importFrom stats time
#' @export
time.tsSlice <- function(x, offset = 0, ...) 
{
	if(offset < 0 || offset > 1) {
		stop("Illegal offset value: ", offset)
	}
	
	attr(x, "window.times") + attr(x, "window.width")/1000 * offset
}

#' @importFrom stats frequency
#' @export
frequency.tsSlice <- function(x, ... )
{
	attr(x, "frequency")
}

#' @importFrom stats start
#' @export
start.tsSlice <- function(x, ...) 
{
	attr(x, "start")
}

#' @importFrom stats end
#' @export
end.tsSlice <- function(x, ...)
{
	attr(x, "end")
}

#' @export
print.tsSlice <- function(x, ... ) 
{
	cat("Time Series Slices:")
	f <- attr(x,"frequency")
	cat(paste("\n\tSampling Frequency:", f, sep="\t\t"))
	
	window.width 			<- attr(x, "window.width")
	window.width.ms 		<- paste(window.width,"ms",sep="")
	window.width.samples 	<- paste(nrow(x),"samples",sep="")
	cat(paste("\n\twindow.width:\t", window.width.ms, window.width.samples,  sep="\t\t"))
	
	stepsize 			<- attr(x,"stepsize")
	stepsize.ms			<- paste(stepsize,"ms",sep="")
	stepsize.samples	<- paste(stepsize*f/1000,"samples",sep="")
	cat(paste("\n\tstepsize:\t", stepsize.ms, stepsize.samples, sep="\t\t\t"))
	
	cat(paste("\n\tStart time:\t\t", start(x), sep="\t\t"))
	cat(paste("\n\tEnd time:\t\t", end(x), sep="\t\t"))
	cat(paste("\n\tNumber of Windows:", ncol(x), sep="\t\t"))
	cat("\n")
}
