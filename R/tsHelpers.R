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
#' @param x				Time series to be sliced
#' @param window.width 	Width of each partition window (in ms)
#' @param stepsize		Stepsize for the windowing (in ms)
#' @param ...			Extra arguments for future methods
#'  
#' @return A matrix with one window per column
#' 
#' @export
slice <- function(x, window.width, stepsize, ... ) {
	UseMethod("slice")
}