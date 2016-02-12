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

#' @export
print.WaveData <- function(x, ...) {
	cat("WaveData Object:")
	cat(paste("\n\tNumber of Samples:", length(x$samples), sep="\t\t"))
	cat(paste("\n\tSampling Frequency:", frequency(x$samples),sep="\t\t"))
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
	plot(x$samples, ylab="Intensity", xlab="Time (s)", ...)
}