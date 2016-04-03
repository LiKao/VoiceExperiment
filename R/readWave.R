# Functions for reading wave files into usable formats
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

#' Reads Wave Date From a File
#'
#' This function reads wave data from a file. The data is read using
#' the \code{\link[tuneR]{readWave}} function and then automatically
#' simplified for further processing.
#' 
#' @param filename The filename to be read
#' 
#' @param channels The channel(s) that should be used. The wave file is automatically
#' reduced to a mono wave file, either by selecting a single channel (values 
#' \code{left} or \code{right}) or by averaging accros both channels (\code{both}).
#' If the file is alread a mono file, this argument is ignored.
#' 
#' @return An object of type \code{WaveData}
#' 
#' @export 
#' 
read.wav <- function(filename, channels=c("both","left","right")) {
	channels <- match.arg(channels)
	
	w <- tuneR::readWave(filename)
	
	# Reduce stereo and Normalize to format independent of bit-depth
	s <- (if(w@stereo){ tuneR::mono(w,which=channels)@left} else {w@left}) / (2^w@bit) 
	
	l <- length(s)
	d <- l/w@samp.rate
	
	s <- ts(s, start=0, end=d, frequency=w@samp.rate)
	
	r <- list(samples=s,
			  filename=filename,
			  duration=d,
			  tuneR.data = w)
	class(r) <- append(class(r),"WaveData")
	attr(r,"params") <- list(channels=channels) 
	r
}
