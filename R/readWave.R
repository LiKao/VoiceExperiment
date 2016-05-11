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
#' @param filter List of filter parameters (set to \code{NULL} to deactivate filter). 
#' 		  See "Details"
#' 
#' @return An object of type \code{WaveData}
#' 
#' @details
#'
#' The wavefile is filtered upon loading using an elliptic bandpass filter.
#' The filter parameters are passed as a list within the \code{filter} 
#' argument, which must contain the following elements. Set the \code{filter}
#' argument to \code{NULL} to deactivate the bandpass filter. 
#' 
#' Default values are based on the telephony voice band 
#' 
#' \itemize{
#' 		\item{low}{Low border (in Hz) of the bandpass region. Default 300Hz}
#' 		\item{high}{High border (in Hz) of the bandpass region. Default 4kHz}
#' 		\item{Rp}{Maximum ripple in the bandpass region (in dB). Default to 0.001dB}
#' 		\item{Rs}{Attenuation within the stop bands (in dB). Default to 40dB}
#' 		\item{steepness}{Width of the transition region (in octaves). Default to 1octave} 
#' 	}
#' 
#' So for the default parameters the first stopband goes from 0Hz to 150Hz (1octave 
#' below 300Hz) with an attenuation of 40dB. Then there is a transition band from 
#' 150Hz to 300Hz and a passband from 300Hz to 4kHz. The next transition band ranges
#' from 4kHz to 8kHz and everything above 8kHz is again fully attenuated.
#' 
#' @export 
#' 
read.wav <- function(filename, channels=c("both","left","right"), 
		             filter=list(low=300, high=4000, Rp=0.001, Rs=40, steepness=1)) 
{
	channels <- match.arg(channels)
	
	w <- tuneR::readWave(filename)
	
	# Reduce stereo and Normalize to format independent of bit-depth
	s <- (if(w@stereo){ tuneR::mono(w,which=channels)@left} else {w@left}) / (2^w@bit) 
	f <- w@samp.rate
	
	l <- length(s)
	d <- l/f
	
	if(!is.null(filter)) {
		
		fp <- list(low=300, high=4000, Rp=0.001, Rs=40, steepness=1)
		fp[names(filter)] <- filter
		
		if(fp$low<0) {
			stop("Illegal value ", fp$low, " for lower passband border in filter parameter!")
		}
		if(fp$high <= fp$low ) {
			stop("Higher passband border ",fp$high," below lower passband border ",fp$low)
		}
		if(fp$high >= f/2) {
			stop("Higher passband border ", fp$high, " above Nyquist frequency ",f/2)
		}
		if(fp$Rp <= 0) {
			stop("Illegal passband ripple: ", fp$Rp)
		}
		if(fp$Rs <= 0 ) {
			stop("Illegal attenuation: ", fp$Rs)
		}
		if(fp$steepness <= 0) {
			stop("Illegal steepness: ", fp$steepness)
		}
		
		# Bandpass boundaries for filter
		# Filter design requires parameters in range [0,1], 
		# so we have to normalize by the nyquist
		nq <- f/2
		f.Bp.low  <- fp$low  / nq 
		f.Bp.high <- fp$high / nq
		
		# Band reject boundaries are steepness octaves below/above 
		# the Bp boundaries
		factor <- 2^fp$steepness
		f.Br.low  <- f.Bp.low  / factor
		f.Br.high <- f.Bp.high * factor
		
		if(f.Br.high > 1) {
			stop("Illegal upper stopband border: ", fp$high * factor, " (above Nyquist frequency of ", f/2, ")")
		}
		
		f.ord <- signal::ellipord(Wp=c(f.Bp.low,f.Bp.high), Ws=c(f.Br.low,f.Br.high), Rp=fp$Rp, Rs=fp$Rs)
		filt <- signal::ellip(f.ord)
		s <- signal::filtfilt(filt, s)
	}
	else {
		fp <- NULL
	}
	
	s <- stats::ts(s, start=0, end=d, frequency=f)
	
	r <- list(samples=s,
			  filename=filename,
			  duration=d,
			  tuneR.data = w,
			  filter=filt)
	class(r) <- append(class(r),"WaveData")
	attr(r,"params") <- list(channels=channels,filter=fp) 
	r
}
