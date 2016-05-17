# Functions for calulation of MFCCs from wave data
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

freq.to.mel <- function(f) {
	if( any(f < 0) ) {
		stop("Negative frequency in freq.to.mel conversion")
	}
	2595*log10(1+f/700)
}

mel.to.freq <- function(m) {
	if( any(m < 0) ) {
		stop("Negative mel in mel.to.freq conversion")
	}
	700*(10^(m/2595)-1)
}

preemphasis <- function(ts, coeff) {
	if(coeff >=1 ) {
		stop("Preemphasis too large")
	}
	if(coeff < 0) {
		stop("Negative preemphasis value supplied to preemphasis filter")
	}
	ts-coeff*c(0,ts[1:(length(ts)-1)])
}

create.filter <- function(lower,top,upper,bins) 
{
	if( bins <= 0 ) {
		stop("invalid number of bins in creation of MFCC filter")
	}
	
	if( lower < 0 ) {
		stop("negative lower end of MFCC filter")
	}
	
	if( upper > bins ) {
		stop("upper end of MFCC filter larger than total number of bins")
	}
	
	if( lower > top ) {
		stop("lower end of MFCC filter is larger than top", lower, ">", top)
	}
	
	if( top > upper ) {
		stop("top of MFCC filter is larger than upper end: ", top, ">", upper)
	}
	
	left <- (1:bins-lower)/(top-lower)
	left <- ifelse(left<0 | left > 1, 0, left)
	
	right <- (upper-1:bins)/(upper-top)
	right <- ifelse(right< 0 | right >= 1, 0, right)
	
	left+right
}

create.filterbank <- function(lower,upper,filterbanks,frequency,fft.bins)
{
	if(lower > upper) {
		stop("Lower end of filterbank larger than upper end")
	}
	if(filterbanks <= 0) {
		stop("Invalid number of MFCC filterbanks")
	}
	if(frequency<=0) {
		stop("Invalid sampling frequency")
	}
	if(fft.bins <=0) {
		stop("Invalid number of FFT bins")
	}
	
	
	lower.mel <- freq.to.mel(lower)
	upper.mel <- freq.to.mel(upper)
	
	tops.mel <- seq(from=lower.mel, to=upper.mel, length.out=filterbanks+2)
	tops.freq <- mel.to.freq(tops.mel)
	
	tops.bins <- round(fft.bins*tops.freq/(frequency/2))+1
	
	filters.matrix <- matrix(tops.bins[c(1:(filterbanks),2:(filterbanks+1),3:(filterbanks+2))],ncol=3)
	
	apply(filters.matrix,1,function(v){create.filter(v[1],v[2],v[3],fft.bins)})
}

#' S3 Generic to calculate MFCCs (Mel Frequency Cepstral Coefficients) for a given time series
#' 
#' @param ts				The time series object
#' @param window.width		Window width for each MFCC vector (in ms)
#' @param stepsize			Stepsize ms samples by which each window is advanced
#' @param window.function	Windowing function for window extraction.
#' @param lower				Lowest frequency to be used for MFCC calculation (default 300Hz)
#' @param upper				Highest frequency to be used for MFCC calculation (default 8kHz)
#' @param filterbanks		Number of MFCC filterbanks to produce (default 26)
#' @param preemph			Strength of pre-emphasis filter applied to original signal (defaul 0.97). 
#' @param retain.coeffs		A vector of the coefficients to retain (starting a 0; default 1:13).
#' 							Set to \code{NULL} to keep all coefficients.
#' @param delta				Number of deltas (differences between subsequent vectors) to be computed 
#' @param ...				Additional parameters for future methods
#' 
#' @details 
#' 
#' The signal is sliced into windows each of the size determined by \code{window.width}. For each window
#' a MFCC vector is calculated. For each   							
#' 
#' @export 
MFCCs <- function(ts, window.width=25, stepsize=10,  window.function=signal::hanning, lower=300, 
				  upper=8000, filterbanks=26, preemph=0.97, retain.coeffs = 1:13, delta=2, ... )
{
	UseMethod("MFCCs")
}

MFCCs.WaveData <- function(ts, window.width=25, stepsize=10,  window.function=signal::hanning, 
		                   lower=300, upper=8000, filterbanks=26, preemph=0.97, 
						   retain.coeffs = 1:13, delta = 2, ... )
{
	if( any(retain.coeffs < 0) || any(retain.coeffs>=filterbanks)) {
		stop("Invalid coefficents to retain")
	}
	
	if( delta < 0 ) {
		stop("Invalid negative delta value")
	}
	
	ts.emph <- preemphasis(ts,preemph)
	
	spec <- spectrum(ts.emph, window.width=window.width, stepsize=stepsize, 
					 window.function=window.function)
	
	bins <- attr(spec,"bins")
	f <- frequency(ts)
	
	pw <- spec^2
	
	filters <- create.filterbank(lower=lower, upper=upper, filterbanks=filterbanks,
			                     frequency=f, fft.bins=bins)
	energies <- t(filters) %*% pw 
	
	log.energies <- log(energies)
		
	# Default type for MFCCs is DCT-II
	plan.dct = fftw::planDCT(n=filterbanks,type=2)
	r <- apply(log.energies,2,function(v){fftw::DCT(v,plan=plan.dct,type=2)})

	if(!is.null(retain.coeffs)) {
		r <- r[retain.coeffs+1,]
	}
	
	if( delta > 0) {
		deltas <- list(r)
		for(i in 1:delta) {
			d <- deltas[[i]][,1:ncol(r)] - cbind(deltas[[i]][,2:ncol(r)],rep(0,nrow(r)))
			d[,ncol(r)-i+1] <- 0
			deltas[[i+1]] <- d
		}
		r <- as.matrix(do.call(rbind,deltas)[,1:(ncol(r)-delta)])
	}
	

	attr(r,"window.width") <- window.width
	attr(r,"stepsize") <- stepsize
	attr(r,"filterbanks") <- filterbanks
	attr(r,"delta") <- delta
	attr(r,"retain.coeffs") <- if(is.null(retain.coeffs)) {1:filterbanks} else {retain.coeffs}
	attr(r,"time") <- attr(spec,"time")[1:ncol(r)]
	attr(r,"start") <- attr(spec,"start")
	attr(r,"end") <- attr(spec,"end")
	class(r) <- append("MFCCs",class(r))
	r
}
