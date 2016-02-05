# Functions for reading wave files into usable formats
# 
# Author: till
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
	s <- if(w@stereo){ tuneR::mono(w,which=channels)@left} else {w@left} / (2^w@bit) 
	
	l <- length(s)
	d <- l/w@samp.rate
	
	s <- ts(s, start=0, end=d, frequency=w@samp.rate)
	
	r <- list(samples=s,
			  filename=filename,
			  duration=d,
			  tuneR.data = w)
	class(r) <- append(class(r),"WaveData")
	r
}
