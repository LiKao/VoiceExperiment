# Class implementation of WaveData S3 Class
# 
# Author: till
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