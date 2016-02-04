# Class implementation of WaveData S3 Class
# 
# Author: till
###############################################################################

#' @export
print.WaveData <- function(object, ..) {
	cat("WaveData Object:")
	cat(paste("\n\tNumber of Samples:", length(w$samples), sep="\t\t"))
	cat(paste("\n\tSampling Frequency:", frequency(w$samples),sep="\t\t"))
	cat(paste("\n\tDuration (seconds):", w$duration, sep="\t\t"))
	if( !is.null(w$filename) ) {
		cat(paste("\n\tOriginal Filename:", w$filename, sep="\t\t"))	
	}
}

#' @export
summary.WaveData <- function(object, ...) {
	print(object)
} 

#' @export
plot.WaveData <- function(object, ... ) {
	plot(object$samples, ylab="Intensity", xlab="Time (s)", ...)
}