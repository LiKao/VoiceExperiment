# Functions for complete analysis of experimental data (files/directories)
# 
# Author: till
###############################################################################

#' Analyse a Single File for Onset Times
#'
#' @inheritParams read.wav
#' @inheritParams onsets
#' @inheritParams energyDensity
#' 
#' @export
analyse.file <- function( filename, channels=c("both","left","right"), 
						  limit = 0.1, window.width=10, stepsize=5, window.function=signal::boxcar ) {
	
	channels <- match.arg(channels)
	w <- read.wav(filename, channels )
	onsets.WaveData( w, limit, window.width, stepsize, window.function )
}


#' Analyse a Complete Directory for Onset Times
#' 
#' @param dirname	Name of the directory from which the files should be taken
#' @inheritParams read.wav
#' @inheritParams onsets
#' @inheritParams energyDensity
#' 
#' @export
analyse.directory <- function(dirname, channels=c("both","left","right"), 
						  	  limit = 0.1, window.width=10, stepsize=5, window.function=signal::boxcar ) {
	filenames <- list.files(dirname, pattern="\\.wav")
	fullnames <- paste(dirname,filenames,sep="/")
	r <- lapply(fullnames,analyse.file)
	r <- lapply(1:length(filenames), function(i){list(filename=filenames[[i]], onsets=r[[i]])})
	class(r) <- append(class(r),"voiceExperimentData")
	r
}
