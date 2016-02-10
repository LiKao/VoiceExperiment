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
