# Functions for Formatting CSV output
# 
# Author: till
###############################################################################

#' Analyse All Files from a Given Directory and Write Results as CSV file
#' 
#' This function uses the formating defined by \code{\link[utils]{write.csv}}
#' 
#' 
#' @inheritParams analyse.directory
#' @inheritParams read.wav
#' @inheritParams onsets
#' @inheritParams energyDensity
#' @param filename	The filename of the csv file to which the results should be written
#' @param ... Optional arguments passed to \code{\link[utils]{write.csv}}
#' 
#' @export
#' 
expOnsets.as.csv <- function(dirname, filename, channels=c("both","left","right"), 
							 limit = 0.1, window.width=10, stepsize=5, window.function=signal::boxcar,
							 ... ) {
	a <- analyse.directory(dirname, channels, limit, window.width, stepsize, window.function)
	write.csv(a, file = filename, ...)
}

#' Analyse All Files from a Given Directory and Write Results as CSV file
#' 
#' This function uses the formating defined by \code{\link[utils]{write.csv2}}
#' 
#' @inheritParams analyse.directory
#' @inheritParams read.wav
#' @inheritParams onsets
#' @inheritParams energyDensity
#' @param filename	The filename of the csv file to which the results should be written
#' @param ... Optional arguments passed to \code{\link[utils]{write.csv2}}
#' 
#' @export
#' 
expOnsets.as.csv2 <- function(dirname, filename, channels=c("both","left","right"), 
		limit = 0.1, window.width=10, stepsize=5, window.function=signal::boxcar,
		... ) {
	a <- analyse.directory(dirname, channels, limit, window.width, stepsize, window.function)
	write.csv2(a, file = filename, ...)
}

#' Analyse All Files from a Given Directory and Write Results as Tabular Format
#' 
#' @inheritParams analyse.directory
#' @inheritParams read.wav
#' @inheritParams onsets
#' @inheritParams energyDensity
#' @param filename	The filename of the csv file to which the results should be written
#' @param ... Optional arguments passed to \code{\link[utils]{write.table}}
#' 
#' @export
#' 
expOnsets.as.table <- function(dirname, filename, channels=c("both","left","right"), 
		limit = 0.1, window.width=10, stepsize=5, window.function=signal::boxcar,
		... ) {
	a <- analyse.directory(dirname, channels, limit, window.width, stepsize, window.function)
	write.table(a, file = filename, ...)
} 
