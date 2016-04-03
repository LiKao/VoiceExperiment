# Functions for Formatting CSV output
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
expOnsets.as.csv <- function(dirname, filename, channels=c("both","left","right"), limit = 0.1, limit.type=c("absolute","relative"),
							 normalize=0.9, window.width=10, stepsize=5, window.function=signal::hanning, ... ) {
						 
	a <- analyse.directory(dirname=dirname, channels=channels, limit=limit, limit.type=limit.type, normalize=0.9,
			               window.width=window.width, stepsize=stepsize, window.function=window.function, quiet=FALSE)
				   
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
expOnsets.as.csv2 <- function(dirname, filename, channels=c("both","left","right"), limit = 0.1, limit.type=c("absolute","relative"),
		                      normalize=0.9,window.width=10, stepsize=5, window.function=signal::hanning, ... ) {
						  
	a <- analyse.directory(dirname=dirname, channels=channels, limit=limit, limit.type=limit.type, normalize=normalize,
			               window.width=window.width, stepsize=stepsize, window.function=window.function, quiet=FALSE)
				   
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
expOnsets.as.table <- function(dirname, filename, channels=c("both","left","right"), limit = 0.1, limit.type=c("absolute","relative"),
		                       normalize=0.9, window.width=10, stepsize=5, window.function=signal::hanning, ... ) {
						   
	a <- analyse.directory(dirname=dirname, channels=channels, limit=limit, limit.type=limit.type, normalize=normalize,
			               window.width=window.width, stepsize=stepsize, window.function=window.function, quiet=FALSE)
				   
	write.table(a, file = filename, ...)
} 
