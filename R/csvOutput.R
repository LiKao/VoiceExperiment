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
#' @inheritParams onsets
#' @inheritParams onsets.WaveData
#' @param filename	The filename of the csv file to which the results should be written
#' @param ... Optional arguments passed to \code{\link[utils]{write.csv}}
#' 
#' @export
#' 
expOnsets.as.csv <- function(dirname, filename, read.params=list(), filter=list(), onset.params=list(), energy.params=list(), ... ) 
{
						 
	a <- analyse.directory(dirname=dirname, read.params=read.params, filter=filter, onset.params=onset.params, 
			               energy.params=energy.params, quiet=FALSE)
				   
	write.csv(a, file = filename, ...)
}

#' Analyse All Files from a Given Directory and Write Results as CSV file
#' 
#' This function uses the formating defined by \code{\link[utils]{write.csv2}}
#' 
#' @inheritParams analyse.directory
#' @inheritParams read.wav
#' @inheritParams onsets.WaveData
#' @param filename	The filename of the csv file to which the results should be written
#' @param ... Optional arguments passed to \code{\link[utils]{write.csv2}}
#' 
#' @export
#' 
expOnsets.as.csv2 <- function(dirname, filename, read.params=list(), filter=list(), onset.params=list(), energy.params=energy.params, ... ) 
{
						  
	a <- analyse.directory(dirname=dirname, read.params=read.params, onset.params=onset.params, 
			               energy.params=energy.params, quiet=FALSE)
				   
	write.csv2(a, file = filename, ...)
}

#' Analyse All Files from a Given Directory and Write Results as Tabular Format
#' 
#' @inheritParams analyse.directory
#' @inheritParams read.wav
#' @inheritParams onsets.WaveData
#' @param filename	The filename of the csv file to which the results should be written
#' @param ... Optional arguments passed to \code{\link[utils]{write.table}}
#' 
#' @export
#' 
expOnsets.as.table <- function(dirname, filename, read.params=list(), filter=list(), onset.params=list(), energy.params=energy.params, ... ) 
{
						   
	a <- analyse.directory(dirname=dirname, read.params=read.params, filter=filter, onset.params=onset.params, 
			               energy.params=energy.params, quiet=FALSE)
				   
	write.table(a, file = filename, ...)
} 
