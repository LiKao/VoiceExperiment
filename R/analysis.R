# Functions for complete analysis of experimental data (files/directories)
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

#' Analyse WaveData for Onset Times
#'
#' This function mainly serves as a wrapper for calling the onset detection
#' function with a different parameter format.
#' 
#' @inheritParams onsets.WaveData
#' @param wavdata			WaveData object to be analysed
#' @param onset.params		Parameters passed to \code{\link{onsets}}
#' @param quiet				If set to true, the function will output the current filename to be analysed
#' 
#' @export
analyse.wav.onsets <- function( wavdata, onset.params=list(), energy.params=list(), quiet=TRUE ) 
{
	if(!quiet) {
		cat("\tExtracting onsets...\n")
	}
	
	do.call(onsets.WaveData, c(ts=list(wavdata), onset.params, energy.params=list(energy.params)))
}



#' Analyse a Single File for Onset Times
#'
#' @inheritParams onsets.WaveData
#' @param filename			Name of the file to be analyzed
#' @param filter			Filter parameters passed to \code{\link{read.wav}}
#' @param read.params		Parameters passed to \code{\link{read.wav}} (except filter parameters)
#' @param onset.params		Parameters passed to \code{\link{onsets}}
#' @param quiet				If set to true, the function will output the current filename to be analysed
#' 
#' @export
analyse.file <- function( filename, read.params=list(), filter=list(), onset.params=list(),energy.params=list(), quiet=TRUE ) 
{
	
	if(!quiet) {
		cat("Analysing file: ", filename,"\n")
	}
	
	w <- do.call(read.wav, c(filename, read.params, filter=list(filter)) )
	r <- list()
	r$onsets 	<- analyse.wav.onsets(w, onset.params, energy.params, quiet=quiet)
	r$filename 	<- filename 
		
	p1 <- attr(w, "params")
	p2 <- attr(r$onsets, "params")
	attr(r$onsets, "params") <- NULL
	
	attr(r,"params") <- list(read.params=p1,onset.params=p2)
	r
}


#' Analyse a Complete Directory for Onset Times
#' 
#' @inheritParams 	read.wav
#' @inheritParams 	onsets.WaveData
#' @inheritParams	analyse.file
#' @param dirname	Name of the directory from which the files should be taken 
#' @param quiet 	If set to true, the function will output the current filename to be analysed
#' 
#' @export
analyse.directory <- function(dirname, read.params=list(), filter=list(), onset.params=list(),energy.params=list(), quiet=TRUE ) 
{
	if(!dir.exists(dirname)) {
		stop("Directory '",dirname,"' does not exist.")
	}
						  
	filenames <- list.files(dirname, pattern="\\.wav")
	fullnames <- paste(dirname,filenames,sep="/")
	r <- lapply(fullnames, analyse.file, read.params=read.params, filter=filter, onset.params=onset.params, 
			    energy.params=energy.params, quiet=quiet)
	names(r) <- filenames
	class(r) <- c("voiceExperimentData","list")
	r
}

#' @export
as.data.frame.voiceExperimentData <- function(x, ...) {
	max.onsets <- max(unlist(lapply(x, function(d){length(d$onsets)})))
	onsets <- do.call(rbind,lapply(x,function(d){as.matrix(d$onsets,padding=max.onsets)}))
	filenames <- names(x)
	cbind(data.frame(filename=filenames,stringsAsFactors=FALSE),as.data.frame(onsets))
}
