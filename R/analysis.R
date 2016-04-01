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

#' Analyse a Single File for Onset Times
#'
#' @inheritParams read.wav
#' @inheritParams onsets
#' @inheritParams energyDensity
#' 
#' @export
analyse.file <- function( filename, channels=c("both","left","right"), 
						  limit = 0.1, window.width=10, stepsize=5, window.function=signal::hanning ) {
	
	channels <- match.arg(channels)
	w <- read.wav(filename, channels )
	r <- onsets.WaveData( w, limit=limit, window.width=window.width, stepsize=stepsize, window.function=window.function )
	p1 <- attr(r,"params")
	p2 <- attr(w,"params")
	attr(r,"params") <- append(p1,p2)
	r
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
						  	  limit = 0.1, window.width=10, stepsize=5, window.function=signal::hanning ) {
	if(!dir.exists(dirname)) {
		stop("Directory '",dirname,"' does not exist.")
	}
						  
	filenames <- list.files(dirname, pattern="\\.wav")
	fullnames <- paste(dirname,filenames,sep="/")
	r <- lapply(fullnames, analyse.file, channels=channels, limit=limit, window.width=window.width, 
			    stepsize=stepsize, window.function=window.function)
	r <- lapply(1:length(filenames), function(i){list(filename=filenames[[i]], onsets=r[[i]])})
	class(r) <- c("voiceExperimentData","list")
	r
}

#' @export
as.data.frame.voiceExperimentData <- function(x, ...) {
	max.onsets <- max(unlist(lapply(x, function(d){length(d$onsets)})))
	onsets <- do.call(rbind,lapply(x,function(d){as.matrix(d$onsets,padding=max.onsets)}))
	filenames <- unlist(lapply(x,function(d){d$filename}))
	cbind(data.frame(filename=filenames,stringsAsFactors=FALSE),as.data.frame(onsets))
}
