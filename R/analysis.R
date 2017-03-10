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
	
	do.call(onsets, c(ts=list(wavdata), onset.params, energy.params=list(energy.params)))
}

#' Analyse WaveData for Fingerprints
#'
#' @param wavdata			WaveData object to be analysed
#' @param onsets			List of onset objects, from which the start and stop times 
#' 							for the extraction should be taken
#' @param fp.params			Parameters passed to \code{\link{fingerprint}}
#' @param stoptime			Stoptime to filter onsets in fingerprinting
#' @param duration			Explicit duration of the part to be fingerprinted
#' @param quiet				If set to true, the function will output the current filename to be analysed
#' 
#' @details
#' 
#' Fingerprints are extracted from the detected onset area with the largest energy. If a 
#' stoptime is given onsets before the stoptime are ignored. End times can be overridden by
#' providing an explicit duration in the duration parameter.
#' 
#' @export
analyse.wav.fingerprint <- function( wavdata, onsets, fp.params=list(), stoptime=NULL, 
									 duration=NULL, quiet=TRUE)
{
	if(!quiet) {
		cat("\tExtracting fingerprints...\n")
	}
	
	if(length(onsets)==1 &&  is.na(onsets)) {
		# We redirect this to fingerprint, so we are sure
		# to always get the correct class
		r <- do.call(fingerprint, c(ts=list(NA), start=0, end=0, fp.params))
		return(r)
	}
	
	if(!is.null(stoptime) && duration(wavdata) <= stoptime) {
		r <- do.call(fingerprint, c(ts=list(NA), start=0, end=0, fp.params))
		return(r)
	}
	
	max.o = NULL
	for(o in onsets) {
		if(is.null(stoptime) || o$start > stoptime) {
			if(is.null(max.o) || max.o$energy.avg < o$energy.avg) {
				max.o <- o
			}
		}
	}
	
	if(is.null(max.o)) {
		warning("No suitable onset found for fingerprinting")
		r <- do.call(fingerprint, c(ts=list(NA), start=0, end=0, fp.params))
		return(r)
	} 
	
	start <- max.o$start
	end   <- max.o$end
	if(!is.null(duration)) {
		end <- start + duration
	}
	
	do.call(fingerprint, c(ts=list(wavdata), start=start, end=end, fp.params))
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
analyse.file.onsets <- function( filename, read.params=list(), filter=list(), onset.params=list(),energy.params=list(), quiet=TRUE ) 
{
	
	if(!quiet) {
		cat("Analysing file: ", filename,"\n")
	}
	
	w <- do.call(read.wav, c(filename, read.params, filter=list(filter)) )
	r <- list()
	r$onsets 		<- analyse.wav.onsets(w, onset.params, energy.params, quiet=quiet)
	r$filename 		<- filename 
	
	p1 <- attr(w, "params")
	p2 <- attr(r$onsets, "params")
	
	attr(r,"params") <- list(read.params=p1,onset.params=p2)
	r
}


#' Analyse a Single File for Onset Times and Fingerprint
#'
#' @inheritParams onsets.WaveData
#' @inheritParams analyse.wav.fingerprint
#' @param filename			Name of the file to be analyzed
#' @param filter			Filter parameters passed to \code{\link{read.wav}}
#' @param read.params		Parameters passed to \code{\link{read.wav}} (except filter parameters)
#' @param onset.params		Parameters passed to \code{\link{onsets}}
#' @param quiet				If set to true, the function will output the current filename to be analysed
#' 
#' @details
#' 
#' Fingerprints are extracted from the detected onset area with the largest energy. If a 
#' stoptime is given onsets before the stoptime are ignored. End times can be overridden by
#' providing an explicit duration in the duration parameter.
#' 
#' @export
analyse.file <- function( filename, read.params=list(), filter=list(), onset.params=list(),
						  energy.params=list(), fp.params=list(), stoptime=NULL, 
		duration=NULL, quiet=TRUE ) 
{
	
	if(!quiet) {
		cat("Analysing file: ", filename,"\n")
	}
	
	w <- do.call(read.wav, c(filename, read.params, filter=list(filter)) )
	r <- list()
	r$onsets 		<- analyse.wav.onsets(w, onset.params, energy.params, quiet=quiet)
	r$fingerprint	<- analyse.wav.fingerprint(w, r$onsets, fp.params=fp.params, 
						       stoptime=stoptime, duration=duration, quiet=quiet )
	r$filename 		<- filename 
		
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
analyse.directory.onsets <- function(dirname, read.params=list(), filter=list(), onset.params=list(),energy.params=list(), quiet=TRUE ) 
{
	if(!dir.exists(dirname)) {
		stop("Directory '",dirname,"' does not exist.")
	}
	
	filenames <- list.files(dirname, pattern="\\.wav")
	fullnames <- paste(dirname,filenames,sep="/")
	r <- lapply(fullnames, analyse.file.onsets, read.params=read.params, filter=filter, onset.params=onset.params, 
			energy.params=energy.params, quiet=quiet)
	names(r) <- filenames
	class(r) <- c("voiceExperimentData","list")
	r
}

analyse.clusters <- function(df, nresponses)
{
	
	m <- do.call(rbind,lapply(df, function(d) as.matrix(d$fingerprint)))
	c <- kmeans(na.omit(m), centers=nresponses)
	r <- data.frame( dist=matrix(rep(0,length(df)*nresponses),ncol=nresponses))
	dmin  <- rep(Inf,length(df))
	dbest <- rep(NA,length(df))
	for(i in 1:nresponses) {
		curr <- sqrt(colSums((t(m) - c$centers[i,])^2))
		filter <- curr < dmin
		dmin  <- ifelse(filter, curr, dmin)
		dbest <- ifelse(filter, i, dbest)
		r[,paste("dist",i,sep=".")] <- curr 
	}
	r$cluster <- dbest
	r
}

#' Analyse a Complete Directory for Onset Times, Fingerprints and clusters
#' 
#' @inheritParams 		read.wav
#' @inheritParams 		onsets.WaveData
#' @inheritParams		analyse.file
#' @param dirname		Name of the directory from which the files should be taken
#' @param nresponses	Number of response classes used in clustering. Set to \code{NULL} to 
#' 						deactivate cluster analysis 
#' @param quiet 		If set to true, the function will output the current filename to be analysed
#' 
#' @export
analyse.directory <- function(dirname, read.params=list(), filter=list(), onset.params=list(),
							  energy.params=list(), fp.params=list(), stoptime=NULL, duration=NULL,
							  nresponses=NULL, quiet=TRUE ) 
{
	if(!dir.exists(dirname)) {
		stop("Directory '",dirname,"' does not exist.")
	}
						  
	filenames <- list.files(dirname, pattern="\\.wav")
	fullnames <- paste(dirname,filenames,sep="/")
	r <- lapply(fullnames, analyse.file, read.params=read.params, filter=filter, onset.params=onset.params, 
			    energy.params=energy.params, fp.params=fp.params, stoptime=stoptime, duration=duration,
				quiet=quiet)
	if(!is.null(nresponses)) {
		c <- analyse.clusters(r, nresponses)
		for(i in 1:nrow(c)) {
			r[[i]]$response <- as.list(c[i,])
		}
	}
	names(r) <- filenames
	class(r) <- c("voiceExperimentData","list")
	r
}

#' @export
as.data.frame.voiceExperimentData <- function(x, ..., include.fp=FALSE) {
	max.onsets <- max(unlist(lapply(x, function(d){length(d$onsets)})))
	onsets <- do.call(rbind,lapply(x,function(d){as.matrix(d$onsets,padding=max.onsets)}))
	filenames <- names(x)
	r <- cbind(data.frame(filename=filenames,stringsAsFactors=FALSE),as.data.frame(onsets))
	if(include.fp) {
		fps <- do.call(rbind, lapply(x, function(d){as.data.frame(d$fingerprint)}))
		r <- cbind(r,fp=fps)
	}
	if( all(unlist(lapply(x, function(d) !is.null(d$response)))) ) {
		if(!include.fp) {
			fps <- do.call(rbind, lapply(x, function(d){as.data.frame(d$fingerprint)}))
			r <- cbind(r,fp=fps[,c("type","feature.type","start","end")])
		}
		cs <- do.call(rbind,lapply(x, function(d) as.data.frame(d$response)))
		r <- cbind(r, response=cs)
	}
	r$note <- unlist(lapply(x,function(o) ifelse(length(o$onsets) == 1 && is.na(o$onsets), attr(o$onsets,"Error"),"")))
	r
}
