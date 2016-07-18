# Functions to generate fingerprints from time series of feature vectors
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

#' Extract Fingerprints from any Kind of Time series.
#' 
#' This provides a generic S3 method to extract fingerprints from any kind of time
#' series. The concrete type of possible fingerprints is determined by the type
#' of object.
#' 
#' @param 	ts		The time series from which fingerprints should be extracted
#' @param	...		Parameters for future methods
#' 
#'  
#' @export
fingerprint <- function(ts, ... ) 
{
	UseMethod("fingerprint")
}

#' Extract Fingerprints from a time series of feature vectors (e.g. spectrum or MFCC data)
#'
#' Simple wrapper, which calls the specified extraction function.
#' 
#' @param	ts		The feature time series from which the fingerprint should be extracted
#' @param	fp.type The type of extraction to be performed (see details)
#' @param	...		Parameters for future methods
#'
#' @details
#' 
#' This function calls the specified extraction function. For \code{type="minmax"} the
#' function \code{\link{extract.fp.minmax}} is called and for \code{type="mean"} the
#' \code{\link{extract.fp.mean}} function is called.
#' 
#' Fingerprints can be compared by (euclidean) distance to determine the similarity between
#' feature series given some invariants. The type of invariants is determined by the concrete
#' extraction method. 
#' 
#' @export
fingerprint.Features <- function(ts, fp.type = c("mean","minmax"), ...) {
	fp.type <- match.arg(fp.type)
	r <- switch(fp.type,
		   			minmax 	= extract.fp.minmax(ts),
		   			mean	= extract.fp.mean(ts) )
	class(r) <- append("fingerprint",class(r))
	attr(r, "feature.type") <- feature.type(ts)
	attr(r, "fp.type") <- fp.type
   	r
}

#' Extract Time and Strength Independent Fingerprints
#' 
#' @param fs		The feature time series from which the fingerprints should be extracted
#' 
#' @details
#' 
#' This function extracts fingerprints, which are somewhat invariant to stretching in the time
#' domain while keeping the order of features intact. Furthermore these fingerprints are
#' completely invariant to a scaling or offset of the complete feature series.
#' 
#' For each feature within the feature vectors, the times of maximum and minimum value are 
#' extracted. Since no values of the actual features are used, scaling and offset invariance
#' is achieved. Furthermore, the positions of minima and maxima are scaled relative to the
#' total length of the series. Therefore, these fingerprints are robust to stretching of
#' the complete time series. Local stretching of the feature series are translated to
#' movements in euclidean space.
#'
#' @export
extract.fp.minmax <- function(fs) {
	l <- ncol(fs)
	mins <- apply(fs, 1, function(f) which(f==min(f))/l)
	maxs <- apply(fs, 1, function(f) which(f==max(f))/l)
	c(mins,maxs)
}

#' Extract Fingerprints by Averaging Across the Feature Series
#' 
#' @param fs		The feature time series from which the fingerprints should be extracted
#' 
#' @details
#' 
#' This method creates a fingerprint by averaging across all repeated measurements of the 
#' same feature. This removes any kind of time information but keeps differences in the
#' absolute intensity of the features intact.
#' 
#' @export
extract.fp.mean <- function(fs) {
	apply(fs, 1, mean)
}

#' Extract Fingerprints for any Kind of ts Object
#' 
#' Fingerprints from ts objects are extracted by first extracting a feature
#' time series from the original time series and then running the appropriate
#' fingerprint extractor on this feature series.
#' 
#' @param	ts				The time series object from which the fingerprint should
#' 							be extracted
#' @param	feature.type	The type of features to be used in fignerprinting
#' @param	start			The start position at which to start extraction (see details)
#' @param	end				The end position at which to end extraction (see details)
#' @param	duration		The duration of the window to be extracted (see details)
#' @param	feature.params	Additional arguments for the feature extractor
#' @param	...				Arguments passed to fingerprinting function for the features
#' 
#' @details
#' 
#' The position of the wave file for which the fingerprint should be extracted can 
#' be set based either on start or end values or start and a duration.
#' 
#' @export
fingerprint.ts <- function(ts, feature.type=c("MFCCs", "MFCC", "spectrum"),
						   start=NULL, end=NULL, duration=NULL,
						   feature.params=list(), ...)
{
	feature.type <- match.arg(feature.type)
	
	if(!is.null(start)) {
		if( is.null(end) && is.null(duration)) {
			stop("Start time for fingerprinting specified but no end or duration is given")
		}
		
		if(!is.null(end) && !is.null(duration)) {
			stop("Both end as well as duration supplied")
		}
		
		if(!is.null(end) && end < start) {
			stop("End before start in fingerprinting")
		}
		
		if(is.null(end)) {
			end <- start + duration
		}
		
		ts.windowed <- window(ts, start=start, end=end)
	}
	else {
		ts.windowed <- ts
	}
	
	fs <- switch(feature.type,
			MFCCs 		= do.call(MFCCs, 	c(list(ts=ts.windowed), feature.params)),
			MFCC  		= do.call(MFCCs, 	c(list(ts=ts.windowed), feature.params)),
			spectrum 	= do.call(spectrum, c(list(x=ts.windowed),  feature.params)))
	
	r <- fingerprint.Features(fs, ...)
	if(!is.null(start)) {
		attr(r, "start") 	<- start
		attr(r, "end") 		<- end
	}
	r
}

#' @export
as.matrix.fingerprint <- function(x, ... ) 
{
	t(NextMethod("as.matrix", x))
}

#' @export
as.data.frame.fingerprint <- function(x, ...) {
	m <- as.matrix(x)
	r <- data.frame( type=attr(x,"fp.type"), feature.type=attr(x,"feature.type") )
	if(!is.null(attr(x, "start"))) {
		r$start <- attr(x, "start")
		r$end   <- attr(x, "end")
	}
	cbind(r,value=m)
}
