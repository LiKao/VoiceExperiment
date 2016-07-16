# Class implementation of WaveData S3 Class
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

#' @export
duration.WaveData <- function(x, ... ) {
	attr(x,"duration")
}

#' @importFrom stats window
#' @export
window.WaveData <- function( x, ... ) {
	r <- NextMethod("window", x, ... )
	d <- length(r)/frequency(r)
	attr(r,"duration") <- d
	class(r) <- append("WaveData",class(r))
	r
}

	
#####
# Output Methods
#####
	
#' @export
print.WaveData <- function(x, ...) {
	cat("WaveData Object:")
	cat(paste("\n\tNumber of Samples:", length(x), sep="\t\t"))
	cat(paste("\n\tSampling Frequency:", frequency(x),sep="\t\t"))
	cat(paste("\n\tDuration (seconds):", duration(x), sep="\t\t"))
	if( !is.null(attr(x,"filename")) ) {
		cat(paste("\n\tOriginal Filename:", attr(x,"filename"), sep="\t\t"))	
	}
	cat("\n")
}

#' @export
summary.WaveData <- function(object, ...) {
	print(object)
} 

#' @export
slice.WaveData <- function(x, window.width, stepsize, window.function=signal::hanning, ... ) 
{
	duration.ms <- duration(x)*1000
	f <- frequency(x)
	l <- ceiling(window.width/1000*f)
	
	w <- window.function(l)
	w <- w/sum(w)
		
	starts <- (seq(from=window.width,to=duration.ms,by=stepsize)-window.width)/1000
	starts.samples <- round(starts*f)
	j <- do.call(c,lapply(starts.samples,FUN=function(v){seq(v,v+l-1)+1}))
	m <- matrix(x[j],nrow=l)*w	
	
	attr(m, "window.times") <- time(x)[starts.samples+1]
	attr(m, "window.width") <- window.width
	attr(m, "stepsize") <- stepsize
	attr(m, "frequency") <- f
	attr(m, "start") <- time(x)[starts.samples[1]+1]
	attr(m, "end")   <- time(x)[tail(starts.samples,n=1)+1]+window.width/1000
	class(m) <- append("tsSlice", class(m) )
	m
}

#' @export
plot.WaveData <- function(x, type=c("energy","intensity","spectogram","spectrum","spec"), 
                          window.width=10, stepsize=5, window.function=signal::hanning, ... ) 
{
  type <- match.arg(type)
  
  if(substr(type,1,4)=="spec") {
    sp <- spectrum(x, window.width=window.width, stepsize=stepsize, window.function=window.function)
	plot(sp,...)
  } 
  else {
	  NextMethod("plot", x, ylab="Intensity", xlab="Time (s)", ...)
  }
}


#' @export
spectrum.WaveData <- function(x, window.width, stepsize, padding=TRUE, window.function=signal::hanning, ...)
{
	s <- slice(x, window.width=window.width, stepsize=stepsize, window.function=window.function)
	
	if( padding ) {
		# We perform a zero padding to the next power of two 
		# to speed up FFT calculation
		p2 <- 2^ceiling(log2(nrow(s)))
	}
	else {
		p2 <- nrow(s)
	}

	m <- matrix(0,ncol=dim(s)[2],nrow=p2)
	m[1:dim(s)[1],1:dim(s)[2]] <- s

	p <- fftw::planFFT(p2)
	ff <- apply(m,2,function(v){fftw::FFT(v,plan=p)})

	r <- abs(ff[1:((p2/2+1)),])
	class(r) <- append("spectrum", class(r) )
	attr(r, "bins") <- p2/2+1
	attr(r, "time") <- time(s)
	attr(r, "window.width") <- window.width
	attr(r, "stepsize") <- stepsize
	attr(r, "start") <- start(s)
	attr(r, "end") <- end(s)
	attr(r, "frequency") <-  seq(from=0, to=frequency(x)/2,length.out=p2/2+1)
	r
}

