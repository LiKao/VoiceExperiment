# Class implementation of Spectrum S3 class
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

#' S3 Generic Function to Create a Spectrum From a Time Series
#' 
#' @param x 				Time Series for which the spectrum should be 
#' 							calculated
#'
#' @param window.width 		Width of each spectrum arrea (in ms)
#'
#' @param stepsize 			Stepsize for the windows (in ms)
#' 
#' @param padding			If this is set to TRUE (default), the windows are
#' 							zero padded to the next power of two for efficiency 
#'
#' @param window.function 	Windowing function to be applied to the 
#' 							signal frames
#' 
#' @param ...				Passed to future methods.
#' 
#' @return A matrix of the spectrum data with one time step in each column
#' 
#' @export
spectrum <- function(x, window.width, stepsize, padding=TRUE, window.function=signal::hanning, ...)
{
	UseMethod("spectrum")
}

#' @importFrom stats time
#' @export
time.spectrum <- function(x, offset = 0, ...)
{
	if(offset < 0 || offset > 1) {
		stop("Illegal offset value: ", offset)
	}
	
	attr(x, "time") + attr(x, "window.width")/1000 * offset
}

#' @importFrom stats start
#' @export
start.spectrum <- function(x, ...) 
{
	attr(x, "start")
}

#' @importFrom stats end
#' @export
end.spectrum <- function(x, ...)
{
	attr(x, "end")
}

#' @export
plot.spectrum <- function(x, log="y", scale=c("db","energy","amplitude"), ...) 
{
	bins <- attr(x, "bins")
	scale <- match.arg(scale)
	
	mar <- par("mar")
		
	sp.scaled <- switch(scale, db=10*log10(x^2), energy=x^2, amplitude=x)
	
	# frequencies from FFT represent the center frequencies of the bins,
	# but we need the boundaries
	freqs <- attr(x,"frequency")
	freqs <- c((freqs[2:bins]+freqs[1:(bins-1)])/2,freqs[bins])
	
	pal <- colorRampPalette(c("black","red","yellow"), space="rgb")(100)
	
	par(fig=c(0,0.8,0,1))
	smar <- mar
	smar[4] <- 0
	par(mar=smar)
	
	image( x=c(time(x)[1],time(x,offset=1)), 
			y=freqs[1:bins], z=t(sp.scaled[2:bins,]), 
			log=log, xlab="Time",ylab="Frequency", col=pal, ...)
	
	par(fig=c(0.8,1,0,1), new=TRUE)
	cmar <- mar
	cmar[1] <- cmar[1] + 1
	cmar[2] <- 1
	cmar[3] <- cmar[3] + 1
	cmar[4] <- cmar[4] + 2
	par(mar=cmar)
	
	plot(1,1,t="n",xlim=c(0,1),ylim=c(0,100),xaxt="n",yaxt="n",
		 xaxs="i",yaxs="i", xlab="", ylab="",...)
 
 	min.value <- min(sp.scaled)
	max.value <- max(sp.scaled)

	min.db <- switch(scale, db 			=min.value, 
							energy		=10*log10(min.value), 
							amplitude	=10*log10(min.value^2))
	
	max.db <- switch(scale, db 			=max.value, 
							energy		=10*log10(max.value), 
							amplitude	=10*log10(max.value^2))
	
	min.db.axis <- ceiling(min.db/10)*10
	max.db.axis <- floor(max.db/10)*10
	
	min.axis <- switch(scale, 	db			=min.db.axis,
								energy		=10^(min.db.axis/10),
								amplitude	=10^(min.db.axis/20))
								
	max.axis <- switch(scale, 	db			=max.db.axis,
								energy		=10^(max.db.axis/10),
								amplitude	=10^(max.db.axis/20))
	
	pos.value <- seq(from=min.axis,to=max.axis,length.out=10)
	pos.axis <- (pos.value-min.value)/(max.value-min.value)*100	
	
	lbl.db <- switch(scale, db			=round(pos.value),
							energy		=round(10*log10(pos.value)), 
							amplitude	=round(10*log10(pos.value^2)))
	
 	axis(4,at=pos.axis, labels=paste(lbl.db,"dB",sep=""), las=2)
 	for(i in 0:100) {
		polygon(c(0,0,1,1), c(i-1,i,i,i-1), col=pal[i], border=NA)
	}
 	par(mar=mar)

}

#' @export
print.spectrum <- function(x, ... ) 
{
	cat("Spectrum Data:")
	bins <- attr(x,"bins")
	cat(paste("\n\tNumber of Bins:", bins, sep="\t\t\t"))
	maxfreq <- tail(attr(x, "frequency"),n=1)
	cat(paste("\n\tMaximum Frequency:", maxfreq, sep="\t\t"))
	
	window.width 			<- attr(x, "window.width")
	window.width.ms 		<- paste(window.width,"ms",sep="")
	cat(paste("\n\twindow.width:\t\t", window.width.ms,  sep="\t"))
	
	stepsize 			<- attr(x,"stepsize")
	stepsize.ms			<- paste(stepsize,"ms",sep="")
	cat(paste("\n\tstepsize:\t\t", stepsize.ms, sep="\t\t"))
	
	cat(paste("\n\tStart time:\t\t", start(x), sep="\t\t"))
	cat(paste("\n\tEnd time:\t\t", end(x), sep="\t\t"))
	cat(paste("\n\tNumber of Windows:", ncol(x), sep="\t\t"))
	cat("\n")
}

