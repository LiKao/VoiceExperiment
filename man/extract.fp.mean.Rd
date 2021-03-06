% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fingerprinting.R
\name{extract.fp.mean}
\alias{extract.fp.mean}
\title{Extract Fingerprints by Averaging Across the Feature Series}
\usage{
extract.fp.mean(fs)
}
\arguments{
\item{fs}{The feature time series from which the fingerprints should be extracted}
}
\description{
Extract Fingerprints by Averaging Across the Feature Series
}
\details{
This method creates a fingerprint by averaging across all repeated measurements of the 
same feature. This removes any kind of time information but keeps differences in the
absolute intensity of the features intact.
}

