#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
indir   <- args[[1]]
outfile <- args[[2]]


devtools::load_all(".")

expResponses.as.csv(dirname=indir,filename=outfile,stoptime=0.200, nresponses=2)
