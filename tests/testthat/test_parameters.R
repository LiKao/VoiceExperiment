# test file for propagation of parameters to final analysis functions
#
# Added in Version 0.1.2 as test for bugfix
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

context("Parameters")

# TODO: Split this up in multiple files

###############################################

test_that("Illegal Paramters produce errors", {
			
	### Prepare test data
	
	illegalfile <- "../testdata/illegal.wav"
	testfile	<- "../testdata/silence_50ms_mono.wav"

	illegaldir	<- "../testdata/illegal/"
	testdir		<- "../testdata/testsets"
	
	w <- read.wav(testfile)
	e <- energyDensity(w)
			
	### read.wav
	
	expect_error( read.wav(illegalfile), 				"File '[^']*' does not exist.")
	expect_error( read.wav(testfile,channels="none"),	"'arg' should be one of \"both\", \"left\", \"right\"")
		  
	### energyDensity
	
	expect_error(   energyDensity(w, window.width=-10, stepsize=5,   normalize=0.9),  "Illegal window width: -10")
	expect_error(   energyDensity(w, window.width=0,   stepsize=5,   normalize=0.9),  "Illegal window width: 0")
	expect_error(   energyDensity(w, window.width=10,  stepsize=-10, normalize=0.9),  "Illegal stepsize: -10")
	expect_error(   energyDensity(w, window.width=10,  stepsize=0,   normalize=0.9),  "Illegal stepsize: 0")
	expect_warning( energyDensity(w, window.width=10,  stepsize=15,  normalize=0.9),  "Stepsize 15 is larger than window width 10")
	expect_error(   energyDensity(w, window.width=10,  stepsize=5,   normalize=-1.5), "Illegal normalization value: -1.5")
	expect_error(   energyDensity(w, window.width=10,  stepsize=5,   normalize=-0.5), "Illegal normalization value: -0.5")
	expect_error(   energyDensity(w, window.width=10,  stepsize=5,   normalize= 1.5), "Illegal normalization value: 1.5")
	
	### energyDensity.WaveData
	
	expect_error(   energyDensity.WaveData(w, window.width=-10, stepsize=5,   normalize=0.9),  "Illegal window width: -10")
	expect_error(   energyDensity.WaveData(w, window.width=0,   stepsize=5,   normalize=0.9),  "Illegal window width: 0")
	expect_error(   energyDensity.WaveData(w, window.width=10,  stepsize=-10, normalize=0.9),  "Illegal stepsize: -10")
	expect_error(   energyDensity.WaveData(w, window.width=10,  stepsize=0,   normalize=0.9),  "Illegal stepsize: 0")
	expect_warning( energyDensity.WaveData(w, window.width=10,  stepsize=15,  normalize=0.9),  "Stepsize 15 is larger than window width 10")
	expect_error(   energyDensity.WaveData(w, window.width=10,  stepsize=5,   normalize=-1.5), "Illegal normalization value: -1.5")
	expect_error(   energyDensity.WaveData(w, window.width=10,  stepsize=5,   normalize=-0.5), "Illegal normalization value: -0.5")
	expect_error(   energyDensity.WaveData(w, window.width=10,  stepsize=5,   normalize= 1.5), "Illegal normalization value: 1.5")
	
	### onsets
	
	expect_error( onsets(w, limit=-2,   limit.type="absolute"), 	"Illegal limit value: -2" )
	expect_error( onsets(w, limit=-1,   limit.type="absolute"), 	"Illegal limit value: -1" )
	expect_error( onsets(w, limit=-0.1, limit.type="absolute"), 	"Illegal limit value: -0.1" )
	expect_error( onsets(w, limit= 0,   limit.type="absolute"), 	"Illegal limit value: 0" )
	expect_error( onsets(w, limit= 1,   limit.type="absolute"), 	"Illegal limit value: 1" )
	expect_error( onsets(w, limit= 2,   limit.type="absolute"), 	"Illegal limit value: 2" )
	
	expect_error( onsets(e, limit=-2,   limit.type="absolute"), 	"Illegal limit value: -2" )
	expect_error( onsets(e, limit=-1,   limit.type="absolute"), 	"Illegal limit value: -1" )
	expect_error( onsets(e, limit=-0.1, limit.type="absolute"), 	"Illegal limit value: -0.1" )
	expect_error( onsets(e, limit= 0,   limit.type="absolute"), 	"Illegal limit value: 0" )
	expect_error( onsets(e, limit= 1,   limit.type="absolute"), 	"Illegal limit value: 1" )
	expect_error( onsets(e, limit= 2,   limit.type="absolute"), 	"Illegal limit value: 2" )
	
	### onsets.WaveData
	
	expect_error(   onsets.WaveData(w, limit=-2,   limit.type="absolute"), 	"Illegal limit value: -2" )
	expect_error(   onsets.WaveData(w, limit=-1,   limit.type="absolute"), 	"Illegal limit value: -1" )
	expect_error(   onsets.WaveData(w, limit=-0.1, limit.type="absolute"), 	"Illegal limit value: -0.1" )
	expect_error(   onsets.WaveData(w, limit= 0,   limit.type="absolute"), 	"Illegal limit value: 0" )
	expect_error(   onsets.WaveData(w, limit= 1,   limit.type="absolute"), 	"Illegal limit value: 1" )
	expect_error(   onsets.WaveData(w, limit= 2,   limit.type="absolute"), 	"Illegal limit value: 2" )
	
	expect_error(   onsets.WaveData(w, energy.params=list(window.width=-10, stepsize=  5, normalize=0.9)), 	"Illegal window width: -10")
	expect_error(   onsets.WaveData(w, energy.params=list(window.width=  0, stepsize=  5, normalize=0.9)), 	"Illegal window width: 0")
	expect_error(   onsets.WaveData(w, energy.params=list(window.width= 10, stepsize=-10, normalize=0.9)), 	"Illegal stepsize: -10")
	expect_error(   onsets.WaveData(w, energy.params=list(window.width= 10, stepsize=  0, normalize=0.9)), 	"Illegal stepsize: 0")
	expect_warning( onsets.WaveData(w, energy.params=list(window.width= 10, stepsize= 15, normalize=0.9)),	"Stepsize 15 is larger than window width 10")
	expect_error(   onsets.WaveData(w, energy.params=list(window.width= 10, stepsize=  5, normalize=-1.5)), "Illegal normalization value: -1.5")
	expect_error(   onsets.WaveData(w, energy.params=list(window.width= 10, stepsize=  5, normalize=-0.5)), "Illegal normalization value: -0.5")
	expect_error(   onsets.WaveData(w, energy.params=list(window.width= 10, stepsize=  5, normalize=1.5)), 	"Illegal normalization value: 1.5")
	
	### onsets.energyDensity
	
	expect_error( onsets.energyDensity(e, limit=-2,   limit.type="absolute"), 	"Illegal limit value: -2" )
	expect_error( onsets.energyDensity(e, limit=-1,   limit.type="absolute"), 	"Illegal limit value: -1" )
	expect_error( onsets.energyDensity(e, limit=-0.1, limit.type="absolute"), 	"Illegal limit value: -0.1" )
	expect_error( onsets.energyDensity(e, limit= 0,   limit.type="absolute"), 	"Illegal limit value: 0" )
	expect_error( onsets.energyDensity(e, limit= 1,   limit.type="absolute"), 	"Illegal limit value: 1" )
	expect_error( onsets.energyDensity(e, limit= 2,   limit.type="absolute"), 	"Illegal limit value: 2" )
	
	
	### analyse.file
	
	expect_error( analyse.file( illegalfile, 	channels="both"), "File '[^']*' does not exist.")
	expect_error( analyse.file( testfile,		channels="none"), "'arg' should be one of \"both\", \"left\", \"right\"")
	
	expect_error( analyse.file( testfile, channels="both", onset.params=list(limit = -2,	limit.type="absolute")), "Illegal limit value: -2" )
	expect_error( analyse.file( testfile, channels="both", onset.params=list(limit = -1,	limit.type="absolute")), "Illegal limit value: -1" )
	expect_error( analyse.file( testfile, channels="both", onset.params=list(limit = -0.1, 	limit.type="absolute")), "Illegal limit value: -0.1" )
	expect_error( analyse.file( testfile, channels="both", onset.params=list(limit =  0,	limit.type="absolute")), "Illegal limit value: 0" )
	expect_error( analyse.file( testfile, channels="both", onset.params=list(limit =  1,	limit.type="absolute")), "Illegal limit value: 1" )
	expect_error( analyse.file( testfile, channels="both", onset.params=list(limit =  2,	limit.type="absolute")), "Illegal limit value: 2" )
		  
	expect_error(	analyse.file( testfile, channels="both", energy.params=list(normalize= 0.9, window.width=-10, stepsize=  5)), "Illegal window width: -10")
	expect_error(	analyse.file( testfile, channels="both", energy.params=list(normalize= 0.9, window.width=  0, stepsize=  5)), "Illegal window width: 0")
	expect_error(	analyse.file( testfile, channels="both", energy.params=list(normalize= 0.9, window.width= 10, stepsize=-10)), "Illegal stepsize: -10")
	expect_error(	analyse.file( testfile, channels="both", energy.params=list(normalize= 0.9, window.width= 10, stepsize=  0)), "Illegal stepsize: 0")
	expect_warning(	analyse.file( testfile, channels="both", energy.params=list(normalize= 0.9, window.width= 10, stepsize= 15)), "Stepsize 15 is larger than window width 10")
	expect_error(	analyse.file( testfile, channels="both", energy.params=list(normalize=-1.5, window.width= 10, stepsize=  5)), "Illegal normalization value: -1.5")
	expect_error(	analyse.file( testfile, channels="both", energy.params=list(normalize=-0.5, window.width= 10, stepsize=  5)), "Illegal normalization value: -0.5")
	expect_error(	analyse.file( testfile, channels="both", energy.params=list(normalize= 1.5, window.width= 10, stepsize=  5)), "Illegal normalization value: 1.5")
	
	
	### analyse.directory
	
	expect_error( analyse.directory( illegaldir, 	channels="both"), "Directory '[^']*' does not exist.")
	expect_error( analyse.directory( testdir,		channels="none"), "'arg' should be one of \"both\", \"left\", \"right\"")
	
	expect_error( analyse.directory( testdir, channels="both", onset.params=list(limit = -2,   limit.type="absolute")), "Illegal limit value: -2" )
	expect_error( analyse.directory( testdir, channels="both", onset.params=list(limit = -1,   limit.type="absolute")), "Illegal limit value: -1" )
	expect_error( analyse.directory( testdir, channels="both", onset.params=list(limit = -0.1, limit.type="absolute")), "Illegal limit value: -0.1" )
	expect_error( analyse.directory( testdir, channels="both", onset.params=list(limit =  0,   limit.type="absolute")), "Illegal limit value: 0" )
	expect_error( analyse.directory( testdir, channels="both", onset.params=list(limit =  1,   limit.type="absolute")), "Illegal limit value: 1" )
	expect_error( analyse.directory( testdir, channels="both", onset.params=list(limit =  2,   limit.type="absolute")), "Illegal limit value: 2" )
	
	expect_error(	analyse.directory( testdir, channels="both", energy.params=list(normalize= 0.9, window.width=-10, stepsize=  5)), "Illegal window width: -10")
	expect_error(	analyse.directory( testdir, channels="both", energy.params=list(normalize= 0.9, window.width=  0, stepsize=  5)), "Illegal window width: 0")
	expect_error(	analyse.directory( testdir, channels="both", energy.params=list(normalize= 0.9, window.width= 10, stepsize=-10)), "Illegal stepsize: -10")
	expect_error(	analyse.directory( testdir, channels="both", energy.params=list(normalize= 0.9, window.width= 10, stepsize=  0)), "Illegal stepsize: 0")
	expect_warning(	analyse.directory( testdir, channels="both", energy.params=list(normalize= 0.9, window.width= 10, stepsize= 15)), "Stepsize 15 is larger than window width 10")
	expect_error(	analyse.directory( testdir, channels="both", energy.params=list(normalize=-1.5, window.width= 10, stepsize=  5)), "Illegal normalization value: -1.5")
    expect_error(	analyse.directory( testdir, channels="both", energy.params=list(normalize=-0.5, window.width= 10, stepsize=  5)), "Illegal normalization value: -0.5")
    expect_error(	analyse.directory( testdir, channels="both", energy.params=list(normalize= 1.5, window.width= 10, stepsize=  5)), "Illegal normalization value: 1.5")
})

#############################################################

test_that("Parameters are propagated during file analysis", {
			
	testfile	<- "../testdata/silence_50ms_mono.wav"
	
	###### read.wav
	
	### channels = both
				
	o1 <- analyse.file( testfile, channels="both" )
	expect_equal(attr(o1,"params")$channels, 		"both")
	
	### channels = left
	
	o1 <- analyse.file( testfile, channels="left")
	expect_equal(attr(o1,"params")$channels, 		"left")
	
	### channels = right
	
	o1 <- analyse.file( testfile, channels="right")
	expect_equal(attr(o1,"params")$channels, 		"right")
	
	###### onsets

	### limit = 0.5
	
	o1 <- analyse.file( testfile, onset.params=list(limit = 0.5, limit.type="absolute"))
	expect_equal(attr(o1,"params")$limit,			0.5)
	expect_equal(attr(o1,"params")$limit.type,		"absolute")
	
	### limit.type = "relative"
	
	o1 <- analyse.file( testfile, onset.params=list(limit = 0.1, limit.type="relative"))
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$limit.type,		"relative")
	
	### All parameters
	
	o1 <- analyse.file( testfile, onset.params=list(limit = 0.5, limit.type="relative"))
	expect_equal(attr(o1,"params")$limit,			0.5)
	expect_equal(attr(o1,"params")$limit.type,		"relative")

	###### energy.params
	
	### window.witdht = 20
	
	o1 <- analyse.file( testfile, energy.params=list(window.width=20, stepsize=5, normalize=0.9))
	expect_equal(attr(o1,"params")$energy.params$window.width,	20)
	expect_equal(attr(o1,"params")$energy.params$stepsize,		5)
	expect_equal(attr(o1,"params")$energy.params$normalize,		0.9)
	
	### stepsize = 3
	
	o1 <- analyse.file( testfile, energy.params=list(window.width=10, stepsize=3, normalize=0.9))

	expect_equal(attr(o1,"params")$energy.params$window.width,	10)
	expect_equal(attr(o1,"params")$energy.params$stepsize,		3)
	expect_equal(attr(o1,"params")$energy.params$normalize,		0.9)
	
	### normalize = 0.7
	
	o1 <- analyse.file( testfile, energy.params=list(window.width=10, stepsize=5, normalize=0.7))
	expect_equal(attr(o1,"params")$energy.params$window.width,	10)
	expect_equal(attr(o1,"params")$energy.params$stepsize,		5)
	expect_equal(attr(o1,"params")$energy.params$normalize,		0.7)
	
	### All parameters
	
	o1 <- analyse.file( testfile, energy.params=list(window.width=20, stepsize=3, normalize=0.7))
	expect_equal(attr(o1,"params")$energy.params$window.width,  20)
	expect_equal(attr(o1,"params")$energy.params$stepsize,		3)
	expect_equal(attr(o1,"params")$energy.params$normalize,		0.7)
})

##################################################################

test_that("Parameters are propagated during directory analysis", {
			
		testdir		<- "../testdata/testsets"
		
		###### read.wav
		
		### channels = both
			
		os <- analyse.directory( testdir, channels="both")
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$channels, 		"both")
		}
		
		
		### channels = left
		
		os <- analyse.directory( testdir, channels="left")
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$channels, 		"left")
		}
		
		### channels = right
		
		os <- analyse.directory( testdir, channels="right")
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$channels, 		"right")
		}
		
		###### onsets
		
		### limit = 0.5
		
		os <- analyse.directory( testdir, onset.params=list(limit = 0.5, limit.type="absolute"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.5)
			expect_equal(attr(o$onsets,"params")$limit.type,	"absolute")
		}
		
		### limit.type = "relative"
		
		os <- analyse.directory( testdir, onset.params=list(limit = 0.1, limit.type="relative"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.1)
			expect_equal(attr(o$onsets,"params")$limit.type,	"relative")
		}
		
		### All parameters changed
		
		os <- analyse.directory( testdir, onset.params=list(limit = 0.5, limit.type="relative"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.5)
			expect_equal(attr(o$onsets,"params")$limit.type,	"relative")
		}
		
		##### energy.params
		
		### window.witdth = 20
		
		os <- analyse.directory( testdir, energy.params=list(window.width=20, stepsize=5, normalize=0.9))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$energy.params$window.width,	20)
			expect_equal(attr(o$onsets,"params")$energy.params$stepsize,		5)
			expect_equal(attr(o$onsets,"params")$energy.params$normalize,		0.9)
		}
		
		### stepsize = 3
		
		os <- analyse.directory( testdir, energy.params=list(window.width=10, stepsize=3, normalize=0.9))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$energy.params$window.width,	10)
			expect_equal(attr(o$onsets,"params")$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$energy.params$normalize,		0.9)
		}
		
				
		### normalize = 0.7
		
		os <- analyse.directory( testdir, energy.params=list(window.width=10, stepsize=3, normalize=0.7))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$energy.params$window.width,	10)
			expect_equal(attr(o$onsets,"params")$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$energy.params$normalize,		0.7)
		}
		
		
		### All parameters changed
		
		os <- analyse.directory( testdir, energy.params=list(window.width=20, stepsize=3, normalize=0.7))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$energy.params$window.width,	20)
			expect_equal(attr(o$onsets,"params")$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$energy.params$normalize,		0.7)
		}
})

