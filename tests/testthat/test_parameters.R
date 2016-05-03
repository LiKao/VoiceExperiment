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
	
	w <- read.wav("../testdata/silence_50ms_mono.wav")
	e <- energyDensity(w)
			
	### read.wav
	
	expect_error( read.wav("../testdata/illegal.wav"), "File '[^']*' does not exist.")
	expect_error( read.wav("../testdata/silence_50ms_mono.wav",channels="none"), 
			      "'arg' should be one of \"both\", \"left\", \"right\"")
		  
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
	
	expect_error( analyse.file( "../testdata/illegal.wav", channels="both", limit = 0.1, limit.type="absolute"), 			"File '[^']*' does not exist.")
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="none", limit = 0.1, limit.type="absolute"),	"'arg' should be one of \"both\", \"left\", \"right\"")
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit = -2, limit.type="absolute"),	"Illegal limit value: -2" )
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit = -1, limit.type="absolute"),	"Illegal limit value: -1" )
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit = -0.1, limit.type="absolute"),	"Illegal limit value: -0.1" )
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0, limit.type="absolute"),	"Illegal limit value: 0" )
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  1, limit.type="absolute"),	"Illegal limit value: 1" )
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  2, limit.type="absolute"),	"Illegal limit value: 2" )
		  
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0.1, limit.type="absolute",
					            energy.params=list(normalize=0.9, window.width=-10, stepsize=  5)), 
				  "Illegal window width: -10")
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0.1, limit.type="absolute",
					            energy.params=list(normalize=0.9, window.width=  0, stepsize=  5)),
				  "Illegal window width: 0")
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0.1, limit.type="absolute",
					            energy.params=list(normalize=0.9, window.width= 10, stepsize=-10)),
			      "Illegal stepsize: -10")
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0.1, limit.type="absolute",
					            energy.params=list(normalize=0.9, window.width= 10, stepsize=  0)),
			      "Illegal stepsize: 0")
	expect_warning( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0.1, limit.type="absolute",
					              energy.params=list(normalize=0.9, window.width=10, stepsize=15)),
			        "Stepsize 15 is larger than window width 10")
			
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0.1, limit.type="absolute",
					            energy.params=list(window.width= 10, stepsize=5, normalize=-1.5)),
				  "Illegal normalization value: -1.5")
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0.1, limit.type="absolute",
					            energy.params=list(window.width= 10, stepsize=5, normalize=-0.5)),
  				  "Illegal normalization value: -0.5")
	expect_error( analyse.file( "../testdata/silence_50ms_mono.wav",channels="both", limit =  0.1, limit.type="absolute",
					            energy.params=list(window.width= 10, stepsize=5, normalize=1.5)),
  				  "Illegal normalization value: 1.5")
	
	
	### analyse.directory
	
	expect_error( analyse.directory( "../testdata/illegal/", channels="both", limit = 0.1, limit.type="absolute"),	"Directory '[^']*' does not exist.")
	expect_error( analyse.directory( "../testdata/testsets",channels="none", limit = 0.1, limit.type="absolute"),	"'arg' should be one of \"both\", \"left\", \"right\"")
	
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit = -2, limit.type="absolute"),	"Illegal limit value: -2" )
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit = -1, limit.type="absolute"),	"Illegal limit value: -1" )
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit = -0.1, limit.type="absolute"),	"Illegal limit value: -0.1" )
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  0, limit.type="absolute"),	"Illegal limit value: 0" )
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  1, limit.type="absolute"),	"Illegal limit value: 1" )
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  2, limit.type="absolute"),	"Illegal limit value: 2" )
	
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  0.1, limit.type="absolute",
					                 energy.params=list(normalize=0.9, window.width=-10, stepsize=  5)),
			      "Illegal window width: -10")
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  0.1, limit.type="absolute",
					                 energy.params=list(normalize=0.9, window.width=  0, stepsize=  5)),
			      "Illegal window width: 0")
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  0.1, limit.type="absolute",
					                 energy.params=list(normalize=0.9, window.width= 10, stepsize=-10)),
			      "Illegal stepsize: -10")
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  0.1, limit.type="absolute",
					                 energy.params=list(normalize=0.9, window.width= 10, stepsize=  0)),
			      "Illegal stepsize: 0")
	expect_warning( analyse.directory( "../testdata/testsets",channels="both", limit =  0.1, limit.type="absolute",
					                   energy.params=list(normalize=0.9, window.width=10, stepsize=15)),
			        "Stepsize 15 is larger than window width 10")
			
	expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  0.1, limit.type="absolute",
					                 energy.params=list(normalize=-1.5, window.width= 10, stepsize=  5)),
				  "Illegal normalization value: -1.5")
    expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  0.1, limit.type="absolute",
					                 energy.params=list(normalize=-0.5, window.width= 10, stepsize=  5)),
	              "Illegal normalization value: -0.5")
    expect_error( analyse.directory( "../testdata/testsets",channels="both", limit =  0.1, limit.type="absolute",
					                 energy.params=list(normalize=1.5, window.width= 10, stepsize=  5)),
	              "Illegal normalization value: 1.5")
})

#############################################################

test_that("Parameters are propagated during file analysis", {
	
	### All parameters at default values
				
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, limit.type="absolute")
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$channels, 		"both")
	expect_equal(attr(o1,"params")$limit.type,		"absolute")
	
	### channels = left
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="left", limit = 0.1, limit.type="absolute")
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$channels, 		"left")
	expect_equal(attr(o1,"params")$limit.type,		"absolute")
	
	### channels = right
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="right", limit = 0.1, limit.type="absolute")
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$channels, 		"right")
	expect_equal(attr(o1,"params")$limit.type,		"absolute")

	### limit = 0.5
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="both", limit = 0.5, limit.type="absolute")
	expect_equal(attr(o1,"params")$limit,			0.5)
	expect_equal(attr(o1,"params")$channels, 		"both")
	expect_equal(attr(o1,"params")$limit.type,		"absolute")
	
	### limit.type = "relative"
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, limit.type="relative")
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$channels, 		"both")
	expect_equal(attr(o1,"params")$limit.type,		"relative")
	
	### All parameters
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="left", limit = 0.5, limit.type="relative")
	expect_equal(attr(o1,"params")$limit,			0.5)
	expect_equal(attr(o1,"params")$channels, 		"left")
	expect_equal(attr(o1,"params")$limit.type,		"relative")

	###### energy.params
	
	### window.witdht = 20
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, limit.type="absolute",
			            energy.params=list(window.width=20, stepsize=5, normalize=0.9))
	expect_equal(attr(o1,"params")$energy.params$window.width,	20)
	expect_equal(attr(o1,"params")$energy.params$stepsize,		5)
	expect_equal(attr(o1,"params")$energy.params$normalize,		0.9)
	
	### stepsize = 3
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, limit.type="absolute",
			             energy.params=list(window.width=10, stepsize=3, normalize=0.9))

	expect_equal(attr(o1,"params")$energy.params$window.width,	10)
	expect_equal(attr(o1,"params")$energy.params$stepsize,		3)
	expect_equal(attr(o1,"params")$energy.params$normalize,		0.9)
	
	### normalize = 0.7
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, limit.type="absolute",
			            energy.params=list(window.width=10, stepsize=5, normalize=0.7))
	expect_equal(attr(o1,"params")$energy.params$window.width,	10)
	expect_equal(attr(o1,"params")$energy.params$stepsize,		5)
	expect_equal(attr(o1,"params")$energy.params$normalize,		0.7)
	
	### All parameters
	
	o1 <- analyse.file( "../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, limit.type="absolute",
			            energy.params=list(window.width=20, stepsize=3, normalize=0.7))
	expect_equal(attr(o1,"params")$energy.params$window.width,  20)
	expect_equal(attr(o1,"params")$energy.params$stepsize,		3)
	expect_equal(attr(o1,"params")$energy.params$normalize,		0.7)
})

##################################################################

test_that("Parameters are propagated during directory analysis", {
				
		### All parameters at default values
			
		os <- analyse.directory( "../testdata/testsets", channels="both", limit = 0.1, limit.type="absolute")
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.1)
			expect_equal(attr(o$onsets,"params")$channels, 		"both")
			expect_equal(attr(o$onsets,"params")$limit.type,	"absolute")
		}
		
		
		### channels = left
		
		os <- analyse.directory( "../testdata/testsets", channels="left", limit = 0.1, limit.type="absolute")
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.1)
			expect_equal(attr(o$onsets,"params")$channels, 		"left")
			expect_equal(attr(o$onsets,"params")$limit.type,	"absolute")
		}
		
		### channels = right
		
		os <- analyse.directory( "../testdata/testsets", channels="right", limit = 0.1, limit.type="absolute")
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.1)
			expect_equal(attr(o$onsets,"params")$channels, 		"right")
			expect_equal(attr(o$onsets,"params")$limit.type,	"absolute")
		}
		
		### limit = 0.5
		
		os <- analyse.directory( "../testdata/testsets", channels="both", limit = 0.5, limit.type="absolute")
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.5)
			expect_equal(attr(o$onsets,"params")$channels, 		"both")
			expect_equal(attr(o$onsets,"params")$limit.type,	"absolute")
		}
		
		### limit.type = "relative"
		
		os <- analyse.directory( "../testdata/testsets", channels="both", limit = 0.1, limit.type="relative")
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.1)
			expect_equal(attr(o$onsets,"params")$channels, 		"both")
			expect_equal(attr(o$onsets,"params")$limit.type,	"relative")
		}
		
		### All parameters changed
		
		os <- analyse.directory( "../testdata/testsets", channels="left", limit = 0.5, limit.type="relative")
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$limit,			0.5)
			expect_equal(attr(o$onsets,"params")$channels, 		"left")
			expect_equal(attr(o$onsets,"params")$limit.type,	"relative")
		}
		
		##### energy.params
		
		### window.witdth = 20
		
		os <- analyse.directory( "../testdata/testsets", channels="both", limit = 0.1, limit.type="absolute",
				                 energy.params=list(window.width=20, stepsize=5, normalize=0.9))
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$energy.params$window.width,	20)
			expect_equal(attr(o$onsets,"params")$energy.params$stepsize,		5)
			expect_equal(attr(o$onsets,"params")$energy.params$normalize,		0.9)
		}
		
		### stepsize = 3
		
		os <- analyse.directory( "../testdata/testsets", channels="both", limit = 0.1, limit.type="absolute",
				                 energy.params=list(window.width=10, stepsize=3, normalize=0.9))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$energy.params$window.width,	10)
			expect_equal(attr(o$onsets,"params")$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$energy.params$normalize,		0.9)
		}
		
				
		### normalize = 0.7
		
		os <- analyse.directory( "../testdata/testsets", channels="both", limit = 0.1, limit.type="absolute",
				                 energy.params=list(window.width=10, stepsize=3, normalize=0.7))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$energy.params$window.width,	10)
			expect_equal(attr(o$onsets,"params")$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$energy.params$normalize,		0.7)
		}
		
		
		### All parameters changed
		
		os <- analyse.directory( "../testdata/testsets", channels="both", limit = 0.1, limit.type="absolute",
				                 energy.params=list(window.width=20, stepsize=3, normalize=0.7))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$energy.params$window.width,	20)
			expect_equal(attr(o$onsets,"params")$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$energy.params$normalize,		0.7)
		}
})

