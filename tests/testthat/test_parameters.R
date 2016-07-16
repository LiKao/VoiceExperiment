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
	
	expect_error( read.wav(testfile, filter=list(low=-100, high= 4000, Rp=0.01, 	Rs= 40, steepness= 1)), "Illegal value -100 for lower passband border in filter parameter" )
	expect_error( read.wav(testfile, filter=list(low= 300, high=  200, Rp=0.01, 	Rs= 40, steepness= 1)), "Higher passband border 200 below lower passband border 300" )
	# The testfile is sampled at 44.1kHz, so the Nyquist Frequency is at 22.05kHz
	expect_error( read.wav(testfile, filter=list(low= 300, high=25000, Rp=0.01, 	Rs= 40, steepness= 1)), "Higher passband border 25000 above Nyquist frequency 22050" )
	# With a passband border at 15kHz and a steepness of 1 octave, we get a stopband at 30kHz, 
	# i.e. above Nyquist
	expect_error( read.wav(testfile, filter=list(low= 300, high=15000, Rp=  0.01, 	Rs= 40, steepness= 1)), "Illegal upper stopband border: 30000" )
	# With a steepness of 2 octave we ge a lower limit (2 octaves is times 4, i.e. 2 octaves above 10kHz is 40kHz)
	expect_error( read.wav(testfile, filter=list(low= 300, high=10000, Rp=  0.01, 	Rs= 40, steepness= 2)), "Illegal upper stopband border: 40000" )
	
	expect_error( read.wav(testfile, filter=list(low= 300, high= 4000, Rp=-10,		Rs= 40, steepness= 1)), "Illegal passband ripple: -10" )
	expect_error( read.wav(testfile, filter=list(low= 300, high= 4000, Rp=  0, 		Rs= 40, steepness= 1)), "Illegal passband ripple: 0" )
	expect_error( read.wav(testfile, filter=list(low= 300, high= 4000, Rp=  0.01, 	Rs=-10, steepness= 1)), "Illegal attenuation: -10" )
	expect_error( read.wav(testfile, filter=list(low= 300, high= 4000, Rp=0.01, 	Rs=  0, steepness= 1)), "Illegal attenuation: 0" )
	expect_error( read.wav(testfile, filter=list(low= 300, high= 4000, Rp=0.01,     Rs= 40, steepness=-1)), "Illegal steepness: -1" )
	expect_error( read.wav(testfile, filter=list(low= 300, high= 4000, Rp=0.01,     Rs= 40, steepness= 0)), "Illegal steepness: 0" )
	
	
		  
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
	
	# Limits with hysteresis

	expect_error( onsets(w, limit=c(0.1,  -2), limit.type="absolute"), 	"Illegal lower limit value: -2" )
	expect_error( onsets(w, limit=c(0.1,  -1), limit.type="absolute"), 	"Illegal lower limit value: -1" )
	expect_error( onsets(w, limit=c(0.1,-0.1), limit.type="absolute"), 	"Illegal lower limit value: -0.1" )
	expect_error( onsets(w, limit=c(0.1,   0), limit.type="absolute"), 	"Illegal lower limit value: 0" )
	expect_error( onsets(w, limit=c(1,  0.01), limit.type="absolute"), 	"Illegal higher limit value: 1" )
	expect_error( onsets(w, limit=c(2,  0.01), limit.type="absolute"), 	"Illegal higher limit value: 2" )
	expect_error( onsets(w, limit=c(0.01,0.1), limit.type="absolute"), 	"Illegal limit value: High limit 0.01 is below low limit 0.1" )
	
	# Limits without hysteresis
	
	expect_error( onsets(w, limit=-2,   limit.type="absolute"), 	"Illegal limit value: -2" )
	expect_error( onsets(w, limit=-1,   limit.type="absolute"), 	"Illegal limit value: -1" )
	expect_error( onsets(w, limit=-0.1, limit.type="absolute"), 	"Illegal limit value: -0.1" )
	expect_error( onsets(w, limit= 0,   limit.type="absolute"), 	"Illegal limit value: 0" )
	expect_error( onsets(w, limit= 1,   limit.type="absolute"), 	"Illegal limit value: 1" )
	expect_error( onsets(w, limit= 2,   limit.type="absolute"), 	"Illegal limit value: 2" )
	
	### onsets.WaveData
	
	# Limits with hysteresis
	
	expect_error( onsets.WaveData(w, limit=c(0.1,  -2), limit.type="absolute"), 	"Illegal lower limit value: -2" )
	expect_error( onsets.WaveData(w, limit=c(0.1,  -1), limit.type="absolute"), 	"Illegal lower limit value: -1" )
	expect_error( onsets.WaveData(w, limit=c(0.1,-0.1), limit.type="absolute"), 	"Illegal lower limit value: -0.1" )
	expect_error( onsets.WaveData(w, limit=c(0.1,   0), limit.type="absolute"), 	"Illegal lower limit value: 0" )
	expect_error( onsets.WaveData(w, limit=c(1,  0.01), limit.type="absolute"), 	"Illegal higher limit value: 1" )
	expect_error( onsets.WaveData(w, limit=c(2,  0.01), limit.type="absolute"), 	"Illegal higher limit value: 2" )
	expect_error( onsets.WaveData(w, limit=c(0.01,0.1), limit.type="absolute"), 	"Illegal limit value: High limit 0.01 is below low limit 0.1" )
	
	# Limits without hysteresis
	
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
	
	# Limits with hysteresis
	
	expect_error( onsets.energyDensity(e, limit=c(0.1,  -2), limit.type="absolute"), 	"Illegal lower limit value: -2" )
	expect_error( onsets.energyDensity(e, limit=c(0.1,  -1), limit.type="absolute"), 	"Illegal lower limit value: -1" )
	expect_error( onsets.energyDensity(e, limit=c(0.1,-0.1), limit.type="absolute"), 	"Illegal lower limit value: -0.1" )
	expect_error( onsets.energyDensity(e, limit=c(0.1,   0), limit.type="absolute"), 	"Illegal lower limit value: 0" )
	expect_error( onsets.energyDensity(e, limit=c(1,  0.01), limit.type="absolute"), 	"Illegal higher limit value: 1" )
	expect_error( onsets.energyDensity(e, limit=c(2,  0.01), limit.type="absolute"), 	"Illegal higher limit value: 2" )
	expect_error( onsets.energyDensity(e, limit=c(0.01,0.1), limit.type="absolute"), 	"Illegal limit value: High limit 0.01 is below low limit 0.1" )
	
	# Limits without hysteresis
	
	expect_error( onsets.energyDensity(e, limit=-2,   limit.type="absolute"), 	"Illegal limit value: -2" )
	expect_error( onsets.energyDensity(e, limit=-1,   limit.type="absolute"), 	"Illegal limit value: -1" )
	expect_error( onsets.energyDensity(e, limit=-0.1, limit.type="absolute"), 	"Illegal limit value: -0.1" )
	expect_error( onsets.energyDensity(e, limit= 0,   limit.type="absolute"), 	"Illegal limit value: 0" )
	expect_error( onsets.energyDensity(e, limit= 1,   limit.type="absolute"), 	"Illegal limit value: 1" )
	expect_error( onsets.energyDensity(e, limit= 2,   limit.type="absolute"), 	"Illegal limit value: 2" )
	
	
	### analyse.file
	
	expect_error( analyse.file( illegalfile ), "File '[^']*' does not exist.")
	
	expect_error( analyse.file( testfile, read.params=list(channels="none") ), "'arg' should be one of \"both\", \"left\", \"right\"")
	
	expect_error( analyse.file(testfile, filter=list(low=-100, high= 4000, Rp=0.01, 	Rs= 40, steepness= 1)), "Illegal value -100 for lower passband border in filter parameter" )
	expect_error( analyse.file(testfile, filter=list(low= 300, high=  200, Rp=0.01, 	Rs= 40, steepness= 1)), "Higher passband border 200 below lower passband border 300" )
	# The testfile is sampled at 44.1kHz, so the Nyquist Frequency is at 22.05kHz
	expect_error( analyse.file(testfile, filter=list(low= 300, high=25000, Rp=0.01, 	Rs= 40, steepness= 1)), "Higher passband border 25000 above Nyquist frequency 22050" )
	# With a passband border at 15kHz and a steepness of 1 octave, we get a stopband at 30kHz, 
	# i.e. above Nyquist
	expect_error( analyse.file(testfile, filter=list(low= 300, high=15000, Rp=  0.01, 	Rs= 40, steepness= 1)), "Illegal upper stopband border: 30000" )
	# With a steepness of 2 octave we ge a lower limit (2 octaves is times 4, i.e. 2 octaves above 10kHz is 40kHz)
	expect_error( analyse.file(testfile, filter=list(low= 300, high=10000, Rp=  0.01, 	Rs= 40, steepness= 2)), "Illegal upper stopband border: 40000" )
	
	expect_error( analyse.file(testfile, filter=list(low= 300, high= 4000, Rp=-10,		Rs= 40, steepness= 1)), "Illegal passband ripple: -10" )
	expect_error( analyse.file(testfile, filter=list(low= 300, high= 4000, Rp=  0, 		Rs= 40, steepness= 1)), "Illegal passband ripple: 0" )
	expect_error( analyse.file(testfile, filter=list(low= 300, high= 4000, Rp=  0.01, 	Rs=-10, steepness= 1)), "Illegal attenuation: -10" )
	expect_error( analyse.file(testfile, filter=list(low= 300, high= 4000, Rp=0.01, 	Rs=  0, steepness= 1)), "Illegal attenuation: 0" )
	expect_error( analyse.file(testfile, filter=list(low= 300, high= 4000, Rp=0.01,     Rs= 40, steepness=-1)), "Illegal steepness: -1" )
	expect_error( analyse.file(testfile, filter=list(low= 300, high= 4000, Rp=0.01,     Rs= 40, steepness= 0)), "Illegal steepness: 0" )
	
	# Limits with hysteresis
	
	expect_error( analyse.file( testfile, onset.params=list(limit=c(0.1,  -2), limit.type="absolute")), 	"Illegal lower limit value: -2" )
	expect_error( analyse.file( testfile, onset.params=list(limit=c(0.1,  -1), limit.type="absolute")), 	"Illegal lower limit value: -1" )
	expect_error( analyse.file( testfile, onset.params=list(limit=c(0.1,-0.1), limit.type="absolute")), 	"Illegal lower limit value: -0.1" )
	expect_error( analyse.file( testfile, onset.params=list(limit=c(0.1,   0), limit.type="absolute")), 	"Illegal lower limit value: 0" )
	expect_error( analyse.file( testfile, onset.params=list(limit=c(1,  0.01), limit.type="absolute")), 	"Illegal higher limit value: 1" )
	expect_error( analyse.file( testfile, onset.params=list(limit=c(2,  0.01), limit.type="absolute")), 	"Illegal higher limit value: 2" )
	expect_error( analyse.file( testfile, onset.params=list(limit=c(0.01,0.1), limit.type="absolute")), 	"Illegal limit value: High limit 0.01 is below low limit 0.1" )
	
	# Limits without hysteresis
	
	expect_error( analyse.file( testfile, onset.params=list(limit = -2,	limit.type="absolute")), "Illegal limit value: -2" )
	expect_error( analyse.file( testfile, onset.params=list(limit = -1,	limit.type="absolute")), "Illegal limit value: -1" )
	expect_error( analyse.file( testfile, onset.params=list(limit = -0.1, 	limit.type="absolute")), "Illegal limit value: -0.1" )
	expect_error( analyse.file( testfile, onset.params=list(limit =  0,	limit.type="absolute")), "Illegal limit value: 0" )
	expect_error( analyse.file( testfile, onset.params=list(limit =  1,	limit.type="absolute")), "Illegal limit value: 1" )
	expect_error( analyse.file( testfile, onset.params=list(limit =  2,	limit.type="absolute")), "Illegal limit value: 2" )
		  
	expect_error(	analyse.file( testfile, energy.params=list(normalize= 0.9, window.width=-10, stepsize=  5)), "Illegal window width: -10")
	expect_error(	analyse.file( testfile, energy.params=list(normalize= 0.9, window.width=  0, stepsize=  5)), "Illegal window width: 0")
	expect_error(	analyse.file( testfile, energy.params=list(normalize= 0.9, window.width= 10, stepsize=-10)), "Illegal stepsize: -10")
	expect_error(	analyse.file( testfile, energy.params=list(normalize= 0.9, window.width= 10, stepsize=  0)), "Illegal stepsize: 0")
	expect_warning(	analyse.file( testfile, energy.params=list(normalize= 0.9, window.width= 10, stepsize= 15)), "Stepsize 15 is larger than window width 10")
	expect_error(	analyse.file( testfile, energy.params=list(normalize=-1.5, window.width= 10, stepsize=  5)), "Illegal normalization value: -1.5")
	expect_error(	analyse.file( testfile, energy.params=list(normalize=-0.5, window.width= 10, stepsize=  5)), "Illegal normalization value: -0.5")
	expect_error(	analyse.file( testfile, energy.params=list(normalize= 1.5, window.width= 10, stepsize=  5)), "Illegal normalization value: 1.5")
	
	
	### analyse.directory
	
	expect_error( analyse.directory( illegaldir ), "Directory '[^']*' does not exist.")
	
	expect_error( analyse.directory( testdir, read.params=list(channels="none")), "'arg' should be one of \"both\", \"left\", \"right\"")
	
	# Limits with hysteresis
	
	expect_error( analyse.directory( testdir, onset.params=list(limit=c(0.1,  -2), limit.type="absolute")), 	"Illegal lower limit value: -2" )
	expect_error( analyse.directory( testdir, onset.params=list(limit=c(0.1,  -1), limit.type="absolute")), 	"Illegal lower limit value: -1" )
	expect_error( analyse.directory( testdir, onset.params=list(limit=c(0.1,-0.1), limit.type="absolute")), 	"Illegal lower limit value: -0.1" )
	expect_error( analyse.directory( testdir, onset.params=list(limit=c(0.1,   0), limit.type="absolute")), 	"Illegal lower limit value: 0" )
	expect_error( analyse.directory( testdir, onset.params=list(limit=c(1,  0.01), limit.type="absolute")), 	"Illegal higher limit value: 1" )
	expect_error( analyse.directory( testdir, onset.params=list(limit=c(2,  0.01), limit.type="absolute")), 	"Illegal higher limit value: 2" )
	expect_error( analyse.directory( testdir, onset.params=list(limit=c(0.01,0.1), limit.type="absolute")), 	"Illegal limit value: High limit 0.01 is below low limit 0.1" )
	
	# Limits without hysteresis
	
	expect_error( analyse.directory( testdir, onset.params=list(limit = -2,   limit.type="absolute")), "Illegal limit value: -2" )
	expect_error( analyse.directory( testdir, onset.params=list(limit = -1,   limit.type="absolute")), "Illegal limit value: -1" )
	expect_error( analyse.directory( testdir, onset.params=list(limit = -0.1, limit.type="absolute")), "Illegal limit value: -0.1" )
	expect_error( analyse.directory( testdir, onset.params=list(limit =  0,   limit.type="absolute")), "Illegal limit value: 0" )
	expect_error( analyse.directory( testdir, onset.params=list(limit =  1,   limit.type="absolute")), "Illegal limit value: 1" )
	expect_error( analyse.directory( testdir, onset.params=list(limit =  2,   limit.type="absolute")), "Illegal limit value: 2" )
	
	expect_error( analyse.directory( testdir, filter=list(low=-100, high= 4000, Rp=0.01, 	Rs= 40, steepness= 1)), "Illegal value -100 for lower passband border in filter parameter" )
	expect_error( analyse.directory( testdir, filter=list(low= 300, high=  200, Rp=0.01, 	Rs= 40, steepness= 1)), "Higher passband border 200 below lower passband border 300" )
	# The testfile is sampled at 44.1kHz, so the Nyquist Frequency is at 22.05kHz
	expect_error( analyse.directory( testdir, filter=list(low= 300, high=25000, Rp=0.01, 	Rs= 40, steepness= 1)), "Higher passband border 25000 above Nyquist frequency 22050" )
	# With a passband border at 15kHz and a steepness of 1 octave, we get a stopband at 30kHz, 
	# i.e. above Nyquist
	expect_error( analyse.directory( testdir, filter=list(low= 300, high=15000, Rp=  0.01, 	Rs= 40, steepness= 1)), "Illegal upper stopband border: 30000" )
	# With a steepness of 2 octave we ge a lower limit (2 octaves is times 4, i.e. 2 octaves above 10kHz is 40kHz)
	expect_error( analyse.directory( testdir, filter=list(low= 300, high=10000, Rp=  0.01, 	Rs= 40, steepness= 2)), "Illegal upper stopband border: 40000" )
	
	expect_error( analyse.directory( testdir, filter=list(low= 300, high= 4000, Rp=-10,		Rs= 40, steepness= 1)), "Illegal passband ripple: -10" )
	expect_error( analyse.directory( testdir, filter=list(low= 300, high= 4000, Rp=  0, 		Rs= 40, steepness= 1)), "Illegal passband ripple: 0" )
	expect_error( analyse.directory( testdir, filter=list(low= 300, high= 4000, Rp=  0.01, 	Rs=-10, steepness= 1)), "Illegal attenuation: -10" )
	expect_error( analyse.directory( testdir, filter=list(low= 300, high= 4000, Rp=0.01, 	Rs=  0, steepness= 1)), "Illegal attenuation: 0" )
	expect_error( analyse.directory( testdir, filter=list(low= 300, high= 4000, Rp=0.01,     Rs= 40, steepness=-1)), "Illegal steepness: -1" )
	expect_error( analyse.directory( testdir, filter=list(low= 300, high= 4000, Rp=0.01,     Rs= 40, steepness= 0)), "Illegal steepness: 0" )
	
	expect_error(	analyse.directory( testdir, energy.params=list(normalize= 0.9, window.width=-10, stepsize=  5)), "Illegal window width: -10")
	expect_error(	analyse.directory( testdir, energy.params=list(normalize= 0.9, window.width=  0, stepsize=  5)), "Illegal window width: 0")
	expect_error(	analyse.directory( testdir, energy.params=list(normalize= 0.9, window.width= 10, stepsize=-10)), "Illegal stepsize: -10")
	expect_error(	analyse.directory( testdir, energy.params=list(normalize= 0.9, window.width= 10, stepsize=  0)), "Illegal stepsize: 0")
	expect_warning(	analyse.directory( testdir, energy.params=list(normalize= 0.9, window.width= 10, stepsize= 15)), "Stepsize 15 is larger than window width 10")
	expect_error(	analyse.directory( testdir, energy.params=list(normalize=-1.5, window.width= 10, stepsize=  5)), "Illegal normalization value: -1.5")
    expect_error(	analyse.directory( testdir, energy.params=list(normalize=-0.5, window.width= 10, stepsize=  5)), "Illegal normalization value: -0.5")
    expect_error(	analyse.directory( testdir, energy.params=list(normalize= 1.5, window.width= 10, stepsize=  5)), "Illegal normalization value: 1.5")
	
	#### MFCC calculation
	
	expect_error( freq.to.mel(-1), 		"Negative frequency in freq.to.mel conversion")
	expect_error( mel.to.freq(-1), 		"Negative mel in mel.to.freq conversion")
	expect_error( freq.to.mel(-2), 		"Negative frequency in freq.to.mel conversion")
	expect_error( mel.to.freq(-2), 		"Negative mel in mel.to.freq conversion")
	expect_error( freq.to.mel(-100), 	"Negative frequency in freq.to.mel conversion")
	expect_error( mel.to.freq(-100), 	"Negative mel in mel.to.freq conversion")
	
	expect_error( preemphasis(w,  1), 	"Preemphasis too large")
	expect_error( preemphasis(w,  2), 	"Preemphasis too large")
	expect_error( preemphasis(w, -0.5), "Negative preemphasis value supplied to preemphasis filter")
	expect_error( preemphasis(w, -1), 	"Negative preemphasis value supplied to preemphasis filter")
	
	expect_error( create.filter(  10, 20,  30,  -1), "invalid number of bins in creation of MFCC filter")
	expect_error( create.filter(  10, 20,  30,   0), "invalid number of bins in creation of MFCC filter")
	expect_error( create.filter( -10, 20,  30, 100), "negative lower end of MFCC filter")
	expect_error( create.filter(  10, 20, 150, 100), "upper end of MFCC filter larger than total number of bins")
	expect_error( create.filter(  15, 10,  30, 100), "lower end of MFCC filter is larger than top")
	expect_error( create.filter(  10, 25,  15, 100), "top of MFCC filter is larger than upper end")
	
	expect_error( create.filterbank(8000, 300, 26, 44100, 1024), "Lower end of filterbank larger than upper end")
	expect_error( create.filterbank(300, 8000, -1, 44100, 1024), "Invalid number of MFCC filterbanks")
	expect_error( create.filterbank(300, 8000,  0, 44100, 1024), "Invalid number of MFCC filterbanks")
	expect_error( create.filterbank(300, 8000, 26,    -1, 1024), "Invalid sampling frequency")
	expect_error( create.filterbank(300, 8000, 26,     0, 1024), "Invalid sampling frequency")
	expect_error( create.filterbank(300, 8000, 26, 44100,   -1), "Invalid number of FFT bins")
	expect_error( create.filterbank(300, 8000, 26, 44100,    0), "Invalid number of FFT bins")
	
	expect_error( MFCCs(w, filterbanks=26, retain.coeffs=-1:13), 	"Invalid coefficents to retain")
	expect_error( MFCCs(w, filterbanks=26, retain.coeffs= 0:27), 	"Invalid coefficents to retain")
	expect_error( MFCCs(w, delta=-1), "Invalid negative delta value")
	
	### Windowing
	invalid_window1 <- function(l) {
		rep(1,l-2)
	}
	expect_error( slice(w, window.width=10, stepsize=5, window.function=invalid_window1), "Invalid windowing function")
	
	invalid_window2 <- function(l) {
		rep(-1,l)
	}
	expect_error( slice(w, window.width=10, stepsize=5, window.function=invalid_window2), "Invalid windowing function")
})

#############################################################

test_that("Parameters are propagated during file analysis", {
			
	testfile	<- "../testdata/silence_50ms_mono.wav"
	
	###### read.wav
	
	### channels = both (default)
				
	o1 <- analyse.file( testfile, read.params=list(channels="both") )
	expect_equal(attr(o1,"params")$read.params$channels, "both")
	
	### channels = left
	
	o1 <- analyse.file( testfile, read.params=list(channels="left"))
	expect_equal(attr(o1,"params")$read.params$channels, "left")
	
	### channels = right
	
	o1 <- analyse.file( testfile, read.params=list(channels="right"))
	expect_equal(attr(o1,"params")$read.params$channels, "right")
	
	### Filter parameters
	
	### Defaults
	
	o1 <- analyse.file( testfile, filter=list(low=300, high=4000, Rp=0.001, Rs=40, steepness=1))
	expect_equal(attr(o1,"params")$read.params$filter$low,			300)
	expect_equal(attr(o1,"params")$read.params$filter$high,			4000)
	expect_equal(attr(o1,"params")$read.params$filter$Rp,			0.001)
	expect_equal(attr(o1,"params")$read.params$filter$Rs,			40)
	expect_equal(attr(o1,"params")$read.params$filter$steepness,	1)
	
	### low=200
	
	o1 <- analyse.file( testfile, filter=list(low=200, high=4000, Rp=0.001, Rs=40, steepness=1))
	expect_equal(attr(o1,"params")$read.params$filter$low,			200)
	expect_equal(attr(o1,"params")$read.params$filter$high,			4000)
	expect_equal(attr(o1,"params")$read.params$filter$Rp,			0.001)
	expect_equal(attr(o1,"params")$read.params$filter$Rs,			40)
	expect_equal(attr(o1,"params")$read.params$filter$steepness,	1)
	
	### high=5000
	
	o1 <- analyse.file( testfile, filter=list(low=300, high=5000, Rp=0.001, Rs=40, steepness=1))
	expect_equal(attr(o1,"params")$read.params$filter$low,			300)
	expect_equal(attr(o1,"params")$read.params$filter$high,			5000)
	expect_equal(attr(o1,"params")$read.params$filter$Rp,			0.001)
	expect_equal(attr(o1,"params")$read.params$filter$Rs,			40)
	expect_equal(attr(o1,"params")$read.params$filter$steepness,	1)
	
	### Rp=1
	
	o1 <- analyse.file( testfile, filter=list(low=300, high=4000, Rp=1, Rs=40, steepness=1))
	expect_equal(attr(o1,"params")$read.params$filter$low,			300)
	expect_equal(attr(o1,"params")$read.params$filter$high,			4000)
	expect_equal(attr(o1,"params")$read.params$filter$Rp,			1)
	expect_equal(attr(o1,"params")$read.params$filter$Rs,			40)
	expect_equal(attr(o1,"params")$read.params$filter$steepness,	1)
	
	### Rs=20
	
	o1 <- analyse.file( testfile, filter=list(low=300, high=4000, Rp=0.001, Rs=20, steepness=1))
	expect_equal(attr(o1,"params")$read.params$filter$low,			300)
	expect_equal(attr(o1,"params")$read.params$filter$high,			4000)
	expect_equal(attr(o1,"params")$read.params$filter$Rp,			0.001)
	expect_equal(attr(o1,"params")$read.params$filter$Rs,			20)
	expect_equal(attr(o1,"params")$read.params$filter$steepness,	1)
	
	### steepness=2
	
	o1 <- analyse.file( testfile, filter=list(low=300, high=4000, Rp=0.001, Rs=40, steepness=2))
	expect_equal(attr(o1,"params")$read.params$filter$low,			300)
	expect_equal(attr(o1,"params")$read.params$filter$high,			4000)
	expect_equal(attr(o1,"params")$read.params$filter$Rp,			0.001)
	expect_equal(attr(o1,"params")$read.params$filter$Rs,			40)
	expect_equal(attr(o1,"params")$read.params$filter$steepness,	2)
	
	### All parameters
	
	o1 <- analyse.file( testfile, filter=list(low=200, high=5000, Rp=1, Rs=20, steepness=2))
	expect_equal(attr(o1,"params")$read.params$filter$low,			200)
	expect_equal(attr(o1,"params")$read.params$filter$high,			5000)
	expect_equal(attr(o1,"params")$read.params$filter$Rp,			1)
	expect_equal(attr(o1,"params")$read.params$filter$Rs,			20)
	expect_equal(attr(o1,"params")$read.params$filter$steepness,	2)
	
	###### onsets

	### limit.type = "absolute"
	
	o1 <- analyse.file( testfile, onset.params=list(limit.type="absolute"))
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"absolute")
	
	### limit.type = "relative"
	
	o1 <- analyse.file( testfile, onset.params=list(limit.type="relative"))
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"relative")
	
	### lower limit parameter at 0.05

	o1 <- analyse.file( testfile, onset.params=list(limit = c(0.1,0.05), limit.type="absolute"))
	expect_equal(attr(o1,"params")$onset.params$limit,		c(0.1,0.05))
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"absolute")
	
	o1 <- analyse.file( testfile, onset.params=list(limit = c(0.1,0.05), limit.type="relative"))
	expect_equal(attr(o1,"params")$onset.params$limit,		c(0.1,0.05))
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"relative")
	
	### upper limit parameter at 0.5
	
	o1 <- analyse.file( testfile, onset.params=list(limit = c(0.5,0.01), limit.type="absolute"))
	expect_equal(attr(o1,"params")$onset.params$limit,		c(0.5,0.01))
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"absolute")
	
	o1 <- analyse.file( testfile, onset.params=list(limit = c(0.5,0.01), limit.type="relative"))
	expect_equal(attr(o1,"params")$onset.params$limit,		c(0.5,0.01))
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"relative")
	
	### limit = 0.5 (no hysteresis)
	
	o1 <- analyse.file( testfile, onset.params=list(limit = 0.5, limit.type="absolute"))
	expect_equal(attr(o1,"params")$onset.params$limit,		0.5)
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"absolute")
	
	o1 <- analyse.file( testfile, onset.params=list(limit = 0.5, limit.type="relative"))
	expect_equal(attr(o1,"params")$onset.params$limit,		0.5)
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"relative")
	
	### All parameters
	
	o1 <- analyse.file( testfile, onset.params=list(limit = 0.5, limit.type="relative"))
	expect_equal(attr(o1,"params")$onset.params$limit,		0.5)
	expect_equal(attr(o1,"params")$onset.params$limit.type,	"relative")

	###### energy.params
	
	### window.witdht = 20
	
	o1 <- analyse.file( testfile, energy.params=list(window.width=20, stepsize=5, normalize=0.9))
	expect_equal(attr(o1,"params")$onset.params$energy.params$window.width,	20)
	expect_equal(attr(o1,"params")$onset.params$energy.params$stepsize,		5)
	expect_equal(attr(o1,"params")$onset.params$energy.params$normalize,		0.9)
	
	### stepsize = 3
	
	o1 <- analyse.file( testfile, energy.params=list(window.width=10, stepsize=3, normalize=0.9))

	expect_equal(attr(o1,"params")$onset.params$energy.params$window.width,	10)
	expect_equal(attr(o1,"params")$onset.params$energy.params$stepsize,		3)
	expect_equal(attr(o1,"params")$onset.params$energy.params$normalize,		0.9)
	
	### normalize = 0.7
	
	o1 <- analyse.file( testfile, energy.params=list(window.width=10, stepsize=5, normalize=0.7))
	expect_equal(attr(o1,"params")$onset.params$energy.params$window.width,	10)
	expect_equal(attr(o1,"params")$onset.params$energy.params$stepsize,		5)
	expect_equal(attr(o1,"params")$onset.params$energy.params$normalize,		0.7)
	
	### All parameters
	
	o1 <- analyse.file( testfile, energy.params=list(window.width=20, stepsize=3, normalize=0.7))
	expect_equal(attr(o1,"params")$onset.params$energy.params$window.width,  20)
	expect_equal(attr(o1,"params")$onset.params$energy.params$stepsize,		3)
	expect_equal(attr(o1,"params")$onset.params$energy.params$normalize,		0.7)
})

##################################################################

test_that("Parameters are propagated during directory analysis", {
			
		testdir		<- "../testdata/testsets"
		
		###### read.wav
		
		### channels = both
			
		os <- analyse.directory( testdir, read.params=list(channels="both"))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$channels, "both")
		}
		
		
		### channels = left
		
		os <- analyse.directory( testdir, read.params=list(channels="left"))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$channels, "left")
		}
		
		### channels = right
		
		os <- analyse.directory( testdir, read.params=list(channels="right"))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$channels, "right")
		}
		
		
		### Filter parameters
		
		### Defaults
		
		os <- analyse.directory( testdir, filter=list(low=300, high=4000, Rp=0.001, Rs=40, steepness=1))
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$filter$low,		300)
			expect_equal(attr(o$onsets,"params")$read.params$filter$high,		4000)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rp,			0.001)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rs,			40)
			expect_equal(attr(o$onsets,"params")$read.params$filter$steepness,	1)
		}
		
		### low=200
		
		os <- analyse.directory( testdir, filter=list(low=200, high=4000, Rp=0.001, Rs=40, steepness=1))
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$filter$low,		200)
			expect_equal(attr(o$onsets,"params")$read.params$filter$high,		4000)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rp,			0.001)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rs,			40)
			expect_equal(attr(o$onsets,"params")$read.params$filter$steepness,	1)
		}
		
		### high=5000
		
		os <- analyse.directory( testdir, filter=list(low=300, high=5000, Rp=0.001, Rs=40, steepness=1))
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$filter$low,		300)
			expect_equal(attr(o$onsets,"params")$read.params$filter$high,		5000)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rp,			0.001)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rs,			40)
			expect_equal(attr(o$onsets,"params")$read.params$filter$steepness,	1)
		}
		
		### Rp=1
		
		os <- analyse.directory( testdir, filter=list(low=300, high=4000, Rp=1, Rs=40, steepness=1))
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$filter$low,		300)
			expect_equal(attr(o$onsets,"params")$read.params$filter$high,		4000)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rp,			1)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rs,			40)
			expect_equal(attr(o$onsets,"params")$read.params$filter$steepness,	1)
		}
		
		### Rs=20
		
		os <- analyse.directory( testdir, filter=list(low=300, high=4000, Rp=0.001, Rs=20, steepness=1))
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$filter$low,		300)
			expect_equal(attr(o$onsets,"params")$read.params$filter$high,		4000)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rp,			0.001)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rs,			20)
			expect_equal(attr(o$onsets,"params")$read.params$filter$steepness,	1)
		}
		
		### steepnes=2
		
		os <- analyse.directory( testdir, filter=list(low=300, high=4000, Rp=0.001, Rs=40, steepness=2))
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$filter$low,		300)
			expect_equal(attr(o$onsets,"params")$read.params$filter$high,		4000)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rp,			0.001)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rs,			40)
			expect_equal(attr(o$onsets,"params")$read.params$filter$steepness,	2)
		}
		
		### All parameters
		
		os <- analyse.directory( testdir, filter=list(low=200, high=5000, Rp=1, Rs=20, steepness=2))
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$read.params$filter$low,		200)
			expect_equal(attr(o$onsets,"params")$read.params$filter$high,		5000)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rp,			1)
			expect_equal(attr(o$onsets,"params")$read.params$filter$Rs,			20)
			expect_equal(attr(o$onsets,"params")$read.params$filter$steepness,	2)
		}
		
		###### onsets
		
		### limit.type = "absolute"
		
		os <- analyse.directory( testdir, onset.params=list(limit.type="absolute"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"absolute")
		}
		
		### limit.type = "relative"
		
		os <- analyse.directory( testdir, onset.params=list(limit.type="relative"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"relative")
		}
		
		### lower limit parameter at 0.05
		
		os <- analyse.directory( testdir, onset.params=list(limit = c(0.1,0.05), limit.type="absolute"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit,		c(0.1,0.05))
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"absolute")
		}
		
		os <- analyse.directory( testdir, onset.params=list(limit = c(0.1,0.05), limit.type="relative"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit,		c(0.1,0.05))
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"relative")
		}
		
		### upper limit parameter at 0.5
		
		os <- analyse.directory( testdir, onset.params=list(limit = c(0.5,0.01), limit.type="absolute"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit,		c(0.5,0.01))
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"absolute")
		}
		
		os <- analyse.directory( testdir, onset.params=list(limit = c(0.5,0.01), limit.type="relative"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit,		c(0.5,0.01))
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"relative")
		}
		
		### limit = 0.5 (no hysteresis)
		
		os <- analyse.directory( testdir, onset.params=list(limit = 0.5, limit.type="absolute"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit,		0.5)
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"absolute")
		}
		
		os <- analyse.directory( testdir, onset.params=list(limit = 0.5, limit.type="relative"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit,		0.5)
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"relative")
		}
		
		### All parameters changed
		
		os <- analyse.directory( testdir, onset.params=list(limit = 0.5, limit.type="relative"))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$limit,		0.5)
			expect_equal(attr(o$onsets,"params")$onset.params$limit.type,	"relative")
		}
		
		##### energy.params
		
		### window.witdth = 20
		
		os <- analyse.directory( testdir, energy.params=list(window.width=20, stepsize=5, normalize=0.9))
		
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$window.width,	20)
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$stepsize,		5)
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$normalize,		0.9)
		}
		
		### stepsize = 3
		
		os <- analyse.directory( testdir, energy.params=list(window.width=10, stepsize=3, normalize=0.9))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$window.width,	10)
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$normalize,		0.9)
		}
		
				
		### normalize = 0.7
		
		os <- analyse.directory( testdir, energy.params=list(window.width=10, stepsize=3, normalize=0.7))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$window.width,	10)
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$normalize,		0.7)
		}
		
		
		### All parameters changed
		
		os <- analyse.directory( testdir, energy.params=list(window.width=20, stepsize=3, normalize=0.7))
						
		for(o in os) {
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$window.width,	20)
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$stepsize,		3)
			expect_equal(attr(o$onsets,"params")$onset.params$energy.params$normalize,		0.7)
		}
})

