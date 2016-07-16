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

context("Data Ranges")

###############################################

test_that("Data has correct numerical ranges", {
	### Wavedata
	
	w.mono   <- read.wav("../testdata/silence_50ms_mono.wav")
	w.stereo.both <- read.wav("../testdata/silence_50ms_stereo.wav",channels="both")
	w.stereo.left <- read.wav("../testdata/silence_50ms_stereo.wav",channels="left")
	w.stereo.right <- read.wav("../testdata/silence_50ms_stereo.wav",channels="right")
	
	expect_true(all(w.mono			<= 1))
	expect_true(all(w.stereo.both	<= 1))
	expect_true(all(w.stereo.left	<= 1))
	expect_true(all(w.stereo.right	<= 1))
	
})

test_that("Energy is normalized correctly", {
			
	### Read test files
	
	w1 <- read.wav("../testdata/noiseandsilence_volume_ascending.wav")
	w2 <- read.wav("../testdata/noiseandsilence_volume_descending.wav")
	w3 <- read.wav("../testdata/noise_volume_ascending.wav")
	w4 <- read.wav("../testdata/noise_volume_descending.wav")
	w5 <- read.wav("../testdata/sinesandsilence_volume_ascending.wav")
	w6 <- read.wav("../testdata/sinesandsilence_volume_descending.wav")
	w7 <- read.wav("../testdata/sines_volume_ascending.wav")
	w8 <- read.wav("../testdata/sines_volume_descending.wav")
		
	###### energyDensity
	
	### Normalize to 0.9
	
	expect_equal( max(energyDensity(w1, normalize=0.9)), 0.9, tolerance=0.01)
	expect_equal( max(energyDensity(w2, normalize=0.9)), 0.9, tolerance=0.01)
	expect_equal( max(energyDensity(w3, normalize=0.9)), 0.9, tolerance=0.01)
	expect_equal( max(energyDensity(w4, normalize=0.9)), 0.9, tolerance=0.01)
	expect_equal( max(energyDensity(w5, normalize=0.9)), 0.9, tolerance=0.01)
	expect_equal( max(energyDensity(w6, normalize=0.9)), 0.9, tolerance=0.01)
	expect_equal( max(energyDensity(w7, normalize=0.9)), 0.9, tolerance=0.01)
	expect_equal( max(energyDensity(w8, normalize=0.9)), 0.9, tolerance=0.01)
	
	### Normalize to 0.6
	
	expect_equal( max(energyDensity(w1, normalize=0.6)), 0.6, tolerance=0.01)
	expect_equal( max(energyDensity(w2, normalize=0.6)), 0.6, tolerance=0.01)
	expect_equal( max(energyDensity(w3, normalize=0.6)), 0.6, tolerance=0.01)
	expect_equal( max(energyDensity(w4, normalize=0.6)), 0.6, tolerance=0.01)
	expect_equal( max(energyDensity(w5, normalize=0.6)), 0.6, tolerance=0.01)
	expect_equal( max(energyDensity(w6, normalize=0.6)), 0.6, tolerance=0.01)
	expect_equal( max(energyDensity(w7, normalize=0.6)), 0.6, tolerance=0.01)
	expect_equal( max(energyDensity(w8, normalize=0.6)), 0.6, tolerance=0.01)
	
	### Normalize to 0.3
	
	expect_equal( max(energyDensity(w1, normalize=0.3)), 0.3, tolerance=0.01)
	expect_equal( max(energyDensity(w2, normalize=0.3)), 0.3, tolerance=0.01)
	expect_equal( max(energyDensity(w3, normalize=0.3)), 0.3, tolerance=0.01)
	expect_equal( max(energyDensity(w4, normalize=0.3)), 0.3, tolerance=0.01)
	expect_equal( max(energyDensity(w5, normalize=0.3)), 0.3, tolerance=0.01)
	expect_equal( max(energyDensity(w6, normalize=0.3)), 0.3, tolerance=0.01)
	expect_equal( max(energyDensity(w7, normalize=0.3)), 0.3, tolerance=0.01)
	expect_equal( max(energyDensity(w8, normalize=0.3)), 0.3, tolerance=0.01)
	
})

test_that("Extracted slices have the correct durations", {
	w <- read.wav("../testdata/silence_50ms_mono.wav")
	w2 <- window(w, start=0.015, end=0.045)
	
	
	# Note about end tests:
	# The last slice may end at the last sample of the wavedata
	# For a slice of duration d the last sample is the one just
	# within in this duration, i.e. in the range [start,start+d)
	# Hence the end of the slices may be at end of 
	# file + one sample duration. Because we may also get round
	# of error we check for two samples duration, i.e. 2/frequency.
	
	### Defaults
	s <- slice(w, window.width=10, stepsize=5)
	expect_equal( 	start(s), time(w)[1])
	expect_true( 	end(s) - tail(time(w),n=1) < 2/frequency(w) )
	
	s2 <- slice(w2, window.width=10, stepsize=5)
	expect_equal( 	start(s2), time(w2)[1])
	expect_true( 	end(s2) - tail(time(w2),n=1) < 2/frequency(w2))

	### Some more odd parameters
	s <- slice(w, window.width=7, stepsize=3)
	expect_equal( 	start(s), time(w)[1])
	expect_true( 	end(s) - tail(time(w),n=1) < 2/frequency(w))
	
	s2 <- slice(w2, window.width=7, stepsize=3)
	expect_equal( 	start(s2), time(w2)[1])
	expect_true( 	end(s2) - tail(time(w2),n=1) < 2/frequency(w2))
})

test_that("Extracted spectrums have the correct durations", {
	w <- read.wav("../testdata/silence_50ms_mono.wav")
	w2 <- window(w, start=0.015, end=0.045)
	
	
	# Note about end tests:
	# The last slice may end at the last sample of the wavedata
	# For a slice of duration d the last sample is the one just
	# within in this duration, i.e. in the range [start,start+d)
	# Hence the end of the slices may be at end of 
	# file + one sample duration. Because we may also get round
	# of error we check for two samples duration, i.e. 2/frequency.
	
	### Defaults
	s <- spectrum(w, window.width=10, stepsize=5)
	expect_equal( 	start(s), time(w)[1])
	expect_true( 	end(s) - tail(time(w),n=1) < 2/frequency(w) )
	
	s2 <- spectrum(w2, window.width=10, stepsize=5)
	expect_equal( 	start(s2), time(w2)[1])
	expect_true( 	end(s2) - tail(time(w2),n=1) < 2/frequency(w2))
	
	### Some more odd parameters
	s <- spectrum(w, window.width=7, stepsize=3)
	expect_equal( 	start(s), time(w)[1])
	expect_true( 	end(s) - tail(time(w),n=1) < 2/frequency(w))
	
	s2 <- spectrum(w2, window.width=7, stepsize=3)
	expect_equal( 	start(s2), time(w2)[1])
	expect_true( 	end(s2) - tail(time(w2),n=1) < 2/frequency(w2))		
})

test_that("Durations after Pre-emphasis are correct", {
	w1 <- read.wav("../testdata/noiseandsilence_volume_ascending.wav")
	w2 <- read.wav("../testdata/noiseandsilence_volume_descending.wav")
	w3 <- read.wav("../testdata/noise_volume_ascending.wav")
	w4 <- read.wav("../testdata/noise_volume_descending.wav")
	w5 <- read.wav("../testdata/sinesandsilence_volume_ascending.wav")
	w6 <- read.wav("../testdata/sinesandsilence_volume_descending.wav")
	w7 <- read.wav("../testdata/sines_volume_ascending.wav")
	w8 <- read.wav("../testdata/sines_volume_descending.wav")
	
	expect_equal(duration(preemphasis(w1,0.95)), duration(w1))
	expect_equal(duration(preemphasis(w2,0.95)), duration(w2))
	expect_equal(duration(preemphasis(w3,0.95)), duration(w3))
	expect_equal(duration(preemphasis(w4,0.95)), duration(w4))
	expect_equal(duration(preemphasis(w5,0.95)), duration(w5))
	expect_equal(duration(preemphasis(w6,0.95)), duration(w6))
	expect_equal(duration(preemphasis(w7,0.95)), duration(w7))
	expect_equal(duration(preemphasis(w8,0.95)), duration(w8))
	
	expect_equal(duration(preemphasis(w1,0.50)), duration(w1))
	expect_equal(duration(preemphasis(w2,0.50)), duration(w2))
	expect_equal(duration(preemphasis(w3,0.50)), duration(w3))
	expect_equal(duration(preemphasis(w4,0.50)), duration(w4))
	expect_equal(duration(preemphasis(w5,0.50)), duration(w5))
	expect_equal(duration(preemphasis(w6,0.50)), duration(w6))
	expect_equal(duration(preemphasis(w7,0.50)), duration(w7))
	expect_equal(duration(preemphasis(w8,0.50)), duration(w8))
	
	expect_equal(duration(preemphasis(w1,0.10)), duration(w1))
	expect_equal(duration(preemphasis(w2,0.10)), duration(w2))
	expect_equal(duration(preemphasis(w3,0.10)), duration(w3))
	expect_equal(duration(preemphasis(w4,0.10)), duration(w4))
	expect_equal(duration(preemphasis(w5,0.10)), duration(w5))
	expect_equal(duration(preemphasis(w6,0.10)), duration(w6))
	expect_equal(duration(preemphasis(w7,0.10)), duration(w7))
	expect_equal(duration(preemphasis(w8,0.10)), duration(w8))
})

test_that("MFCC filters have correct length", {
	expect_equal(length(create.filter(10, 20, 30, 100)), 100)
	expect_equal(length(create.filter(10, 20, 30, 200)), 200)
	expect_equal(length(create.filter(10, 20, 30, 300)), 300)
	
	expect_equal(length(create.filter(40, 80, 100, 100)), 100)
	expect_equal(length(create.filter(40, 80, 100, 200)), 200)
	expect_equal(length(create.filter(40, 80, 100, 300)), 300)
})


test_that("MFCC filters have correct value range", {
	expect_equal(max(create.filter(10, 20, 30, 100)), 1)
	expect_equal(max(create.filter(10, 20, 30, 200)), 1)
	expect_equal(max(create.filter(10, 20, 30, 300)), 1)
	
	expect_equal(min(create.filter(40, 80, 100, 100)), 0)
	expect_equal(min(create.filter(40, 80, 100, 200)), 0)
	expect_equal(min(create.filter(40, 80, 100, 300)), 0)
})

test_that("MFCC filters have maxima and minima at correct positions", {
	expect_equal(which(create.filter(10, 20, 30, 100)==1), 20)
	expect_equal(which(create.filter(10, 20, 30, 200)==1), 20)
	expect_equal(which(create.filter(10, 20, 30, 300)==1), 20)
	
	expect_equal(which(create.filter(40, 80, 100, 100)==1), 80)
	expect_equal(which(create.filter(40, 80, 100, 200)==1), 80)
	expect_equal(which(create.filter(40, 80, 100, 300)==1), 80)
	
	expect_true(all(1:10 %in% which(create.filter(10, 20, 30, 100)==0)))
	expect_true(all(1:10 %in% which(create.filter(10, 20, 30, 200)==0)))
	expect_true(all(1:10 %in% which(create.filter(10, 20, 30, 300)==0)))
	
	expect_true(all(30:100 %in% which(create.filter(10, 20, 30, 100)==0)))
	expect_true(all(30:200 %in% which(create.filter(10, 20, 30, 200)==0)))
	expect_true(all(30:300 %in% which(create.filter(10, 20, 30, 300)==0)))
	
	expect_true(all(1:40 %in% which(create.filter(40, 80, 100, 100)==0)))
	expect_true(all(1:40 %in% which(create.filter(40, 80, 100, 200)==0)))
	expect_true(all(1:40 %in% which(create.filter(40, 80, 100, 300)==0)))
	
	expect_true(all(100:200 %in% which(create.filter(40, 80, 100, 200)==0)))
	expect_true(all(100:300 %in% which(create.filter(40, 80, 100, 300)==0)))
})

test_that("MFCC filterbanks have correct size", {
	expect_equal(ncol(create.filterbank(300, 8000, 10, 44100, 256 )), 10)
	expect_equal(ncol(create.filterbank(300, 8000, 20, 44100, 256 )), 20)
	expect_equal(ncol(create.filterbank(300, 8000, 30, 44100, 256 )), 30)
	expect_equal(ncol(create.filterbank(300, 8000, 40, 44100, 256 )), 40)
	
	expect_equal(nrow(create.filterbank(300, 8000, 10, 44100,  256 )),  256)
	expect_equal(nrow(create.filterbank(300, 8000, 20, 44100,  512 )),  512)
	expect_equal(nrow(create.filterbank(300, 8000, 30, 44100, 1024 )), 1024)
	expect_equal(nrow(create.filterbank(300, 8000, 40, 44100, 2048 )), 2048)
})

test_that("MFCC vectors have correct size", {
	w1 <- read.wav("../testdata/noiseandsilence_volume_ascending.wav")
	w2 <- read.wav("../testdata/noiseandsilence_volume_descending.wav")
	w3 <- read.wav("../testdata/noise_volume_ascending.wav")
	w4 <- read.wav("../testdata/noise_volume_descending.wav")
	w5 <- read.wav("../testdata/sinesandsilence_volume_ascending.wav")
	w6 <- read.wav("../testdata/sinesandsilence_volume_descending.wav")
	w7 <- read.wav("../testdata/sines_volume_ascending.wav")
	w8 <- read.wav("../testdata/sines_volume_descending.wav")
	
	### delta = 0
	
	expect_equal(nrow(MFCCs(w1, retain.coeffs=1:13, delta=0)), length(1:13)*1)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=1:13, delta=0)), length(1:13)*1)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=1:13, delta=0)), length(1:13)*1)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=1:13, delta=0)), length(1:13)*1)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=1:13, delta=0)), length(1:13)*1)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=1:13, delta=0)), length(1:13)*1)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=1:13, delta=0)), length(1:13)*1)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=1:13, delta=0)), length(1:13)*1)
	
	expect_equal(nrow(MFCCs(w1, retain.coeffs=5:13, delta=0)), length(5:13)*1)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=5:13, delta=0)), length(5:13)*1)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=5:13, delta=0)), length(5:13)*1)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=5:13, delta=0)), length(5:13)*1)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=5:13, delta=0)), length(5:13)*1)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=5:13, delta=0)), length(5:13)*1)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=5:13, delta=0)), length(5:13)*1)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=5:13, delta=0)), length(5:13)*1)

	expect_equal(nrow(MFCCs(w1, retain.coeffs=5:20, delta=0)), length(5:20)*1)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=5:20, delta=0)), length(5:20)*1)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=5:20, delta=0)), length(5:20)*1)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=5:20, delta=0)), length(5:20)*1)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=5:20, delta=0)), length(5:20)*1)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=5:20, delta=0)), length(5:20)*1)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=5:20, delta=0)), length(5:20)*1)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=5:20, delta=0)), length(5:20)*1)
	
	### delta = 1
	
	expect_equal(nrow(MFCCs(w1, retain.coeffs=1:13, delta=1)), length(1:13)*2)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=1:13, delta=1)), length(1:13)*2)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=1:13, delta=1)), length(1:13)*2)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=1:13, delta=1)), length(1:13)*2)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=1:13, delta=1)), length(1:13)*2)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=1:13, delta=1)), length(1:13)*2)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=1:13, delta=1)), length(1:13)*2)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=1:13, delta=1)), length(1:13)*2)
	
	expect_equal(nrow(MFCCs(w1, retain.coeffs=5:13, delta=1)), length(5:13)*2)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=5:13, delta=1)), length(5:13)*2)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=5:13, delta=1)), length(5:13)*2)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=5:13, delta=1)), length(5:13)*2)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=5:13, delta=1)), length(5:13)*2)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=5:13, delta=1)), length(5:13)*2)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=5:13, delta=1)), length(5:13)*2)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=5:13, delta=1)), length(5:13)*2)
	
	expect_equal(nrow(MFCCs(w1, retain.coeffs=5:20, delta=1)), length(5:20)*2)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=5:20, delta=1)), length(5:20)*2)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=5:20, delta=1)), length(5:20)*2)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=5:20, delta=1)), length(5:20)*2)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=5:20, delta=1)), length(5:20)*2)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=5:20, delta=1)), length(5:20)*2)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=5:20, delta=1)), length(5:20)*2)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=5:20, delta=1)), length(5:20)*2)
	
	### delta = 2
	
	expect_equal(nrow(MFCCs(w1, retain.coeffs=1:13, delta=2)), length(1:13)*3)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=1:13, delta=2)), length(1:13)*3)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=1:13, delta=2)), length(1:13)*3)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=1:13, delta=2)), length(1:13)*3)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=1:13, delta=2)), length(1:13)*3)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=1:13, delta=2)), length(1:13)*3)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=1:13, delta=2)), length(1:13)*3)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=1:13, delta=2)), length(1:13)*3)
	
	expect_equal(nrow(MFCCs(w1, retain.coeffs=5:13, delta=2)), length(5:13)*3)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=5:13, delta=2)), length(5:13)*3)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=5:13, delta=2)), length(5:13)*3)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=5:13, delta=2)), length(5:13)*3)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=5:13, delta=2)), length(5:13)*3)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=5:13, delta=2)), length(5:13)*3)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=5:13, delta=2)), length(5:13)*3)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=5:13, delta=2)), length(5:13)*3)
	
	expect_equal(nrow(MFCCs(w1, retain.coeffs=5:20, delta=2)), length(5:20)*3)
	expect_equal(nrow(MFCCs(w2, retain.coeffs=5:20, delta=2)), length(5:20)*3)
	expect_equal(nrow(MFCCs(w3, retain.coeffs=5:20, delta=2)), length(5:20)*3)
	expect_equal(nrow(MFCCs(w4, retain.coeffs=5:20, delta=2)), length(5:20)*3)
	expect_equal(nrow(MFCCs(w5, retain.coeffs=5:20, delta=2)), length(5:20)*3)
	expect_equal(nrow(MFCCs(w6, retain.coeffs=5:20, delta=2)), length(5:20)*3)
	expect_equal(nrow(MFCCs(w7, retain.coeffs=5:20, delta=2)), length(5:20)*3)
	expect_equal(nrow(MFCCs(w8, retain.coeffs=5:20, delta=2)), length(5:20)*3)
	
})