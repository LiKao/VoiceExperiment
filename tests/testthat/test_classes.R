# test class membership and methods
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

context("Class Types")

###############################################

test_that("Correct classes are returned", {
	
	### Prepare Data
	
	w <- read.wav("../testdata/silence_50ms_mono.wav")
	e <- energyDensity(w)
	o <- onsets(w)
	d <- analyse.directory("../testdata/testsets")
			
	### read.wav	

	expect_is(w, "WaveData")
	expect_is(w, "ts")
	
	### slice; slice.WaveData
	
	expect_is(slice(w, window.width=10, stepsize=5), 			"tsSlice")
	expect_is(slice.WaveData(w, window.width=10, stepsize=5),	"tsSlice")
	
	### energyDensity; energyDensity.WaveData
	
	expect_is(energyDensity.WaveData(w), "energyDensity")
	expect_is(energyDensity.WaveData(w), "ts")
	expect_is(e, "energyDensity")
	expect_is(e, "ts")
	
	### onsets; onsets.WaveData; onsets.energyDensity
	
	expect_is(onsets(w), "onsetData")
	expect_is(onsets(e), "onsetData")
	expect_is(onsets.WaveData(w), "onsetData")
	expect_is(onsets.energyDensity(e), "onsetData")
	
	### Subfields of onsetData
	
	
	# Note: May stop working, if logic does not 
	# return onsets for quiet files anymore, use
	# actual test file instead in that case
	expect_is(o[[1]], "onset")
	
	### analyse.file
	
	expect_is(analyse.file("../testdata/silence_50ms_mono.wav"), "onsetData")
	
	### analyse.directory
	
	expect_is(analyse.directory("../testdata/testsets"), "voiceExperimentData")
	
	### conversion functions
	
	expect_is( as.matrix(o), "numeric")
	
	expect_is( as.data.frame(d), "data.frame")
	
	### spectrum, spectrum.WaveData
	
	expect_is( spectrum(w, window.width=10, stepsize=5), "spectrum")
	expect_is( spectrum.WaveData(w, window.width=10, stepsize=5), "spectrum")
	
	### MFCCs, MFCCs.WaveData
	tryCatch(MFCCs(w), error=function(e) print(e) )
	
	expect_is( MFCCs(w), "MFCCs")
	expect_is( MFCCs(w), "matrix")
	
	expect_is( MFCCs.WaveData(w), "MFCCs")
	expect_is( MFCCs.WaveData(w), "matrix")
	
})