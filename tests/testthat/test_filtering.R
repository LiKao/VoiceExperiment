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

context("Filtering")

###############################################

test_that("False Positives are discarded based on duration", {
	
	# Testcase: 1 block of noise 500ms (above min.duration)
	#
	# Should detect 1 block
	w1 <- read.wav("../testdata/silence_noise_silence_500ms.wav")
	o1 <- onsets(w1)
	expect_equal(length(o1), 1)
	

	# Testcase: 1 block of noise 50ms (below min.duration)
	#
	# Should detect no block (i.e. NA returned)
	w2 <- read.wav("../testdata/silence_noise_silence_50ms.wav")
	expect_warning(o2 <- onsets(w2))
	expect_equal(length(o2), 1) # NA has length=1
	expect_true(is.na(o2))
	
	# Testcase: 2 blocks of noise 500ms (both above min.duration)
	#
	# Should detect 2 blocks
	w3 <- read.wav("../testdata/silence_noise_noise_500ms.wav")
	o3 <- onsets(w3)
	expect_equal(length(o3), 2)
	
	# Testcase: 2 blocks of noise 50ms (both below min.duration)
	#
	# Should detect 0 blocks (i.e. NA returned)
	w4 <- read.wav("../testdata/silence_noise_noise_50ms.wav")
	expect_warning(o4 <- onsets(w4))
	expect_equal(length(o4), 1) # NA has length=1
	expect_true(is.na(o4))
	
	# Testcase: 1 block of noise 50ms  (below min.duration)
	#           1 block of noise 500ms (above min.duration)
	#
	# Should detect 1 blocks
	w5 <- read.wav("../testdata/silence_noise_noise_50ms_500ms.wav")
	o5 <- onsets(w5)
	expect_equal(length(o5), 1)
	
	# Testcase: 1 block of noise 50ms  (below min.duration)
	#           1 block of noise 500ms (above min.duration)
	#
	# Should detect 1 blocks
	w6 <- read.wav("../testdata/silence_noise_noise_500ms_50ms.wav")
	o6 <- onsets(w6)
	expect_equal(length(o6), 1)
	
})