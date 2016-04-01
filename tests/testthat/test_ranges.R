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

context("Data_Formats")

###############################################

test_that("Data has correct numerical ranges", {
	### Wavedata
	
	w.mono   <- read.wav("../testdata/silence_50ms_mono.wav")
	w.stereo.both <- read.wav("../testdata/silence_50ms_stereo.wav",channels="both")
	w.stereo.left <- read.wav("../testdata/silence_50ms_stereo.wav",channels="left")
	w.stereo.right <- read.wav("../testdata/silence_50ms_stereo.wav",channels="right")
	
	expect_true(all(w.mono$samples			<= 1))
	expect_true(all(w.stereo.both$samples	<= 1))
	expect_true(all(w.stereo.left$samples	<= 1))
	expect_true(all(w.stereo.right$samples	<= 1))
	
})

