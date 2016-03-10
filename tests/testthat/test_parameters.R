

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

test_that("Parameters are propagated during file analysis", {
	
	o1 <- analyse.file("../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, window.width=10, stepsize=5)
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$window.width,	10)
	expect_equal(attr(o1,"params")$stepsize,		5)
	expect_equal(attr(o1,"params")$channels, 		"both")
	
	o1 <- analyse.file("../testdata/silence_50ms_mono.wav", channels="left", limit = 0.1, window.width=10, stepsize=5)
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$window.width,	10)
	expect_equal(attr(o1,"params")$stepsize,		5)
	expect_equal(attr(o1,"params")$channels, 		"left")
	
	o1 <- analyse.file("../testdata/silence_50ms_mono.wav", channels="right", limit = 0.1, window.width=10, stepsize=5)
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$window.width,	10)
	expect_equal(attr(o1,"params")$stepsize,		5)
	expect_equal(attr(o1,"params")$channels, 		"right")
	
	o1 <- analyse.file("../testdata/silence_50ms_mono.wav", channels="both", limit = 0.5, window.width=10, stepsize=5)
	expect_equal(attr(o1,"params")$limit,			0.5)
	expect_equal(attr(o1,"params")$window.width,	10)
	expect_equal(attr(o1,"params")$stepsize,		5)
	expect_equal(attr(o1,"params")$channels, 		"both")
	
	o1 <- analyse.file("../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, window.width=20, stepsize=5)
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$window.width,	20)
	expect_equal(attr(o1,"params")$stepsize,		5)
	expect_equal(attr(o1,"params")$channels, 		"both")
	
	o1 <- analyse.file("../testdata/silence_50ms_mono.wav", channels="both", limit = 0.1, window.width=10, stepsize=3)
	expect_equal(attr(o1,"params")$limit,			0.1)
	expect_equal(attr(o1,"params")$window.width,	10)
	expect_equal(attr(o1,"params")$stepsize,		3)
	expect_equal(attr(o1,"params")$channels, 		"both")
	
	o1 <- analyse.file("../testdata/silence_50ms_mono.wav", channels="left", limit = 0.5, window.width=20, stepsize=3)
	expect_equal(attr(o1,"params")$limit,			0.5)
	expect_equal(attr(o1,"params")$window.width,	20)
	expect_equal(attr(o1,"params")$stepsize,		3)
	expect_equal(attr(o1,"params")$channels, 		"left")
})

