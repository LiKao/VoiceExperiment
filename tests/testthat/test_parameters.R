# test file for propagation of parameters to final analysis functions
#
# Added in Version 0.1.2 as test for bugfix
# 
# Author: till
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

