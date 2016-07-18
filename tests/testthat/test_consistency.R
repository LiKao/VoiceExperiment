# test file for consistency checks between methods
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

context("Consistency")

###############################################

test_that("Domain transformations are reversible", {

	### mel.to.freq then freq.to.mel
			
	expect_equal(freq.to.mel(mel.to.freq(    0)),	   0)
	expect_equal(freq.to.mel(mel.to.freq(    1)),	   1)
	expect_equal(freq.to.mel(mel.to.freq(  100)), 	 100)
	expect_equal(freq.to.mel(mel.to.freq(  200)), 	 200)
	expect_equal(freq.to.mel(mel.to.freq(  500)), 	 500)
	expect_equal(freq.to.mel(mel.to.freq(  800)), 	 800)
	expect_equal(freq.to.mel(mel.to.freq( 1000)),   1000)
	expect_equal(freq.to.mel(mel.to.freq( 5000)),   5000)
	expect_equal(freq.to.mel(mel.to.freq(10000)),  10000)
	expect_equal(freq.to.mel(mel.to.freq(15000)),  15000)
	expect_equal(freq.to.mel(mel.to.freq(20000)),  20000)
	
	### freq.to.mel then mel.to.freq
	
	expect_equal(mel.to.freq(freq.to.mel(    0)),	   0)
	expect_equal(mel.to.freq(freq.to.mel(    1)),	   1)
	expect_equal(mel.to.freq(freq.to.mel(  100)), 	 100)
	expect_equal(mel.to.freq(freq.to.mel(  200)), 	 200)
	expect_equal(mel.to.freq(freq.to.mel(  500)), 	 500)
	expect_equal(mel.to.freq(freq.to.mel(  800)), 	 800)
	expect_equal(mel.to.freq(freq.to.mel( 1000)),   1000)
	expect_equal(mel.to.freq(freq.to.mel( 5000)),   5000)
	expect_equal(mel.to.freq(freq.to.mel(10000)),  10000)
	expect_equal(mel.to.freq(freq.to.mel(15000)),  15000)
	expect_equal(mel.to.freq(freq.to.mel(20000)),  20000)
})