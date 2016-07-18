# Generic S3 Methods for working with features
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

#' Return a type identifier for a feature time series
#' 
#' This function returns a type identifier for a feature
#' series. In most cases this should correspond to the
#' primary class of the feature time series.
#' 
#' @param	fs	The time series object
#' 
#' @export
feature.type <- function(fs) 
{
	UseMethod("feature.type")
}
	