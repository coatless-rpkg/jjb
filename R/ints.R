# Copyright (C) 2015  James Balamuta
#
# This file is part of `balamuta` R Package
#
# The `balamuta` R package is free software: you can redistribute it and/or modify it
# under the terms of the MIT LICENSE
# included within the packages source as the LICENSE file.
#
# The `balamuta` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the MIT License along with `balamuta`. If not, see <https://opensource.org/licenses/MIT>.

#' @title Integer Check
#' @description Checks whether the submitted value is an integer
#' @keywords internal
#' @param x A \code{numeric} value to check to see if it is an integer.
#' @return A \code{boolean} value indicating whether the value is an integer or not.
#' @author James J Balamuta
#' @examples
#' is.whole(2.3)
#' is.whole(4)
#' is.whole(c(1,2,3))
#' is.whole(c(.4,.5,.6))
#' is.whole(c(7,.8,9))
is.whole = function(x){ is.numeric(x) && all(floor(x)==x) } 