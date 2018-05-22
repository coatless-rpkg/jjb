# Copyright (C) 2015 - 2018  James Balamuta
#
# This file is part of `bmisc` R Package
#
# The `bmisc` R package is free software: you can redistribute it and/or modify
# it under the terms of the GPL-3 LICENSE included within the packages source 
# as the LICENSE file.
#
# The `bmisc` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the GPL-3 License along with `bmisc`.
# If not, see <https://opensource.org/licenses/GPL-3.0>.

#' Integer Check
#' 
#' Checks whether the submitted value is an `integer`
#' 
#' @param x A `numeric` value to check to see if it is an `integer`.
#' 
#' @return A `boolean` value indicating whether the value is an `integer` or not.
#' 
#' @author James J Balamuta
#' 
#' @export
#' @examples
#' is_whole(2.3)
#' is_whole(4)
#' is_whole(c(1,2,3))
#' is_whole(c(.4,.5,.6))
#' is_whole(c(7,.8,9))
is_whole = function(x) { is.numeric(x) && all(floor(x) == x) } 