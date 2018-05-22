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
# You should have received a copy of the GPL-3 License along with `balamuta`.
# If not, see <https://opensource.org/licenses/GPL-3.0>.

#' @title Character at Position i
#' @description Returns the character at position i.
#' @param s A \code{string} to extract position from. 
#' @param i A \code{int} between 1 and length. 
#' @return A \code{string} of length 1.
#' @author James J Balamuta
#' @export
#' @examples
#' 
#' s = "statistics"
#' charAt(s,1)
charAt = function(s,i){
  
  if(!is.character(s)){ stop("Must be a string")}
  
  a = length(s)
  
  if(i > a) { stop("Index must within string range")}
  
  substr(s,i,i)
}