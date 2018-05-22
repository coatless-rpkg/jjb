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

#' Character at Position i
#' 
#' Returns the character at location i inside the string.
#' 
#' @param s A \code{string} to extract position from. 
#' @param i A \code{int} between 1 and length. 
#' 
#' @return A \code{string} of length 1.
#' 
#' @author James J Balamuta
#' @export
#' @examples
#' 
#' s = "statistics"
#' 
#' # Single
#' char_at(s, 1)
#' 
#' # Vectorized position
#' char_at(s, c(2, 3))
char_at = function(s, i){
  
  if(!is.character(s)){ stop("Must be a string")}
  
  a = nchar(s)
  
  if(any(i > a)) { stop("Index must within string range")}
  
  sapply(i, 
         FUN = function(position, s) { substr(s, position, position) }, 
         s = s)
}