# Copyright (C) 2015 - 2020  James Balamuta
#
# This file is part of `jjb` R Package
#
# The `jjb` R package is free software: you can redistribute it and/or modify
# it under the terms of the GPL-3 LICENSE included within the packages source 
# as the LICENSE file.
#
# The `jjb` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the GPL-3 License along with `jjb`.
# If not, see <https://opensource.org/licenses/GPL-3.0>.

#' Character at Position _i_
#' 
#' Returns the character at location _i_ inside the string.
#' 
#' @param x     A `character vector` to extract position from. 
#' @param index An `integer` between 1 and length _n_. 
#' 
#' @return 
#' A `character vector` of length `index`.
#' 
#' @author 
#' James J Balamuta
#' 
#' @export
#' @examples
#' # Example string
#' s = "statistics"
#' 
#' # Single character
#' char_at(s, 1)
#' 
#' # Vectorized position
#' char_at(s, c(2, 3))
char_at = function(x, index){
  
  if(!is.character(x)){ stop("Must be a string")}
  
  a = nchar(x)
  
  if(any(index > a)) { stop("Must have the values of `i` within string range")}
  
  sapply(index, 
         FUN = function(position, s) { substr(s, position, position) }, 
         s = x)
}
