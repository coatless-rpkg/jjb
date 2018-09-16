# Copyright (C) 2015 - 2018  James Balamuta
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

#' Pad Numeric Numbers
#' 
#' Add zeros before start of the number
#' @param x  A `vector`
#' @return A `character vector` that is padded to the length of the
#' maximum entry.
#' @author James Balamuta
#' @export
#' @examples 
#' # Padding applied
#' pad_number(8:10)
#' 
#' # No padding applied
#' pad_number(2:3)
#' 
#' # Pads non-negative number with 0.
#' # This needs to be improved slightly...
#' pad_number(-1:1)
pad_number = function(x){
  nlen = max(nchar(x))
  sprintf(paste0("%0",nlen,"d"), x)
}

#' Create a "safe" url title
#' 
#' Takes a string, forces characters to lower case, then removes punctuation and switch spaces to - instead of _
#' @param st A \code{string} that needs to be a title in a url
#' @return A \code{string} with the aforementioned modifications.
#' @author James Balamuta
#' @export
#' @examples 
#' url_title("My Name is Jaime!")
url_title = function(st){
  st = tolower(st)
  gsub("[[:space:]]","-",gsub("[[:punct:]]","",st))
}
