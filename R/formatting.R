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

#' Export Data in Scientific Notation to LaTeX 
#' 
#' Takes a vector of observations and converts them to have the
#' scientific notation in LaTeX. E.g. \eqn{0.0003 \Rightarrow 3 \cdot 10^{-4}}
#' @param x        A \code{numeric vector} to be formatted. 
#' @param rounding A \code{integer} that indicates how many decimal places should exist.
#' @param align    A \code{bool} that indicates whether phantom padding should be added to force column into alignment.
#' @return
#' A \code{character vector} containing the latex notation. 
#' @author Stephane Guerrier & James Balamuta
#' @export
#' @examples 
#' 
#' # For reproducibility
#' set.seed(1337)
#' 
#' x = rnorm(5,.0001,.2)
#' 
#' nb2latex(x)
#' 
#' nb2latex(x, rounding = 5)
#' 
#' nb2latex(x, rounding = 5, align = FALSE)
nb2latex = function(x, rounding = 4, align = TRUE){
  
  pow = floor(log10(abs(x)))
  x = x/10^(pow)
  nb = round(x,rounding)
  
  n = length(x)
  add = rep("",n)
  
  if(align){
    d = nchar(pow)
    m = max(d)
    o = m-d
    for(i in 1:n){
      if(o[i] != 0){
        add[i] = paste("\\phantom{",rep(1,o[i]),"}")
      }
    }
  }
  
  paste0("$",sprintf(paste0("%.",rounding,"f"),nb),"\\cdot 10^{",pow,add,"}$")
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