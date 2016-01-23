# Copyright (C) 2015  James Balamuta
#
# This file is part of `balamuta` R Package
#
# The `balamuta` R package is free software: you can redistribute it and/or modify it
# under the terms of the MIT LICENSE included within the packages source as the LICENSE file.
#
# The `balamuta` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the MIT License along with `balamuta`. 
# If not, see <https://opensource.org/licenses/MIT>.

#' @title Floor and Cap a dataset
#' @description Floors and Caps a numeric variable.
#' @param x      A \code{vector} that has length \eqn{N}.
#' @param probs  A \code{vector} containing two values between 0 and 1, with the first being less than the second.
#' @return A \code{vector} with the values floored and capped. 
#' @author James J Balamuta
#' @examples 
#' 
#' # One case version
#' n = 100
#' 
#' x = rnorm(n)
#' 
#' x[n-1] = -99999
#' x[n] = 10000
#' 
#' y = floor_and_cap(x)
#' 
#' # Dataset example
#' 
#' d = data.frame(x,y=rnorm(n))
#' 
#' o = sapply(d,floor_and_cap)
floor_and_cap = function(x, probs=c(.025,0.975)){
  
  if(length(probs) != 2){stop("Must provide two values for `probs`: one floor and one cap.")}
  if(probs[1] > probs[2]){stop("The first element in `probs` must be less than the second element.")}
  
  
  vals = quantile(x, probs=probs, na.rm=TRUE)
  
  x = ifelse(x<vals[1],vals[1],x)
  x = ifelse(x>vals[2],vals[2],x)
  
  x
}



#' Convert Multiple Columns of a data.frame
#'  
#' All at once conversion of a data.frame from current column types to alternates.
#' 
#' @param d    A \code{data.frame} that needs to have specific columns converted.
#' @param cast A \code{string vector} containing either \code{"n"} (numeric), \code{"c"} (character), or \code{"f"} (factor).
#' @return A \code{data.frame} with converted column types.
#' @author James J Balamuta
#' @examples 
#' 
#' n = 100
#' 
#' st = sample(LETTERS, n, replace = TRUE)
#' sr = sample(letters, n, replace = TRUE)
#' num = rnorm(n)
#' 
#' d = data.frame(x=st, y=num, z = sr, stringsAsFactors = FALSE)
#' 
#' # Convert all columns
#' 
#' o = convert_cols(d,c("f","c","f"))
#' 
#' # Convert a subset
#' d[,c(1,3)] = convert_cols(d[,c(1,3)],c("f","f"))
convert_cols = function(d,cast){
  
  if(!is.data.frame(d)){stop("`d` must be a `data.frame`")}
  if(ncol(d)!=length(cast)){stop("Number of cast must match number of `d` columns ")}
  
  d2 = lapply(1:ncol(d),
              FUN = function(i){
                FCALL = switch(cast[i],
                               c = as.character,
                               n = as.numeric,
                               f = as.factor);
                FCALL(d[,i])
              })
  
  names(d2) = colnames(d)
  
  # Reset as data.frame and cast
  as.data.frame(d2, stringsAsFactors = FALSE)
}

