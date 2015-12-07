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
#' @param x A \code{vector} that has length \eqn{N}.
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
floor_and_cap = function(x){
  vals = quantile(x,probs=c(.025,0.975),na.rm=TRUE)
  
  x = ifelse(x<vals[1],vals[1],x)
  x = ifelse(x>vals[2],vals[2],x)
  
  x
}
