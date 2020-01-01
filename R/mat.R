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

#' Obtain the Trace of a Square Matrix
#' 
#' Calculates and returns the trace of a square matrix.
#' 
#' @param x A `matrix` that is square e.g. \eqn{N \times N}{N x N}
#' 
#' @return 
#' A `matrix` with circles imprinted within its dimensions.
#' 
#' @author 
#' James Balamuta
#' 
#' @export
#' @examples 
#' # I_2 matrix
#' tr(diag(2))
tr = function(x){
  if (!is.matrix(x)) {
    stop(sprintf("`x` is not a matrix. `x` is %s", class(x)), call. = FALSE)
  }
  if (nrow(x) != ncol(x)) {
    stop(sprintf("`x` must be a square matrix. `x` is %i x %i", nrow(x), ncol(x)),
         call. = FALSE)
  }
  sum(diag(x))
}

#' Create a circle pattern within a matrix
#' 
#' Takes a default matrix and embeds circles within the matrix.
#' 
#' @param m        A `int` that is the number of rows of the matrix 
#' @param n        A `int` that is the number of the columns of the matrix.
#' @param x.center A `vector` of x coordinate center position of the circle.
#' @param y.center A `vector` of y coordinate center position of the circle.
#' @param r        A `vector` of integers denoting the different circle radii.
#' @param f        A `vector` of values that specify what the inside of the
#'                 circles should be.
#' 
#' @return 
#' A `matrix` with circles imprinted within its dimensions.
#' 
#' @author 
#' James Balamuta
#' 
#' @export
#' @examples 
#' # Generate a basic circle matrix
#' circle_matrix(10, 10, 3, 4, 2)
#' 
#' # Generate two circles within the matrix
#' circle_matrix(10, 20, c(3,6), c(4,6), c(2,2))
#' 
#' # Different fills
#' circle_matrix(10, 20, c(3,6), c(4,6), c(2,2), f = c(1,2))
circle_matrix = function(m, n, x.center, y.center, r, f = 1){
  
  p = length(y.center)
  
  if(length(x.center) !=  p || length(r) != p){
    stop("need to define points for each circle")
  }
  
  if(any(x.center - r < 0) || any(x.center + r > m)){
    stop("X is outside of the center given radius")
  }
  
  if(any(y.center - r < 0) || any(y.center + r > n)){
    stop("Y is outside of the center given radius")
  }
  
  if(length(f) != p){
    f = rep(1,p)
  }
  
  h = matrix(0, nrow = m, ncol=n)
  
  for(k in 1:p){
    
    a.r = r[k]
    a.x = x.center[k]
    a.y = y.center[k]
    a.idx = f[k]
    
    b = (a.x-a.r):(a.x+a.r)
    
    c = (a.y-a.r):(a.y+a.r)
    
    for(i in b){
      for(j in c){
        if(sqrt((i - a.x)^2 + (j - a.y)^2) <= a.r){
          h[i,j] = a.idx
        }
      }
    }
  }
  
  h
}
