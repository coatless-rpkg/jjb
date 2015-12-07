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

#' @title Create a circle pattern within a matrix
#' @description Takes a default matrix and embeds circles within the matrix.
#' @param m A \code{int} that is the number of rows of the matrix 
#' @param n A \code{int} that is the number of the columns of the matrix.
#' @param x.center A \code{vector} of x coordinate center position of the circle.
#' @param y.center A \code{vector} of y coordinate center position of the circle.
#' @param r A \code{vector} of integers denoting the different circle radii.
#' @param f A \code{vector} of values that specify what the inside of the circles should be.
#' @return A \code{matrix} with circles imprinted within its dimensions.
#' @author James J Balamuta
#' @examples 
#' 
#' # Generate a basic circle matrix
#' circle_matrix(dims = c(10,10), 3, 4, 2)
#' 
#' # Generate two circles within the matrix
#' circle_matrix(dims = c(10,10), c(3,6), c(4,6), c(2,2))
#' 
#' # Different fills
#' circle_matrix(dims = c(10,10), c(3,6), c(4,6), c(2,2), f = c(1,2))
circle_matrix = function(dims, x.center, y.center, r, f = 1){
  
  xlim = dims[1]
  ylim = dims[2]
  
  p = length(y.center)
  
  if(length(x.center) !=  p || length(r) != p){
    stop("need to define points for each circle")
  }
  
  if(any(x.center - r < 0) || any(x.center + r > xlim)){
    stop("X is outside of the center given radius")
  }
  
  if(any(y.center - r < 0) || any(y.center + r > ylim)){
    stop("Y is outside of the center given radius")
  }
  
  if(length(f) != p){
    f = rep(1,p)
  }
  
  h = matrix(0, nrow = xlim, ncol=ylim)
  
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
