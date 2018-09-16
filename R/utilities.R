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


#' Make Directory
#' 
#' Create a directory using either a relative path or an absolute path.
#' 
#' @param dir A \code{string} indicating the directory to make.
#' @param r   A \code{boolean} that indicates whether the directories should be made recursively
#' @return New directory on file system
#' @export
#' @examples 
#' \dontrun{
#' # Make directory from working directory
#' mkdir("toad")
#' 
#' # Make directory from absolute path
#' mkdir("C:/path/to/dir/toad")
#' }
mkdir = function(dir, r = TRUE) {
  if(length(dir) != 1){
    stop("Only one directory can be created at a time.")
  }
  
  if(!dir.exists(dir)) {
    if(dir != "."){
      dir.create(dir,  recursive = r)
    }
  }
} 

#' Lag Vector Values
#' 
#' Provides a lagging mechanism for vector data.
#' @param x   A \code{vec} of data.
#' @param lag An \code{integer}.
#' @return A \code{vector} with lagged values / NAs. 
#' @export
#' @examples 
#' x = rnorm(10)
#' 
#' lagged(x, 2)
lagged = function(x, lag = 1){
  if(lag == 0){return(x)}
  x[1:lag] = NA
  
  return(x)
}
