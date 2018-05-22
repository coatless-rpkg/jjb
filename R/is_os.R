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

# Store system name
sys.name = Sys.info()[['sysname']]

# Figure out OS type
sys.arch = if( Sys.info()['machine'] == "x86_64" ) { "x64" } else { "x32" }


#' Check for an Operating System
#' 
#' Performs a check to determine the OS
#' @return Either \code{TRUE} or \code{FALSE}
#' @rdname is.os
#' @export
#' @author James Joseph Balamuta
is.windows = function(){
  sys.name == "Windows"
}

#' @rdname is.os
#' @export
is.macos = function(){
  sys.name == "Darwin"
}

#' @rdname is.os
#' @export
is.linux = function(){
  sys.name == "Linux"
}

#' @rdname is.os
#' @export
is.sun = function(){
  sys.name == "SunOS"
}


#' Require a Specific Operating System
#' 
#' Mandates the presence of an operating system
#' @details 
#' If any of these functions are called
#' on the wrong operating system. A stop error 
#' is triggered and the function will fail.
#' @rdname require.os
#' @export
#' @author James Joseph Balamuta
require.linux = function(){
  if(!is.linux()){
    stop("This function is only supported on Linux!")
  }
}

#' @rdname require.os
#' @export
require.windows = function(){
  if(!is.windows()){
    stop("This function is only supported on Windows!")
  }
}

#' @rdname require.os
#' @export
require.macos = function(){
  if(!is.macos()){
    stop("This function is only supported on MacOS!")
  }
}

#' @rdname require.os
#' @export
require.sun = function(){
  if(!is.sun()){
    stop("This function is only supported on Solaris!")
  }
}

#' System Architecture
#' 
#' @return Either "x64" or "x32"
#' @export
get_arch = function(){
  sys.arch
}