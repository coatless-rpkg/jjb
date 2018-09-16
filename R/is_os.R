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

# Store system name
sys.name = Sys.info()[['sysname']]

# Figure out OS type
sys.arch = if( Sys.info()['machine'] == "x86_64" ) { "x64" } else { "x32" }


#' Check for an Operating System
#' 
#' Performs a check to determine the OS
#' @return Either \code{TRUE} or \code{FALSE}
#' @rdname is_os
#' @export
#' @author James Joseph Balamuta
is_windows = function(){
  sys.name == "Windows"
}

#' @rdname is_os
#' @export
is_macos = function(){
  sys.name == "Darwin"
}

#' @rdname is_os
#' @export
is_linux = function(){
  sys.name == "Linux"
}

#' @rdname is_os
#' @export
is_sun = function(){
  sys.name == "SunOS"
}


#' Require a Specific Operating System
#' 
#' Mandates the presence of an operating system
#' @details 
#' If any of these functions are called
#' on the wrong operating system. A stop error 
#' is triggered and the function will fail.
#' @rdname require_os
#' @export
#' @author James Joseph Balamuta
require_linux = function(){
  if(!is_linux()){
    stop("This function is only supported on Linux!")
  }
}

#' @rdname require_os
#' @export
require_windows = function(){
  if(!is_windows()){
    stop("This function is only supported on Windows!")
  }
}

#' @rdname require_os
#' @export
require_macos = function(){
  if(!is_macos()){
    stop("This function is only supported on MacOS!")
  }
}

#' @rdname require_os
#' @export
require_sun = function(){
  if(!is_sun()){
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
