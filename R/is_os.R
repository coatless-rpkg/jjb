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

# Store system name
system_name = function() {
  Sys.info()[['sysname']]
}

#' Check for an Operating System
#' 
#' Performs a check to determine the OS
#' 
#' @return 
#' Either \code{TRUE} or \code{FALSE}
#' 
#' @author 
#' James Joseph Balamuta
#' 
#' @rdname is_os
#' @export
is_windows = function(){
  system_name() == "Windows"
}

#' @rdname is_os
#' @export
is_macos = function(){
  system_name() == "Darwin"
}

#' @rdname is_os
#' @export
is_linux = function(){
  system_name() == "Linux"
}

#' @rdname is_os
#' @export
is_sun = function(){
  system_name() == "SunOS"
}


#' Require a Specific Operating System
#' 
#' Mandates the presence of an operating system
#' 
#' @details 
#' If any of these functions are called
#' on the wrong operating system. A stop error 
#' is triggered and the function will fail.
#' 
#' @author 
#' James Joseph Balamuta
#' 
#' @rdname require_os
#' @export
require_linux = function(){
  if(!is_linux()){
    stop("This function is only supported on Linux!", call. = FALSE)
  }
}

#' @rdname require_os
#' @export
require_windows = function(){
  if(!is_windows()){
    stop("This function is only supported on Windows!", call. = FALSE)
  }
}

#' @rdname require_os
#' @export
require_macos = function(){
  if(!is_macos()){
    stop("This function is only supported on MacOS!", call. = FALSE)
  }
}

#' @rdname require_os
#' @export
require_sun = function(){
  if(!is_sun()){
    stop("This function is only supported on Solaris!", call. = FALSE)
  }
}

#' System Architecture
#' 
#' @return Either "x64" or "x32"
#' @export
system_arch = function() {
  if( Sys.info()['machine'] == "x86_64" ) 
    "x64" 
  else 
    "x32" 
}
