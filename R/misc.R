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


#' Natural Graphics Driver for Operating System
#' 
#' Provides the default operating system graphics utility
#' 
#' @return A `string` that is either:
#' 
#' - `"quartz"`: if on MacOS
#' - `"windows"`: if on Windows
#' - `"x11"`: if on Linux or Solaris
#' 
#' @seealso [is_rstudio]
#' 
#' @author James Joseph Balamuta
#' @export
#' @examples 
#' # Returns a string depending on test platform
#' get_graphic_driver()
get_graphic_driver = function(){
  switch(tolower(sys.name),
         "darwin"  = "quartz",
         "linux"   = "x11",
         "windows" = "windows",
         "sunos"   = "x11" )
}


#' Is R Open in RStudio?
#' 
#' Detects whether R is open in RStudio. 
#' 
#' @return 
#' A `logical` value that indicates whether R is open in RStudio.
#' @author JJB
#' @export
#' @examples
#' is_rstudio()
is_rstudio = function(){
  .Platform$GUI == "RStudio"
}

#' Change Default Graphing Device from RStudio
#' 
#' Checks to see if the user is in RStudio. If so, then it changes the device
#' to a popup window. 
#' 
#' @param ext A `logical` indicating whether the graph should be done externally
#'            or internally in RStudio.
#' @details 
#' Depending on the operating system, the default drivers attempted to be
#' used are:
#' 
#' - OS X: `quartz()`
#' - Linux: `x11()`
#' - Windows: `windows()`
#' 
#' Note, this setting is not permanent. Thus, the behavioral change will last
#' until the end of the session. 
#'
#' Also, the active graphing environment will be killed. As a result, any graphs
#' that are open will be deleted. You will have to regraph them. 
#' 
#' @author JJB
#' @export
#' @examples
#' \dontrun{
#' # Turn on external graphs
#' external_graphs()
#' 
#' # Turn off external graphs
#' external_graphs(F)
#' }
external_graphs = function(ext = TRUE){
  if( is_rstudio() ){
    if(isTRUE(ext)){
      a = get_graphic_driver()
      options("device" = a)
    } else{
      options("device" = "RStudioGD")
    }
    
    # Kill open graphic devices
    graphics.off()
  }
}
