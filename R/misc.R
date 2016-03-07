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

#' @title Is R Open in RStudio?
#' @description 
#' Detects whether R is open in RStudio. 
#' @return 
#' A \code{logical} value that indicates whether R is open in RStudio.
#' @author JJB
#' @examples
#' is.rstudio()
is.rstudio = function(){
  .Platform$GUI == "RStudio"
}

#' @title Change Default Graphing Device from RStudio
#' @description 
#' Checks to see if the user is in RStudio. If so, then it changes the device to a popup window. 
#' @param ext A \code{logical} indicating whether the graph should be done externally or internally in RStudio.
#' @details 
#' Depending on the operating system, the default drivers attempted to be used are:
#' 
#' OS X and Linux: quartz()
#' 
#' Windows: windows()
#' 
#' Note, this setting is not permanent. Thus, the behavioral change will last until the end of the session. 
#'
#' Also, the active graphing environment will be killed. As a result, any graphs that are open will be deleted. You will have to regraph them. 
#' @author JJB
#' @examples
#' \dontrun{
#' # Turn on external graphs
#' external_graphs()
#' 
#' # Turn off external graphs
#' external_graphs(F)
#' }
external_graphs = function(ext = TRUE){
  if( is.rstudio() ){
    if(isTRUE(ext)){
      o = tolower(Sys.info()["sysname"])
      if(o == "darwin" || o == "linux"){
        options("device"="quartz")
      }else if(o == "windows"){
        options("device"="windows")
      }
    } else{
      options("device"="RStudioGD")
    }
    
    # Kill open graphic devices
    graphics.off()
  }
}
