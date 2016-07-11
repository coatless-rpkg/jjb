#' Install Dropbox CLI version on Linux
#' 
#' Downloads and installs the linux Dropbox CLI version
#' using advice given \url{https://www.dropbox.com/install?os=lnx}
#' @details 
#' Attempts to install Dropbox CLI support in the default location of 
#' \code{"~/Dropbox"}, \code{"~/.dropbox-dist/dropboxd"}, and 
#' \code{"~/dropbox.py"}.
#' @export
#' @examples
#' \dontrun{
#' # Install Dropbox CLI to local directory
#' install_dropbox_cli()
#' }
install_dropbox_cli = function(){

  require.linux()
  
  message("Attempting to download dropboxd... ")
  
  if(get_arch() == "x64"){
    system('cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -')
  }else{
    system('cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86" | tar xzf -')
  }
  
  message("Attempting to download the dropbox.py cli tool ... ")
  system("curl -LO https://www.dropbox.com/download?dl=packages/dropbox.py")

  
  message("Attempting to start dropboxd... \n")
  system("~/.dropbox-dist/dropboxd &")
  message("Started dropboxd ... \n")
  
  message("Done installing Dropbox CLI!")
}

#' Issue Dropbox Commands to System
#' 
#' Enables the use of the system terminal's
#' \code{"dropbox.py"} CLI interface.
#' @param dbpy_loc A \code{string} indicating the location of
#' the \code{"dropbox.py"} CLI interface.
#' @author James Joseph Balamuta
#' @export
#' @examples
#' \dontrun{
#' # Start process
#' dropbox_command("start")
#' }
dropbox_command = function(cmd, dbpy_loc = "~/"){
  message("Executing ", cmd, " via ", dbpy_loc,"dropbox.py")
  system(paste0(dbpy_loc, "dropbox.py ", cmd, intern = T))
}

#' Enable or Disable the autostartup feature
#' 
#' Uses the 
#' @param autostart A \code{boolean} indicating \code{TRUE} (enabled)
#' or \code{FALSE} (disabled) autostart of the dropbox daemon.
#' @inheritParams dropbox_command
#' @details 
#' Attempts to install Dropbox CLI support
#' @export
#' @examples
#' \dontrun{
#' # Enable Autostart
#' dropbox_autostart(T)
#' 
#' # Disable Autostart
#' dropbox_autostart(F)
#' }
dropbox_autostart = function(autostart = T, dbpy_loc = "~/"){
  
  require.linux()
  
  if(!is.logical(autostart)){
    stop("Autostart must be logical.")
  }
  
  if(autostart){
    dropbox_command("autostart y", dbpy_loc)
  }else{
    dropbox_command("autostart n", dbpy_loc)
  }
  
}



#' Start the Dropbox Daemon 
#' 
#' Turns the dropbox daemon online
#' @inheritParams dropbox_command
#' @author James Joseph Balamuta
#' @export
#' @examples
#' \dontrun{
#' # Start the daemon
#' dropbox_start()
#' }
dropbox_start = function(dbpy_loc = "~/"){
  dropbox_command("start", dbpy_loc)
}

#' Stop the Dropbox Daemon 
#' 
#' Turns the dropbox daemon off
#' @param dbpy_loc A \code{string} indicating the location of
#' the \code{"dropbox.py"} CLI interface.
#' @author James Joseph Balamuta
#' @export
#' @examples
#' \dontrun{
#' # Stop the daemon
#' dropbox_stop()
#' }
dropbox_stop = function(dbpy_loc = "~/"){
  dropbox_command("stop", dbpy_loc)
}

#' Obtain the File URL in a Public Folder
#' 
#' Returns the Public File URL in Dropbox's public folder
#' @param fp       A \code{string} containing the file path that is within the \code{"Public"} folder.
#' @param dbpy_loc A \code{string} indicating the location of
#' the \code{"dropbox.py"} CLI interface.
#' @author James Joseph Balamuta
#' @export
#' @examples
#' \dontrun{
#' # Return the file
#' dropbox_puburl("path/to/file.R")
#' }
dropbox_puburl = function(fp, dbpy_loc = "~/"){
  if(isTRUE(grepl("\\bPublic\\b",fp))){
    dropbox_command(paste0("puburl ", fp), dbpy_loc)
  }else{
    stop("Error: File is not in the `Public` folder. Please use `dropbox_sharelink()`!")
  }
}

#' Enable Live View of Dropbox File in Browser
#' 
#' @inheritParams dropbox_puburl
#' @details 
#' To get the "live" aspect of the code view,
#' we opt to set the shared link to have a \code{"?dl=0"}.
#' This forces the file to only show in the browser.
#' For more details please see:
#' \href{https://www.dropbox.com/en/help/201}{How do I force a file or folder to download or render on dropbox.com?}
#' 
#' This was inspired by \href{http://michaellevy.name/blog/useR-talk-on-teaching-R/}{Michael Levy's UseR2016 Talk}
#'  (\href{https://channel9.msdn.com/Events/useR-international-R-User-conference/useR2016/Teaching-R-to-200-people-in-a-week#time=07m20s}{See the part at 7:20})
#' @author James Joseph Balamuta
#' @export
#' @examples
#' \dontrun{
#' # Return the file with live streaming enabled
#' dropbox_puburl_live("path/to/file.R")
#' }
dropbox_puburl_live = function(fp, dbpy_loc = "~/"){
  paste0(dropbox_puburl(fp, dbpy_loc), "?dl=0")
}
