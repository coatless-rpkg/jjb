

#' Install Dropbox CLI version on Linux
#' 
#' Downloads and installs the linux Dropbox CLI version
#' using advice given \url{https://www.dropbox.com/install?os=lnx}
#' @param autostart A \code{boolean} indicating whether autostart should be enabled
#' or disabled. 
#' @details 
#' Attempts to install Dropbox CLI support
#' @export
install_dropbox_cli = function(autostart = TRUE){

  require.linux()
  
  if(!is.logical(autostart)){
    stop("Autostart must be logical.")
  }
  
  message("Attempting to download dropboxd... ")
  
  if(get_arch() == "x64"){
    system('cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -')
  }else{
    system('cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86" | tar xzf -')
  }
  
  message("Attempting to start dropboxd... ")
  system("~/.dropbox-dist/dropboxd &")
  message("Started dropboxd ... ")
  
  if(autostart){
    message("Attempting to enabling autostart")
    system("dropbox autostart y")
  }
  
  message("Done installing Dropbox CLI!")
}