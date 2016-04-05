#' Make Directory
#' 
#' Create a directory using either a relative path or an absolute path.
#' 
#' @param dir A \code{string} indicating the directory to make.
#' @param r   A \code{boolean} that indicates whether the directories should be made recursively
#' @return New directory on file system
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
