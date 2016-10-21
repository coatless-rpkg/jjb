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

#' @title Integer / Whole Number Check
#' @description Checks whether the submitted vector of values is an integer / whole number
#' @param x A \code{numeric} value to check to see if it is an integer.
#' @return A \code{boolean} value indicating whether the value is an integer or not.
#' @author JJB
#' @export
#' @examples
#' is.whole(2.3)
#' is.whole(4)
#' is.whole(c(1,2,3))
#' is.whole(c(.4,.5,.6))
#' is.whole(c(7,.8,9))
is.whole = function(x){ is.numeric(x) && all(floor(x)==x) } 

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