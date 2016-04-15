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

#' @title Integer / Whole Number Check
#' @description Checks whether the submitted vector of values is an integer / whole number
#' @param x A \code{numeric} value to check to see if it is an integer.
#' @return A \code{boolean} value indicating whether the value is an integer or not.
#' @author JJB
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
#' @examples 
#' x = rnorm(10)
#' 
#' lagged(x, 2)
lagged = function(x, lag = 1){
  if(lag == 0){return(x)}
  x[1:lag] = NA
  
  return(x)
}


#' Integer Bins with Overflow
#' 
#' Creates a vector at equal level with the number of items per bin
#' with the overflow going to the last bin. That is, the last bin
#' is filled with less elements than the prior bins. 
#' 
#' @param n    A \code{integer} that is the total number of objects.
#' @param bins A \code{integer} indicating the number of bins. 
#' @param from A \code{integer} indicating where the sequence should start.
#' @return A \code{vector} containing integer binning. 
#' @examples 
#' # Evenly spaced
#' a = seq_bin(100, 2)
#' 
#' b = seq_bin(100, 4)
#' 
#' # Oddly spaced
#' d = seq_bin(101,5)
#' 
#' # First four bins equal, last differs.
#' diff(d)
seq_bin = function (n, bins = 2, from = 1) {
  
  if(!is.whole(c(n,bins,from))){ stop("`n`, `bins`, and `from` must all be whole.") }
  
  if(bins == 1L) return(n)
  
  per_tick = n/bins
  
  if(is.whole(per_tick)){
    offset_val = 1L
  }else{
    per_tick = n/(bins-1)
    offset_val = 2L
  }
  
  o = from + (0L:(bins - offset_val)) * ceiling(per_tick)
  
  if(n %% bins != 0){
    o = c(o, n)
  }
  
  o
}