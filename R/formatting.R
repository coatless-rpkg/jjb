#' Export Data in Scientific Notation to LaTeX 
#' 
#' Takes a vector of observations and converts them to have the
#' scientific notation in LaTeX. E.g. 0.0003 => 3 \\cdot 10^{-4}
#' @param x        A \code{numeric vector} to be formatted. 
#' @param rounding A \code{integer} that indicates how many decimal places should exist.
#' @param align    A \code{bool} that indicates whether phantom padding should be added to force column into alignment.
#' @return
#' A \code{character vector} containing the latex notation. 
#' @author Stephane Guerrier & JJB
#' @export
#' @examples 
#' 
#' # For reproducibility
#' set.seed(1337)
#' 
#' x = rnorm(5,.0001,.2)
#' 
#' nb2latex(x)
#' 
#' nb2latex(x, rounding = 5)
#' 
#' nb2latex(x, rounding = 5, align = FALSE)
nb2latex = function(x, rounding = 4, align = TRUE){
  
  pow = floor(log10(abs(x)))
  x = x/10^(pow)
  nb = round(x,rounding)
  
  n = length(x)
  add = rep("",n)
  
  if(align){
    d = nchar(pow)
    m = max(d)
    o = m-d
    for(i in 1:n){
      if(o[i] != 0){
        add[i] = paste("\\phantom{",rep(1,o[i]),"}")
      }
    }
  }
  
  paste0("$",sprintf(paste0("%.",rounding,"f"),nb),"\\cdot 10^{",pow,add,"}$")
}


#' Create a "safe" url title
#' 
#' Takes a string, forces characters to lower case, then removes punctuation and switch spaces to - instead of _
#' @param st A \code{string} that needs to be a title in a url
#' @return A \code{string} with the aforementioned modifications.
#' @author JJB
#' @export
#' @examples 
#' url_title("My Name is Jaime!")
url_title = function(st){
  st = tolower(st)
  gsub("[[:space:]]","-",gsub("[[:punct:]]","",st))
}