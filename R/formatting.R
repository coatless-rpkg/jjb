#' Export Data in Scientific Notation to LaTeX 
#' 
#' Takes a vector of observations and converts them to have the
#' scientific notation in LaTeX. E.g. 0.0003 => 3 \\cdot 10^{-4}
#' @param x        A \code{numeric vector} to be formatted. 
#' @param rounding A \code{integer} that indicates how many decimal places should exist.
#' @return
#' A \code{character vector} containing the latex notation. 
#' @author Stephane Guerrier & JJB
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
nb2latex = function(x, rounding = 4){
  pow = floor(log10(x))
  x = x/10^(pow)
  nb = round(x,rounding)
  paste0("$",sprintf(paste0("%.",rounding,"f"),nb),"\\cdot 10^{",pow,"}$")
}