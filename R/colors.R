# Copyright (C) 2015  James Balamuta
#
# This file is part of `balamuta` R Package
#
# The `balamuta` R package is free software: you can redistribute it and/or modify it
# under the terms of the MIT LICENSE
# included within the packages source as the LICENSE file.
#
# The `balamuta` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the MIT License along with `balamuta`. If not, see <https://opensource.org/licenses/MIT>.

#' @title Emulate ggplot2 default color palette
#' @description Autogenerate a colors according to the ggplot selection mechanism. 
#' @param n An \code{integer} indicating how many colors user wants.
#' @return A \code{vector} containing \code{n} colors
#' @author John Colby
ggColor <- function(n) {
  hues = seq(15, 375, length=n+1)
  rev(hcl(h=hues, l=70, c=100)[1:n])
}

#' @title Convert 0-255 to a Hex number
#' @description This is a helper function for \link{rgb2hex}.
#' This function takes a single R, G, or B numeric value and converts it to hex.
#' @param n An \code{int}
#' @return A \code{string} of length 2. 
#' @examples
#' toHex(22)
toHex = function(n) {
  if (is.nan(n) || is.whole) return("00")
  
  n = max(0, min(n,255))
  s = "0123456789ABCDEF"
  
  paste0(charAt(s,(n-n%%16)/16 + 1), charAt(s, n%%16 + 1))
}

#' @title Convert RGB Value to Hexadecimal
#' @description This is a helper function for \link{rgb2hex}. 
#' @param n An \code{int}
#' @return A \code{string} of length 2. 
#' @examples
#' toHex(22)
rgb2hex = function(R,G,B,pound=T) {
  
  paste0(if(pound){"#"}else{""},toHex(R),toHex(G),toHex(B))
  
}

#' @title Tint an RGB value
#' @description The function tints or lightens an RGB value by adding white to the values.
#' @param rgbval A \code{matrix} with dimensions \eqn{3 \times 1}{3 x 1}.
#' @param tint_factor A \code{double} that ranges between [0,1].
#' @return A \code{matrix} with dimensions \eqn{3 \times 1}{3 x 1}.
#' @examples
#' tint(c(22,150,230), tint_factor = 0.5)
tint = function(rgbval, tint_factor = 0.2){
  
  if(tint_factor > 1 || tint_factor < 0){ stop("Invalid tint factor")}
  if(!is.matrix(rgbval) || nrow(rgbval) != 3){ stop("Invalid rgbval matrix")}
  
  rgbval[1,] = rgbval[1,] + (255 - rgbval[1,]) * tint_factor
  rgbval[2,] = rgbval[2,] + (255 - rgbval[2,]) * tint_factor
  rgbval[3,] = rgbval[3,] + (255 - rgbval[3,]) * tint_factor
  
  rgbval
}

#' @title Shade an RGB value
#' @description The function shades or darkens an RGB value by adding black to the values.
#' @param rgbval A \code{matrix} with dimensions \eqn{3 \times 1}{3 x 1}.
#' @param shade_factor A \code{double} that ranges between [0,1].
#' @return A \code{matrix} with dimensions \eqn{3 \times 1}{3 x 1}.
#' @examples
#' shade(c(22,150,230), shade_factor = 0.5)
shade = function(rgbval, shade_factor = 0.1){
  
  if(shade_factor > 1 || shade_factor < 0){ stop("Invalid shade factor")}
  if(!is.matrix(rgbval) || nrow(rgbval) != 3){ stop("Invalid rgbval matrix")}
  
  
  rgbval[1,] = rgbval[1,] * (1 - shade_factor)
  rgbval[2,] = rgbval[2,] * (1 - shade_factor)
  rgbval[3,] = rgbval[3,] * (1 - shade_factor)
  
  rgbval
}
