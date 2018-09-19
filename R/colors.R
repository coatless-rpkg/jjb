# Copyright (C) 2015 - 2018  James Balamuta
#
# This file is part of `jjb` R Package
#
# The `jjb` R package is free software: you can redistribute it and/or modify
# it under the terms of the GPL-3 LICENSE included within the packages source 
# as the LICENSE file.
#
# The `jjb` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the GPL-3 License along with `jjb`.
# If not, see <https://opensource.org/licenses/GPL-3.0>.

#' Emulate ggplot2 default color palette
#'
#' Autogenerate a colors according to the ggplot selection mechanism. 
#' 
#' @param n An `integer` indicating how many colors user wants.
#' 
#' @return A `vector` containing `n` colors
#' 
#' @author John Colby
#' @export
#' @details 
#' This implementation is based off an answer given on StackOverflow by
#' John Colby. 
#' @examples
#' 
#' # Only 1 color
#' gg_color(1)
#' 
#' # Five colors
#' gg_color(5)
#' @references 
#' <https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette>
gg_color = function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 70, c = 100)[-(n + 1)]
}

#' Convert 0-255 to a Hex number
#'
#' This is a helper function for [`rgb_to_hex`]. This function takes a
#' single R, G, or B numeric value and converts it to hex.
#' 
#' @param n An `int`
#' 
#' @return A `string` of length 2. 
#' @export
#' @examples
#' int_to_hex(22)
int_to_hex = function(n) {
  
  if (is.nan(n) || is_whole(n))
    return("00")
  
  n = max(0, min(n, 255))
  s = "0123456789ABCDEF"
  
  paste0(char_at(s, (n - n %% 16) / 16 + 1), char_at(s, n %% 16 + 1))
}

#' Convert RGB Value to Hexadecimal
#'
#' This function converts an RGB value to the hexadecimal numbering system.
#' 
#' @param R     A `int` that is between 0 and 255 for the Red value.
#' @param G     A `int` that is between 0 and 255 for the Green value.
#' @param B     A `int` that is between 0 and 255 for the Blue value.
#' @param pound A `bool` that indicates whether a pound sign should be
#'              prepended to the hexadecimal.
#'              
#' @return A `string` containing the hexadecimal information.
#' @export
#' @examples
#' # Hexadecimal with pound sign
#' rgb_to_hex(255,255,255)
#' 
#' # Heaxadecimal without pound sign
#' rgb_to_hex(255,255,255,FALSE)
rgb_to_hex = function(R, G, B, pound = TRUE) {
  
  paste0(if (pound) {
    "#"
  } else{
    ""
  }, int_to_hex(R), int_to_hex(G), int_to_hex(B))
  
}

#' Tint an RGB value
#'
#' The function tints or lightens an RGB value by adding white to the values.
#' 
#' @param rgb_value   A `vector` with length \eqn{3 \times 1}{3 x 1}.
#' @param tint_factor A `double` that ranges between \eqn{[0, 1]}.
#' 
#' @return A `matrix` with dimensions \eqn{3 \times 1}{3 x 1}.
#' 
#' @export
#' @examples
#' tint(c(22, 150, 230), tint_factor = 0.5)
tint = function(rgb_value, tint_factor = 0.2) {
  if (tint_factor > 1 ||
      tint_factor < 0) {
    stop("Invalid tint factor")
  }
  if (!is.vector(rgb_value) ||
      length(rgb_value) != 3) {
    stop("Invalid rgbval vector")
  }
  
  rgb_value[1] = rgb_value[1] + (255 - rgb_value[1]) * tint_factor
  rgb_value[2] = rgb_value[2] + (255 - rgb_value[2]) * tint_factor
  rgb_value[3] = rgb_value[3] + (255 - rgb_value[3]) * tint_factor
  
  rgb_value
}

#' Shade an RGB value
#'
#' The function shades or darkens an RGB value by adding black to the values.
#' 
#' @param rgb_value    A `vector` with length \eqn{3 \times 1}{3 x 1}.
#' @param shade_factor A `double` that ranges between \eqn{[0, 1]}.
#' 
#' @return A `matrix` with dimensions \eqn{3 \times 1}{3 x 1}.
#' 
#' @export
#' @examples
#' shade(c(22, 150, 230), shade_factor = 0.5)
shade = function(rgb_value, shade_factor = 0.1) {
  if (shade_factor > 1 ||
      shade_factor < 0) {
    stop("Invalid shade factor")
  }
  if (!is.vector(rgb_value) ||
      length(rgb_value) != 3) {
    stop("Invalid rgbval vector")
  }
  
  
  rgb_value[1] = rgb_value[1] * (1 - shade_factor)
  rgb_value[2] = rgb_value[2] * (1 - shade_factor)
  rgb_value[3] = rgb_value[3] * (1 - shade_factor)
  
  rgb_value
}
