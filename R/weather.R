# Copyright (C) 2015 - 2020  James Balamuta
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

## redo this file using a roxygen2 template

#' Celsius to Fahrenheit Conversion
#'
#' Converts temperature recorded in Celsius to Fahrenheit.
#'
#' @param t_celsius Temperature recorded in Celsius.
#' 
#' @return 
#' A `numeric` vector.
#' 
#' @export
#' @examples 
#' 
#' celsius_to_fahrenheit(33)
#' 
#' celsius_to_fahrenheit(0)
celsius_to_fahrenheit = function(t_celsius) {
  stopifnot(is.numeric(t_celsius))
  
  t_celsius * 9 / 5 + 32
}

#' Fahrenheit to Celsius Conversion
#'
#' Converts temperature recorded in Fahrenheit to Celsius.
#'
#' @param t_fahrenheit Temperature recorded in Fahrenheit.
#' 
#' @return 
#' A `numeric` vector.
#' 
#' @export
#' 
#' @examples 
#' 
#' fahrenheit_to_celsius(92)
#' 
#' fahrenheit_to_celsius(32)
fahrenheit_to_celsius = function(t_fahrenheit) {
  stopifnot(is.numeric(t_fahrenheit))
  
  5 * (t_fahrenheit - 32) / 9
}


#' Kelvin to Celsius Conversion
#'
#' Converts temperature recorded in Kelvin to Celsius.
#'
#' @param t_kelvin Temperature recorded in Kelvin.
#' 
#' @return
#' A `numeric` vector.
#' 
#' @export
#' 
#' @examples 
#' kelvin_to_celsius(92)
#' 
#' kelvin_to_celsius(32)
kelvin_to_celsius = function(t_kelvin) {
  stopifnot(is.numeric(t_kelvin))
  
  t_kelvin - 273.15
}

#' Celsius to Kelvin Conversion
#'
#' Converts temperature recorded in Celsius to Kelvin.
#'
#' @param t_celsius Temperature recorded in Celsius.
#' 
#' @return 
#' A `numeric` vector.
#' 
#' @export
#' @examples 
#' celsius_to_kelvin(92)
#' 
#' celsius_to_kelvin(32)
celsius_to_kelvin = function(t_celsius) {
  stopifnot(is.numeric(t_celsius))
  
  t_celsius + 273.15
}


#' Fahrenheit to Kelvin to Conversion
#'
#' Converts temperature recorded in Fahrenheit to Kelvin.
#'
#' @param t_fahrenheit Temperature recorded in Fahrenheit.
#' 
#' @return 
#' A `numeric` vector.
#' 
#' @export
#' @examples 
#' fahrenheit_to_kelvin(92)
#' 
#' fahrenheit_to_kelvin(32)
fahrenheit_to_kelvin = function(t_fahrenheit) {
  stopifnot(is.numeric(t_fahrenheit))
  
  (t_fahrenheit + 459.67) * 5 / 9
}

#' Kelvin to Fahrenheit Conversion
#'
#' Converts temperature recorded in Celsius to Kelvin.
#'
#' @param t_kelvin Temperature recorded in Kelvin.
#' 
#' @return 
#' A `numeric` vector.
#' 
#' @export
#' @examples 
#' kelvin_to_fahrenheit(92)
#' 
#' kelvin_to_fahrenheit(32)
kelvin_to_fahrenheit = function(t_kelvin) {
  stopifnot(is.numeric(t_kelvin))
  
  (t_kelvin * 5 / 9) - 459.67
}
