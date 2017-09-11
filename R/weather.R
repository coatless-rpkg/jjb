## redo this file using a roxygen2 template

#' Celsius to Fahrenheit Conversion
#'
#' Converts temperature recorded in Celsius to Fahrenheit.
#'
#' @param t_celsius Temperature recorded in Celsius.
#' @return A `numeric` vector.
#' @export
celsius_to_fahrenheit = function(t_celsius) {
  stopifnot(is.numeric(t_celsius))
  
  t_celsius * 9 / 5 + 32
}

#' Fahrenheit to Celsius Conversion
#'
#' Converts temperature recorded in Fahrenheit to Celsius.
#'
#' @param t_fahrenheit Temperature recorded in Fahrenheit.
#' @return A `numeric` vector.
#' @export
fahrenheit_to_celsius = function(t_fahrenheit) {
  stopifnot(is.numeric(t_fahrenheit))
  
  5 * (t_fahrenheit - 32) / 9
}


#' Kelvin to Celsius Conversion
#'
#' Converts temperature recorded in Kelvin to Celsius.
#'
#' @param t_kelvin Temperature recorded in Kelvin.
#' @return A `numeric` vector.
#' @export
kelvin_to_celsius = function(t_kelvin) {
  stopifnot(is.numeric(t_kelvin))
  
  t_kelvin - 273.15
}

#' Celsius to Kelvin Conversion
#'
#' Converts temperature recorded in Celsius to Kelvin.
#'
#' @param t_celsius Temperature recorded in Celsius.
#' @return A `numeric` vector.
#' @export
celsius_to_kelvin = function(t_celsius) {
  stopifnot(is.numeric(t_celsius))
  
  t_celsius + 273.15
}


#' Fahrenheit to Kelvin to Conversion
#'
#' Converts temperature recorded in Fahrenheit to Kelvin.
#'
#' @param t_fahrenheit Temperature recorded in Fahrenheit.
#' @return A `numeric` vector.
#' @export
fahrenheit_to_kelvin = function(t_fahrenheit) {
  stopifnot(is.numeric(t_fahrenheit))
  
  (t_fahrenheit + 459.67) * 5 / 9
}

#' Celsius to Kelvin Conversion
#'
#' Converts temperature recorded in Celsius to Kelvin.
#'
#' @param t_celsius Temperature recorded in Celsius.
#' @return A `numeric` vector.
#' @export
kelvin_to_fahrenheit = function(t_kelvin) {
  stopifnot(is.numeric(t_kelvin))
  
  (t_kelvin * 5 / 9) - 459.67
}
