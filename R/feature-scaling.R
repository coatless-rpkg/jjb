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

#' Feature Scaling
#'
#' Scale features in a datasets.
#' 
#' @param x      Numeric values
#' @param x_min  Minimum non-normalized numeric value
#' @param x_max  Maximum non-normalized numeric value
#' 
#' @return 
#' A `numeric` vector.
#' 
#' @author 
#' James Balamuta
#' 
#' @details
#' The following functions provide a means to either scale features or 
#' to descale the features and return them to normal. These functions
#' are ideal for working with optimizers.
#' 
#' \tabular{ll}{
#' Feature Scale       \tab Feature Descale      \cr
#' feature_rescale     \tab feature_derescale    \cr
#' feature_norm        \tab feature_denorm       \cr
#' feature_standardize \tab feature_destandardize
#' }
#' 
#' @section Feature Rescaling:
#' 
#' Convert the original data \eqn{x} to \eqn{x_{scaled}}:
#' 
#' \deqn{x[scaled] = (x-x[min])/(x[max]-x[min])}{x_{scaled} = \frac{(x-x_{min})}{(x_{max}-x_{min})}}
#'
#' To move from the rescaled value \eqn{x_{scaled}} to the original value \eqn{x} use:
#' 
#' \deqn{x = x[scaled] * (x[max] - x[min]) + x[min]}{x = x_{scaled} * (x_{max} - x_{min}) + x_{min}}
#' 
#' @section Feature Standardization:
#' 
#' Convert the original data \eqn{x} to \eqn{x_{std}}:
#' 
#' \deqn{x[std] = (x-avg[x])/(sigma[x])}{x_{std} = \frac{(x-\bar{x})}{\sigma_{x}}}
#'
#' To move from the standardized value \eqn{x_{std}} to the original value \eqn{x} use:
#' 
#' \deqn{x = x[std] * sigma[x] + avg[x]}{x = x_{std} \sigma_{x} + \bar{x}}
#' 
#' @section Feature Normalization:
#' 
#' Convert the original data \eqn{x} to \eqn{x_{norm}}:
#' 
#' \deqn{x[norm] = (x)/||x||}{x_{norm} = \frac{x}{\left\| x \right\|}}
#'
#' To move from the normalized value \eqn{x_{norm}} to the original value \eqn{x} use:
#' 
#' \deqn{x = x[norm] * ||x||}{x = x_{norm} \left\| x \right\|}
#' 
#' @name feature_scaling
#' @aliases feature_derescale
#' @aliases feature_rescale
#' @rdname feature_scaling
#' @export
#' @examples 
#' 
#' # Rescaling Features
#' temperatures = c(94.2, 88.1, 32, 0)
#'
#' temp_min = min(temperatures)
#' temp_max = max(temperatures)
#' 
#' temperatures_norm   = feature_rescale(temp_min, temp_max)
#' temperatures_denorm = feature_derescale(temperatures_norm, temp_min, temp_max)
#' 
#' all.equal(temperatures, temperatures_denorm)
feature_rescale = function(x, x_min = NULL, x_max = NULL) {

  stopifnot( all(is.numeric(x)) )
  
  if( is.null(x_min) ) { x_min = min(x) }
  if( is.null(x_max) ) { x_max = max(x) }
  
  stopifnot( is.numeric(x_min) & is.numeric(x_max) )
             
  (x - x_min)/(x_max - x_min)
}

#' @param x_rescaled Rescaled values of `x`.
#' 
#' @rdname feature_scaling
#' @export
feature_derescale = function(x_rescaled, x_min, x_max){
  stopifnot( all(is.numeric(x_rescaled)) & is.numeric(x_min) & is.numeric(x_max) )
  x_rescaled * (x_max - x_min) + x_min
}

#' @param x_norm Euclidean norm of x
#' 
#' @rdname feature_scaling
#' @export
feature_norm = function(x, x_norm = NULL) {
  
  stopifnot( all(is.numeric(x)) )
  
  if( is.null(x_norm) ) { x_norm = sqrt(sum(x^2)) }
  
  stopifnot( is.numeric(x_norm) )
  
  x/x_norm
}

#' @param x_norm_std Euclidean vector of normalized `x` values.
#' 
#' @rdname feature_scaling
#' @export
#' @examples
#'  
#' # Norming Features
#' x = 1:10
#' 
#' x_norm = sqrt(sum(x^2))
#' 
#' x_norm_std = feature_norm(x, x_norm)
#' 
#' x_recover = feature_denorm(x_norm_std, x_norm)
#' all.equal(x, x_recover)
feature_denorm = function(x_norm_std, x_norm = NULL) {
  
  stopifnot( all(is.numeric(x_norm_std)) )
  
  stopifnot( is.numeric(x_norm) )
  
  x_norm_std*x_norm
}

#' @param x_mean Mean of `x` values
#' @param x_sd   Standard Deviation of `x` values
#' 
#' @rdname feature_scaling
#' @export
#' @importFrom stats sd
feature_standardize = function(x, x_mean = NULL, x_sd = NULL) {
  
  stopifnot( all(is.numeric(x)) )
  if( is.null(x_mean) ) { x_mean = mean(x) }
  if( is.null(x_sd) ) { x_sd = sd(x) }
  
  stopifnot( is.numeric(x_mean) & is.numeric(x_sd) & x_sd > 0 )
  
  (x - x_mean) / x_sd
}

#' @param x_std   Z-transformed `x` values
#' 
#' @rdname feature_scaling
#' @export
#' @examples 
#' 
#' # Standardizing Features
#' x      = 1:10
#' 
#' x_mean = mean(x)
#' x_sd   = sd(x)
#' 
#' x_std  = feature_standardize(x, x_mean, x_sd)
#' x_recovery = feature_destandardize(x, x_mean, x_sd)
#' 
#' all.equal(x, x_recovery)
feature_destandardize = function(x_std, x_mean = NULL, x_sd = NULL) {
  
  stopifnot( all(is.numeric(x_std)) )
  
  stopifnot( is.numeric(x_mean) & is.numeric(x_sd) & x_sd > 0 )
  
  x_std * x_sd + x_mean
}
