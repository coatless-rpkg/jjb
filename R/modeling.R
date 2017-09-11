# Copyright (C) 2015  James Balamuta
#
# This file is part of `balamuta` R Package
#
# The `balamuta` R package is free software: you can redistribute it and/or modify it
# under the terms of the MIT LICENSE included within the packages source as the LICENSE file.
#
# The `balamuta` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the MIT License along with `balamuta`. 
# If not, see <https://opensource.org/licenses/MIT>.

#' Remove Feature Scaling of Values
#'
#' Undo the normalization of values done with
#' feature scaling.
#' @param x_norm Normalized Values
#' @param x_min Minimum non-normalized value
#' @param x_max Maximum non-normalized value
#' @return A `numeric` vector.
#' @details
#' Converts from the feature scaled or normalized value given by:
#' 
#' \deqn{x[norm] = (x-x[min])/(x[max]-x[min])}{x_{norm} = \frac{(x-x_{min})}{(x_{max}-x_{min})}}
#'
#' To the original or denormalized value given by:
#' 
#' \deqn{x = x[norm] * (x[max] - x[min]) + x[min]}{x = x_{norm} * (x_{max} - x_{min}) + x_{min}}
#' 
#' @examples 
#' temperatures = c(94.2, 88.1, 32, 0)
#'
#' temperatures_norm   = feature_scale(temperatures, min(temperatures), max(temperatures))
#' temperatures_denorm = feature_descale(temperatures_norm, min(temperatures), max(temperatures))
#' 
#' all.equal(temperatures_denorm, temperatures)
feature_descale = function(x_norm, x_min, x_max){
  stopifnot( all(is.numeric(x_norm)) & is.numeric(x_min) & is.numeric(x_max) )
  x_norm * (x_max - x_min) + x_min
}

#' Feature Scale Values
#'
#' Apply a feature scaling normalization procedure on Values
#' @param x     Values
#' @param x_min Minimum non-normalized Values
#' @param x_max Maximum non-normalized Values
#' @return A `numeric` vector.
#' @details
#' Feature scale equation to normalize values:
#' 
#' \deqn{x[norm] = (x-x[min])/(x[max]-x[min])}{x_{norm} = \frac{(x-x_{min})}{(x_{max}-x_{min})}}
#' 
#' @examples 
#' temperatures = c(94.2, 88.1, 32, 0)
#'
#' temperatures_norm = feature_scale(temperatures, min(temperatures), max(temperatures))
feature_scale <- function(x, x_min, x_max){
  stopifnot( all(is.numeric(x)) & is.numeric(x_min) & is.numeric(x_max) )
  (x - x_min)/(x_max - x_min)
}

#' Accuracy of the model
#' 
#' Calculates the accuracy of the model by taking the mean of the number of times
#' the truth, \eqn{y}, equals the predicted, \eqn{\hat{y}}{y hat}.
#' 
#' @param y    A \code{vector} of the true \eqn{y} values
#' @param yhat A \code{vector} of predicted \eqn{\hat{y}}{y hat} values. 
#' @return A \code{double} indicating the accuracy of the classification
#' @export
#' @examples 
#' # Set seed for reproducibility
#' set.seed(100)
#' 
#' # Generate data
#' n = 1e2
#' 
#' y = round(runif(n))
#' yhat = round(runif(n))
#' 
#' # Compute
#' o = acc(y,yhat)
acc = function(y, yhat) {
  mean(y == yhat)
}

#' Mean Squared Error (MSE)
#' 
#' Calculates the mean square of the model by taking the mean of the
#' sum of squares between the truth, \eqn{y}, and the predicted, \eqn{\hat{y}}{y hat} at each observation \eqn{i}.
#' @inheritParams acc
#' @details 
#' The equation for MSE is:
#' \deqn{\frac{1}{n}\sum\limits_{i = 1}^n {{{\left( {{y_i} - {{\hat y}_i}} \right)}^2}}}{mean((y-yhat)^2)}
#' @return A \code{double} indicating the MSE
#' @export
#' @examples 
#' # Set seed for reproducibility
#' set.seed(100)
#' 
#' # Generate data
#' n = 1e2
#' 
#' y = rnorm(n)
#' yhat = rnorm(n,0.5)
#' 
#' # Compute
#' o = mse(y,yhat)
mse = function(y, yhat){
  mean( (y - yhat) ^ 2)
}

#' Root Mean Squared Error (RMSE)
#' 
#' Calculates the root mean square of the model by taking the square root of mean of the
#' sum of squares between the truth, \eqn{y}, and the predicted, \eqn{\hat{y}}{y hat} at each observation \eqn{i}.
#' @inheritParams acc
#' @details 
#' The formula for RMSE is:
#' \deqn{\sqrt {\frac{1}{n}\sum\limits_{i = 1}^n {{{\left( {{y_i} - {{\hat y}_i}} \right)}^2}} } }{sqrt(mean((y-yhat)^2))}
#' @return A \code{double} indicating the RMSE
#' @export
#' @examples 
#' # Set seed for reproducibility
#' set.seed(100)
#' 
#' # Generate data
#' n = 1e2
#' 
#' y = rnorm(n)
#' yhat = rnorm(n,0.5)
#' 
#' # Compute
#' o = mse(y,yhat)
rmse = function(y, yhat) {
  sqrt(mse(y,yhat))
}



#' @title Floor and Cap a dataset
#' @description Floors and Caps a numeric variable.
#' @param x      A \code{vector} that has length \eqn{N}.
#' @param probs  A \code{vector} containing two values between 0 and 1, with the first being less than the second.
#' @return A \code{vector} with the values floored and capped. 
#' @author James J Balamuta
#' @export
#' @examples 
#' 
#' # One case version
#' n = 100
#' 
#' x = rnorm(n)
#' 
#' x[n-1] = -99999
#' x[n] = 10000
#' 
#' y = floor_and_cap(x)
#' 
#' # Dataset example
#' 
#' d = data.frame(x,y=rnorm(n))
#' 
#' o = sapply(d,floor_and_cap)
floor_and_cap = function(x, probs=c(.025,0.975)){
  
  if(length(probs) != 2){stop("Must provide two values for `probs`: one floor and one cap.")}
  if(probs[1] > probs[2]){stop("The first element in `probs` must be less than the second element.")}
  
  
  vals = quantile(x, probs=probs, na.rm=TRUE)
  
  x = ifelse(x<vals[1],vals[1],x)
  x = ifelse(x>vals[2],vals[2],x)
  
  x
}



#' Convert Multiple Columns of a data.frame
#'  
#' All at once conversion of a data.frame from current column types to alternates.
#' 
#' @param d    A \code{data.frame} that needs to have specific columns converted.
#' @param cast A \code{string vector} containing either \code{"n"} (numeric), \code{"c"} (character), or \code{"f"} (factor).
#' @return A \code{data.frame} with converted column types.
#' @author James J Balamuta
#' @export
#' @examples 
#' 
#' n = 100
#' 
#' st = sample(LETTERS, n, replace = TRUE)
#' sr = sample(letters, n, replace = TRUE)
#' num = rnorm(n)
#' 
#' d = data.frame(x=st, y=num, z = sr, stringsAsFactors = FALSE)
#' 
#' # Convert all columns
#' 
#' o = convert_cols(d,c("f","c","f"))
#' 
#' # Convert a subset
#' d[,c(1,3)] = convert_cols(d[,c(1,3)],c("f","f"))
convert_cols = function(d,cast){
  
  if(!is.data.frame(d)){stop("`d` must be a `data.frame`")}
  if(ncol(d)!=length(cast)){stop("Number of cast must match number of `d` columns ")}
  
  d2 = lapply(1:ncol(d),
              FUN = function(i){
                FCALL = switch(cast[i],
                               c = as.character,
                               n = as.numeric,
                               f = as.factor);
                FCALL(d[,i])
              })
  
  names(d2) = colnames(d)
  
  # Reset as data.frame and cast
  as.data.frame(d2, stringsAsFactors = FALSE)
}

