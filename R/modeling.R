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

#' Accuracy of the Model
#' 
#' Calculates the accuracy of the model by taking the mean of the number of times
#' the truth, \eqn{y}, equals the predicted, \eqn{\hat{y}}{y hat}.
#' 
#' @param y    A `vector` of the true \eqn{y} values
#' @param yhat A `vector` of predicted \eqn{\hat{y}}{y hat} values. 
#' 
#' @return The accuracy of the classification in `numeric` form.
#' 
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
#' o = acc(y, yhat)
acc = function(y, yhat) {
  mean(y == yhat)
}

#' Mean Squared Error (MSE)
#' 
#' Calculates the mean square of the model by taking the mean of the
#' sum of squares between the truth, \eqn{y}, and the predicted, \eqn{\hat{y}}{y hat}
#' at each observation \eqn{i}.
#' 
#' @inheritParams acc
#' 
#' @return The MSE in `numeric` form.
#' 
#' @details 
#' The equation for MSE is:
#' \deqn{\frac{1}{n}\sum\limits_{i = 1}^n {{{\left( {{y_i} - {{\hat y}_i}} \right)}^2}}}{mean((y-yhat)^2)}
#'  
#' @export
#' @examples 
#' # Set seed for reproducibility
#' set.seed(100)
#' 
#' # Generate data
#' n = 1e2
#' 
#' y = rnorm(n)
#' yhat = rnorm(n, 0.5)
#' 
#' # Compute
#' o = mse(y, yhat)
mse = function(y, yhat) {
  mean((y - yhat) ^ 2)
}

#' Root Mean Squared Error (RMSE)
#' 
#' Calculates the root mean square of the model by taking the square root of 
#' mean of the sum of squares between the truth, \eqn{y}, and the predicted,
#'  \eqn{\hat{y}}{y_hat} at each observation \eqn{i}.
#'  
#' @inheritParams acc
#' 
#' @return The RMSE in `numeric` form
#' 
#' @details 
#' The formula for RMSE is:
#' \deqn{\sqrt {\frac{1}{n}\sum\limits_{i = 1}^n {{{\left( {{y_i} - {{\hat y}_i}} \right)}^2}} } }{sqrt(mean((y-yhat)^2))}
#' 
#' @export
#' @examples 
#' # Set seed for reproducibility
#' set.seed(100)
#' 
#' # Generate data
#' n = 1e2
#' 
#' y = rnorm(n)
#' yhat = rnorm(n, 0.5)
#' 
#' # Compute
#' o = mse(y, yhat)
rmse = function(y, yhat) {
  sqrt(mse(y, yhat))
}



#' Floor and Cap a Numeric Variable
#'
#' Determine the floor and cap of a numeric variable by taking quantiles.
#' Using the quantiles, values in the data found to be _lower_ or _higher_ than
#' the floor or cap are replaced.
#' 
#' @param x      A `vector` that has length \eqn{N}.
#' @param probs  A `vector` containing two values between 0 and 1, with the
#'               first being less than the second.
#'               
#' @return A `vector` with the values floored and capped. 
#' 
#' @export
#' @examples 
#' 
#' # One case version
#' n = 100
#' 
#' x = rnorm(n)
#' 
#' x[n - 1] = -99999
#' x[n] = 10000
#' 
#' y = floor_and_cap(x)
#' 
#' # Dataset example
#' 
#' d = data.frame(x, y = rnorm(n))
#' 
#' o = sapply(d, floor_and_cap)
floor_and_cap = function(x, probs = c(.025, 0.975)) {
  
  if (length(probs) != 2) {
    stop("`probs` must provide two values: one floor and one cap.")
  }
  
  if (probs[1] > probs[2]) {
    stop("`probs` must have the first probability be less than the second probability.")
  }
  
  
  vals = quantile(x, probs = probs, na.rm = TRUE)
  
  x = ifelse(x < vals[1], vals[1], x)
  x = ifelse(x > vals[2], vals[2], x)
  
  x
}

#' Convert Multiple Columns of a `data.frame`
#'  
#' All at once conversion of a `data.frame` from current column types to 
#' alternates.
#' 
#' @param d    A `data.frame` that needs to have specific columns converted.
#' @param cast A `string vector` containing either:
#'             `"n"` (numeric), `"c"` (character), or `"f"` (factor).
#'             
#' @return A `data.frame` with converted column types.
#' 
#' @export
#' @examples 
#' 
#' n = 100
#' 
#' st = sample(LETTERS, n, replace = TRUE)
#' sr = sample(letters, n, replace = TRUE)
#' num = rnorm(n)
#' 
#' d = data.frame(x = st, y = num, z = sr, stringsAsFactors = FALSE)
#' 
#' # Convert all columns
#' 
#' o = convert_cols(d,c("f", "c", "f"))
#' 
#' # Convert a subset
#' d[, c(1, 3)] = convert_cols(d[, c(1, 3)], c("f", "f"))
convert_cols = function(d, cast) {
  
  if (!is.data.frame(d)) {
    stop("`d` must be a `data.frame`")
  }
  
  if (ncol(d) != length(cast)) {
    stop("Number of cast must match number of `d` columns")
  }
  
  d2 = lapply(
    seq_len(ncol(d)),
    FUN = function(i, d, cast) {
      FCALL = switch(cast[i],
                     c = as.character,
                     n = as.numeric,
                     f = as.factor)
      
      FCALL(d[, i])
    },
    d = d,
    cast = cast
  )
  
  # Update naming
  names(d2) = colnames(d)
  
  # Reset as data.frame and cast
  as.data.frame(d2, stringsAsFactors = FALSE)
}

