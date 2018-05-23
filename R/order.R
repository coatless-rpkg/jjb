# Copyright (C) 2015 - 2018  James Balamuta
#
# This file is part of `bmisc` R Package
#
# The `bmisc` R package is free software: you can redistribute it and/or modify
# it under the terms of the GPL-3 LICENSE included within the packages source 
# as the LICENSE file.
#
# The `bmisc` R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the GPL-3 License along with `bmisc`.
# If not, see <https://opensource.org/licenses/GPL-3.0>.

retrieve_sorted_elements = function(x, n = 1, find_max = TRUE) {
  
  # Count missing values
  missing_count = sum(is.na(x))
  
  # Remove this amount from the total length
  n_elem = length(x) - missing_count
  
  # Determine if we have enough elements to proceed.
  if(n > n_elem || n <= 0L) {
    stop("`n` must be less than the total number of elements in the vector",
         " but greater than 0.", call. = FALSE)
  }
  
  sort(x)[if(find_max) seq(n_elem, n_elem - n + 1) else seq_len(n)]
}

#' Maxima and Minima _n_ elements
#' 
#' Obtain the Maximum or Minimum _n_ elements from a vector.
#' 
#' @param x       Data vector
#' @param n       Number of observations to select
#' 
#' @return A `vector` containing the maximum/minimum of \eqn{n} elements.
#' 
#' @details 
#' The underlying function sorts the data using [base::sort()] and then extracts
#' out the appropriate n-back or n-forward values.
#' 
#' As a result of the sorting procedure, this is an inefficient function.
#' 
#' @rdname max_min_n
#' @export
#' @examples 
#' 
#' x = 1:10
#' 
#' # Defaults to traditional max
#' # This is more costly to compute than using the regular max function.
#' max_n(x) 
#' 
#' # Retrieve top two observations (highest first)
#' max_n(x, 2)
#' 
#' # Missing values have no effect on the sorting procedure
#' x[9] = NA
#' max_n(x, 3)
#' 
max_n = function(x, n = 1L) {
  retrieve_sorted_elements(x, n = n, find_max = TRUE)
}

#' @rdname max_min_n
#' @export
#' @examples
#' # Defaults to traditional min.
#' # This is more costly to compute than using the regular min function.
#' min_n(x)
#' min(x)
#' 
#' # Retrieve bottom two observations (lowest first)
#' min_n(x, 2)
#' 
#' # Missing values have no effect on the sorting procedure
#' x[2] = NA
#' min_n(x, 3)
min_n = function(x, n = 1) { 
  retrieve_sorted_elements(x, n = n, find_max = FALSE)
}