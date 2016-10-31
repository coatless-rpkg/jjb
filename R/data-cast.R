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

#' Cast Simulation Matrix to Data Frame
#'
#' Creates a \code{data.frame} out of a simulation matrix
#' for use with \code{ggplot2}.
#' @param m     A \code{matrix}
#' @param wide  A \code{boolean} indicating if the simulated data is group
#' by row (\code{TRUE}) or column (\code{FALSE}).
#' @return A \code{data.frame} with three variables:
#' \itemize{
#' \item{Round}{Iteration of the Simulation}
#' \item{Draw}{Draw during the iteration of the simulation}
#' \item{Value}{Value of the statistic at round and draw}
#' }
#' @export
#' @author James Balamuta
#' @examples
#' # Set Seed
#' set.seed(5812)
#'
#' # Generate data
#' m = matrix(rnorm(10), 2,5)
#'
#' # Organize data.frame by row
#' simdf(m)
#'
#' # Organize by column
#' simdf(m, wide = FALSE)
sim_df = function(m, wide = TRUE){
  if(!is.matrix(m)){stop("`m` must be a `matrix`.")}
  
  n = nrow(m)
  p = ncol(m)
  
  if(wide){
    Round = seq_len(n); Draw = seq_len(p)
  } else{
    Draw = seq_len(n); Round = seq_len(p)
  }
  
  # Hacked from as.data.frame(as.table())
  o = data.frame(expand.grid(Round = Round, Draw = Draw), Values = c(m))
  
  class(o) = c("sim_df", "data.frame")
  o
}


#' Cast Simulation Study Matrices to Data.Frame
#'
#' Creates a \code{data.frame} out of multiple simulation matrices
#' for use with \code{ggplot2}.
#' @param ...        A list of \code{matrices}
#' @param wide       A \code{boolean} indicating if the simulated data is group
#'                   by row (\code{TRUE}) or column (\code{FALSE}).
#' @param data_names A \code{character vector} containing name of matrix. If
#'                   empty it uses the simulation matrix name
#' @return A \code{data.frame} with three variables:
#' \itemize{
#' \item{Round}{Iteration of the Simulation}
#' \item{Draw}{Draw during the iteration of the simulation}
#' \item{Value}{Value of the statistic at round and draw}
#' \item{Type}{Study Matrix}
#' }
#' @author James Balamuta
#' @export
#' @examples
#' # Set Seed
#' set.seed(5812)
#'
#' # Generate data
#' m1 = matrix(rnorm(10), 2, 5)
#' m2 = matrix(rnorm(10), 2, 5)
#' m3 = matrix(rnorm(10), 2, 5)
#' 
#' # Organize data.frame by row
#' study_df(m1,m2,m3)
#'
#' # Organize by column
#' study_df(m1,m2,m3, wide = FALSE, data_names = c("Hello", "Goodbye", "Wabbit"))
study_df = function(..., wide = TRUE, data_names = NULL){
  
  obj_list = list(...)
  obj_names = as.character(substitute(...()))
  
  n = length(obj_list)
  
  if(length(data_names) != n) {
    message("No list of names detected... Substituting object names.")
    print(obj_names)
    obj_names = obj_names
  } else {
    obj_names = data_names
  }

  for(i in seq_along(obj_list)){
    temp = cast_simdf(obj_list[[i]], wide = wide)

    temp$Type = obj_names
    
    obj_list[[i]] = temp
  }
  
  o = Reduce(function(...) merge(..., all=TRUE), obj_list)
  
  class(o) = c("study_df", "data.frame")
  o
}


#' Plot Simulation Trials
#'
#' Constructs a line graph containing different simulations
#' @param x,object An \code{\link{simdf}} object.
#' @export
#' @rdname plot.simdf
#' @examples
#' # Set Seed
#' set.seed(5812)
#'
#' # Generate data
#' m = matrix(rnorm(10), 2,5)
#'
#' # Organize data.frame by row
#' sim = cast_simdf(m)
#'
#' # Graph Sim
#' plot(sim)
plot.simdf = function(x, ...){
  autoplot.simdf(object = x, ...)
}


#' @export
#' @rdname plot.simdf
autoplot.simdf = function(object, type = "line", ...){
  ggplot(object, aes(x = Draw, y = Values, group = factor(Round),
                     color = factor(Round))) +
    geom_line(size = 1) +
    theme_bw() + labs(
      x = "Draw",
      y = "Values",
      color = "Round")
}
