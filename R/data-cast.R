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
#' sim_df(m)
#'
#' # Organize by column
#' sim_df(m, wide = FALSE)
sim_df = function(m, wide = TRUE){

  o = cast_simdf(m, wide)
    
  class(o) = c("sim_df", "data.frame")
  o
}

cast_simdf = function(m, wide = TRUE){
  if(!is.matrix(m)){stop("`m` must be a `matrix`.")}
  
  n = nrow(m)
  p = ncol(m)
  
  if(wide){
    Round = seq_len(n); Draw = seq_len(p)
  } else{
    Draw = seq_len(n); Round = seq_len(p)
  }
  
  # Hacked from as.data.frame(as.table())
  data.frame(expand.grid(Round = Round, Draw = Draw), Values = c(m))
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
#' study_df(m1, m2, m3)
#'
#' # Organize by column
#' study_df(m1, m2, m3, wide = FALSE, 
#'          data_names = c("Hello", "Goodbye", "Wabbit"))
study_df = function(..., wide = TRUE, data_names = NULL){
  
  obj_list = list(...)
  
  n = length(obj_list)
  
  if(length(data_names) != n) {
    message("No list of names detected... Substituting object names.")
    obj_names = as.character(substitute(...()))
  } else {
    obj_names = data_names
  }

  for(i in seq_along(obj_list)){
    temp = cast_simdf(obj_list[[i]], wide = wide)

    temp$Type = obj_names[i]
    
    obj_list[[i]] = temp
  }
  
  o = Reduce(function(...) merge(..., all=TRUE), obj_list)
  
  class(o) = c("study_df", "data.frame")
  o
}


#' Plot Simulation Trials
#'
#' Constructs a line graph containing different simulations
#' @param x,object An \code{\link{sim_df}} object.
#' @param ...      Not used...
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
#' sim = sim_df(m)
#'
#' # Correct ggplot2 usage
#' autoplot(sim)
#' 
#' # Base R 
#' plot(sim)
plot.sim_df = function(x, ...){
  autoplot.sim_df(object = x, ...)
}

#' @export
#' @rdname plot.simdf
autoplot.sim_df = function(object, ...){
  
  Draw = Values = Round = NULL
  
  ggplot(object, aes(x = Draw, y = Values, group = factor(Round),
                     color = factor(Round))) +
    geom_line(size = 1) +
    theme_bw() + labs(
      x = "Draw",
      y = "Values",
      color = "Round")
}
