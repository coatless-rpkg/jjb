#' Determine Amount of Missing Values in Data
#' 
#' Computes the proportion of data that is missing in a given data set.
#' 
#' @param x A vector of length \eqn{N} or a matrix with dimensions \eqn{N \times P}{N x P}.
#' 
#' @return 
#' 
#' - Overall: a single numeric value between `[0, 1]` or a count between `[0, N]`.
#' - Variable: \eqn{P} different numeric values between `[0, 1]` or counts between `[0, N]`.
#' - Observation: \eqn{N} different numeric values between `[0, 1]` or counts between `[0, P]`.
#' 
#' @examples 
#' # By vector
#' x = c(1, 2, NA, 4)
#' na_prop_overall(x)
#' na_count_overall(x)
#' 
#' # By Data Frame
#' missing_df = data.frame(
#'  a = c(1, 2, NA, 4),
#'  b = c(3, NA, 2, NA)
#' )
#' 
#' # Proportion
#' na_prop_overall(missing_df)
#' na_prop_by_variable(missing_df)
#' na_prop_by_observation(missing_df)
#' 
#' # Counts
#' na_count_overall(missing_df)
#' na_count_by_variable(missing_df)
#' na_count_by_observation(missing_df)
#' 
#' @export
#' @rdname na_summaries
na_prop_overall = function(x) {
  mean(is.na(x))
}

#' @export
#' @rdname na_summaries
na_prop_by_variable = function(x) {
  colMeans(is.na(x))
}

#' @export
#' @rdname na_summaries
na_prop_by_observation = function(x) {
  rowMeans(is.na(x))
}

#' @export
#' @rdname na_summaries
na_count_overall = function(x) {
  sum(is.na(x))
}

#' @export
#' @rdname na_summaries
na_count_by_variable = function(x) {
  colSums(is.na(x))
}

#' @export
#' @rdname na_summaries
na_count_by_observation = function(x) {
  rowSums(is.na(x))
}
