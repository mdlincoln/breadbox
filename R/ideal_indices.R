#' Given a range anda set of possible breakpoints, return the closest fit to ideal breaks
#'
#' @param r Numeric vector of length 2 giving the desired range
#' @param v Numeric vector of target values
#' @param breaks A function that returns breaks for a given range (see e.g. [scales::extended_breaks])
#'
#' @return An integer vector of indices in v that are the closest available matches for
ideal_indices <- function(r, v, n) {
  stopifnot(is.numeric(r) && length(r) == 2)
  stopifnot(is.numeric(v) & !anyNA(v))

  ideal_breakpoints <- scales::cbreaks(range = r, pretty_breaks(n))
  closest_index <- vapply(ideal_breakpoints$breaks, function(b) {
    diffs <- abs(v - b)
    which.min(diffs)
  }, FUN.VALUE = integer(1L))
}
