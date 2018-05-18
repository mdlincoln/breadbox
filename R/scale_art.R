scale_art <- function(collections = "nga_highlights", breaks_f = scales::log_breaks()) {
  available_sets <- nga_highlights
  function(x) {
    if (max(x) > max(available_sets$area) + 0.1 * max(available_sets$area))
      warning("Data is well outside the range of the available labels. Plotting may look strange.")
    closest_index <- ideal_indices(range(x), available_sets[["area"]])

    list(
      breaks = closest_area,
      labels = closest_labels,
      rmse = sqrt(mean((closest_area - ideal_breakpoints$breaks)^2))
    )
  }
}

to_art <- function(x) {
  closest_index <- ideal_indices(range(x), available_sets[["area"]])
  attr(x, "breadbox_ci") <- closest_index
  attr(x, "breadbox_src") <- "nga_highlights"
  class(x) <- "breadbox"
  return(x)
}

from_art <- function(x) as.numeric(x) {
  as.numeric(x)
}

art_trans <- function() {
  trans_new("art")
}

# Return a function that produces breaks
art_breaks <- function(n = 3) {
  function(x) {
    stopifnot(inherits(x, "breadbox"))
    df <- get(attr(x, "breadbox_src"), envir = as.environment("package:breadbox"))
    i <- attr(x, "breadbox_ci")
    closest_area <- df[["area"]][i]
  }
}

f_art_labels <- function(n = 3, i = NULL) {
  function(x) {
    stopifnot(inherits(x, "breadbox"))
    if (is.null(i))
      i <- ideal_indices(range(x), nga_highlights[["area"]], n, ...)
    closest_labels <- available_sets[["label"]][i]
  }
}

# ggplot2 scale functions ----

# This tells ggplot2 what scale to look for, for yearmon
scale_type.art <- function(x) "art"

scale_art <- function(aesthetics, ...) {
  ggplot2::continuous_scale(aesthetics, "art", identity,
                   guide = "none", trans = art_trans(), ...)
}

scale_x_art <- function(...) {
  scale_art(aesthetics = c("x", "xmin", "xmax", "xend"), ...)
}

scale_y_art <- function(...) {
  scale_art(aesthetics = c("y", "ymin", "ymax", "yend"), ...)
}