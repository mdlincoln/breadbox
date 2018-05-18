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

}

from_art <- function(x) as.numeric(x)

art_trans <- function() {
  trans_new("art")
}

art_breaks <- function(n = 3) {
  function(x) {
    closest_index <- ideal_indices(range(x), available_sets[["area"]], n, ...)
    closest_area <- available_sets[["area"]][i]
  }
}

f_art_labels <- function(i) {
  closest_labels <- available_sets[["label"]][i]
}