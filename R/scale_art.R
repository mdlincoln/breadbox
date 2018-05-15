scale_art <- function(collections = "nga_highlights", breaks_f = scales::log_breaks()) {
  available_sets <- nga_highlights
  function(x) {
    if (max(x) > max(available_sets$area) + 0.1 * max(available_sets$area))
      warning("Data is well outside the range of the available labels. Plotting may look strange.")

    ideal_breakpoints <- scales::cbreaks(range = range(x), breaks = scales::log_breaks())
    closest_index <- purrr::map_int(ideal_breakpoints$breaks, function(b) {
      diffs <- abs(available_sets$area - b)
      which.min(diffs)
    })
    closest_area <- available_sets$area[closest_index]
    closest_labels <- available_sets$label[closest_index]
    list(
      breaks = closest_area,
      labels = closest_labels,
      rmse = sqrt(mean((closest_area - ideal_breakpoints$breaks)^2))
    )
  }
}
