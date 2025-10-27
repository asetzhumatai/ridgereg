#' Visualize mean arrival delays for airports
#'
#' This function creates a scatter plot of mean arrival delays by airport longitude and latitude.
#'
#' @export
visualize_airport_delays <- function() {
  library(dplyr)
  library(ggplot2)
  library(nycflights13)

  flights %>%
    group_by(dest) %>%
    summarise(mean_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop") %>%
    left_join(airports, by = c("dest" = "faa")) %>%
    filter(!is.na(lon) & !is.na(lat)) %>%
    ggplot(aes(x = lon, y = lat, colour = mean_delay)) +
    geom_point(size = 3) +
    scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
    theme_minimal() +
    labs(title = "Mean Arrival Delay by Airport",
         x = "Longitude", y = "Latitude", colour = "Mean Delay (minutes)")
}
