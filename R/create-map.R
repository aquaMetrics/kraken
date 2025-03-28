#' @importFrom rlang .data
create_map <- function(data, areas, method) {
  # Convert survey data to spatial
  test <- sf::st_as_sf(data$survey_data,
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )
  # Calculate area without overrides
  ellipse <- areas$ellipse
  polygon <- areas$polygon
  browser()
  if (method == "iqi") {
    my_colors <- data.frame(colour = c(
      "#d7191c",
      "#fdae61",
      "#fecc5c",
      "#abdda4",
      "#2b83ba"
    ), status = c(
      "Bad",
      "Poor",
      "Moderate",
      "Good",
      "High"
    ))
  } else {
    my_colors <- data.frame(colour = c(
      "#d7191c",
      "#abdda4"
    ), status = c(
      "Fail",
      "Pass"
    ))
  }

  my_colours <- dplyr::filter(my_colors, .data$status %in%
    unique(test$`WFD status`))


  test$`WFD status` <- as.factor(test$`WFD status`)
  test$`WFD status` <- forcats::fct_relevel(
    test$`WFD status`,
    my_colours$status
  )
  blue_theme <- ggplot2::theme(
    panel.background = ggplot2::element_rect(
      fill = "#BFD5E3", colour = "#6D9EC1",
      size = 2, linetype = "solid"
    ),
    panel.grid.major = ggplot2::element_line(
      size = 0.5, linetype = "solid",
      colour = "white"
    ),
    panel.grid.minor = ggplot2::element_line(
      size = 0.25, linetype = "solid",
      colour = "white"
    )
  )

  # names(my_colours$colour) <- levels(data$`WFD status`)
  colScale <- ggplot2::scale_colour_manual(
    name = "Status",
    values = my_colours$colour
  )
  g <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = test, ggplot2::aes(color = .data$`WFD status`)) +
    ggplot2::geom_sf(data = ellipse, alpha = 0) +
    # geom_sf(data = polygon, alpha = 0, colour = "purple") +
    # geom_sf(data = points, colour = "black") +
    colScale +
    blue_theme
  return(g)
}
