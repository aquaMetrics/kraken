create_plot <- function(data, method, pass_fail) {
  # Pivot output ------------------------------------------
  plots_data <- data %>%
    dplyr::filter(question %in% c("Distance", "IQI", "MCFF_Transect"))
  plots_data <- distinct(plots_data)
  distance_to_good <- data$object[data$question == "Distance to Good (m)"][[1]]
  if (method == "iqi") {
    y_lab <- "IQI value"
    plots_data <- pivot_wider(
      plots_data,
      names_from = question,
      values_from = response
    )
    plots_data$IQI <- as.numeric(plots_data$IQI)
    plots_data$Distance <- as.numeric(plots_data$Distance)
  } else {
    y_lab <- "Emmamectin PPM"
    # Residue sampling pivot ------------------------
    # There can be duplicates per station so needs more complicate pivot
    plots_data <- purrr::map_df(
      split(plots_data, plots_data$sample_id),
      function(sample) {
        distance <- filter(sample, question == "Distance")
        MCFF <- filter(sample, question == "MCFF_Transect")
        if (length(sample$response[sample$question == "IQI"]) == 3) {
          sample <- bind_rows(sample, distance, distance, MCFF, MCFF)
          sample <- arrange(sample, question)
          sample$ID <- rep(1:3, 3)
        }
        if (length(sample$response[sample$question == "IQI"]) == 2) {
          sample <- bind_rows(sample, distance, MCFF)
          sample <- arrange(sample, question)
          sample$ID <- rep(1:2, 3)
        }

        sample <- pivot_wider(
          sample,
          names_from = question,
          values_from = response
        )
        sample$IQI <- as.numeric(sample$IQI)
        sample$Distance <- as.numeric(sample$Distance)

        return(sample)
      }
    )
  }
  # Hexagon heatmap --------------
  # representation of all the fitted model outputs
  hex_df <- bind_rows(data$object[data$question == "hex_df"])
  hex_df$Distance <- as.numeric(hex_df$Distance)
  hex_df$IQI <- as.numeric(hex_df$IQI)
  hex_df$Transect <- as.numeric(hex_df$Transect)
  hex_df$Counts <- as.numeric(hex_df$Counts)
  hex_df$shape <- NA
  hex_df$size <- NA
  hex_df$colour <- NA
  hex_df$alpha <- NA
  hex_df$shape[hex_df$Source == "Prob. model"] <- "hexagon"
  hex_df$shape[hex_df$Source == "Survey data"] <- "circle"
  hex_df$size[hex_df$Source == "Prob. model"] <- 3
  hex_df$size[hex_df$Source == "Survey data"] <- 2
  hex_df$alpha[hex_df$Source == "Prob. model"] <- 1
  hex_df$alpha[hex_df$Source == "Survey data"] <- 0.7

  # Plot ----------
  p <- list()
  model <- data$object[data$question == "model_info"][[1]]
  for (i in 1:nrow(model)) {
    model_plot <- model[model$Transect == i, ]
    plot_data <- plots_data[
      grepl(paste0("- ", i), plots_data$MCFF_Transect),
    ]
    model_type <- paste0(trimws(substr(model_plot$bestModel, 1, 4)), "()")
    message(model_type)
    transect_hex <- hex_df[hex_df$Transect == i, ]
    breach <- distance_to_good[
      distance_to_good$Transect == i,
      "95 percentile distance to Good (m)"
    ]
    breach <- as.numeric(breach)
    #  breach <- breach_ensemble[breach_ensemble$Transect == i, ]
    if (!model_type %in% c("Insu()", "Regr()")) {
      p[[i]] <- ggplot2::ggplot(
        ggplot2::aes(
          Distance,
          IQI,
          colour = Counts,
          fill = Counts
        ),
        data = transect_hex
      ) +
        ggstar::geom_star(
          starshape = transect_hex$shape,
          size = transect_hex$size,
          alpha = transect_hex$alpha
        ) +
        ggplot2::ylab(y_lab) +
        ggplot2::xlab("Distance (m)") +
        ggplot2::ggtitle(
          paste0("Site: ", unique(plot_data$project_id)),
          subtitle = unique(plot_data$MCFF_Transect)
        ) +
        ggplot2::geom_hline(
          yintercept = pass_fail,
          colour = "green",
          linewidth = 1,
          linetype = 2
        ) +
        ggplot2::geom_vline(
          xintercept = breach,
          colour = "black",
          linetype = 2,
          linewidth = 1
        ) +
        ggplot2::scale_colour_continuous(
          palette = c("#FEE0D2", "#FC9272", "#DE2D26"),
          na.value = "black"
        ) +
        ggplot2::scale_fill_continuous(
          palette = c("#FEE0D2", "#FC9272", "#DE2D26"),
          na.value = "black"
        )
    } else {
      model_plot <- model[model$Transect == i, ]
      plot_data <- plots_data[
        grepl(paste0("- ", i), plots_data$MCFF_Transect),
      ]
      model_type <- paste0(trimws(substr(model_plot$bestModel, 1, 4)), "()")
      message(model_type)
      transect_hex <- hex_df[hex_df$Transect == i, ]
      p[[i]] <- ggplot2::ggplot(ggplot2::aes(Distance, IQI), data = plot_data) +
        ggplot2::geom_point() +
        ggplot2::ylab(y_lab) +
        ggplot2::xlab("Distance (m)") +
        ggplot2::ggtitle(
          paste0("Site: ", unique(plot_data$project_id)),
          subtitle = paste0(
            unique(plot_data$MCFF_Transect),
            " Model fit not of sufficient quality to use"
          )
        ) +
        ggplot2::geom_hline(
          yintercept = pass_fail,
          colour = "green",
          linetype = 2
        ) +
        ggplot2::geom_vline(
          xintercept = breach,
          colour = "black",
          linetype = 2,
          linewidth = 1
        )
    }
  }
  return(p)
}
