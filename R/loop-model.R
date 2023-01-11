
loop_model <- function(bootstraps = NULL,
                       numberConverged = 0,
                       xy = 1,
                       distVec = NULL,
                       innerTransect = NULL,
                       bestModel = NULL) {
  convergedCount <- rep(0, length(bootstraps))
  nonConvergedCount <- rep(0, length(bootstraps))
  ypred_mLBoot <- vector(mode = "list", length(bootstraps))
  distVec <- data.frame(
    Distance = seq(
      from = 0,
      to = max(as.integer(innerTransect$Distance)),
      by = 1
    )
  )


  while ((numberConverged < 500) & (xy <= length(bootstraps))) {
    mLBoot <- NULL
    try(mLBoot <- suppressMessages(suppressWarnings(drm(IQI ~ Distance,
      data = bootstraps[[xy]],
      fct = bestModel,
      type = "continuous",
      control = drmc(
        noMessage = TRUE,
        warnVal = -1,
        trace = FALSE,
        otrace = FALSE
      )
    ))), silent = TRUE)
    if (is.null(mLBoot) == FALSE) {
      convergedCount[xy] <- 1
      ypred_mLBoot[[xy]] <- data.frame(

          "Distance" = distVec,
          "IQI" = suppressMessages(suppressWarnings(
            predict(mLBoot,
              newdata = distVec,
              interval = "none"
            )
          ))

      )
    } else {
      nonConvergedCount[xy] <- 1
    }
    numberConverged <- sum(convergedCount)
    xy <- xy + 1
  }

  return(list(numberConverged, xy, ypred_mLBoot))
}


# start_time <- Sys.time()
# result_old <- loop_model(bootstraps = bootDRCdata_1, distVec = distVec_1)
# end_time <- Sys.time()
# end_time - start_time


# plan(multisession, workers = 4)

new_loop_model <- function(bootstraps = NULL,
                           distance_vector = NULL,
                           bestModel = NULL) {
  models <- purrr::map_df(bootstraps, function(bootstrap) {
     # browser()
     model <- drm(IQI ~ Distance,
      data = bootstrap,
      fct = bestModel,
      type = "continuous",
      control = drmc(
        errorm = FALSE,
        noMessage = TRUE,
        warnVal = -1,
        trace = FALSE,
        otrace = FALSE
      )
    )

    # model <- loess(IQI ~ Distance,
    #                data = data$survey_data
    #
    # )

    data <- predict(model, newdata = distance_vector, interval = "none")
    data <- data.frame("iqi" = data)
    return(data)
  })
}


# model <- drm(IQI ~ Distance,
#              data = data$survey_data,
#              fct = bestModel,
#              type = "continuous",
#              control = drmc(
#                errorm = FALSE,
#                noMessage = TRUE,
#                warnVal = -1,
#                trace = FALSE,
#                otrace = FALSE
#              )
# )

#  survey_data <- data$survey_data
#  distance <- data.frame("Distance" = 1:293)
# prediction <- predict(model, distance)
#
#
# data <- data.frame("IQI" = c(0.4,0.59, 0.61, 0.56, 0.56, 0.53, 0.67, 0.68, 0.67),
#                    "Distance" = c(0,27.03,51.76, 60.85, 71.73, 127.48, 208.56, 278.09, 293.71))
# model <- drc::drm(IQI ~ Distance,
#              data = data,  fct = MM.3(),  type = "continuous",
#              control = drmc(
#                               errorm = FALSE,
#                               noMessage = TRUE,
#                               warnVal = -1,
#                               trace = FALSE,
#                               otrace = FALSE
#                             )
#                )
# distance <- 1:294
# prediction_loess <- predict(model, data.frame(distance))
#
# start_time <- Sys.time()
# result_new <- new_loop_model(bootstraps = bootDRCdata_1,
#                              distance_vector = distVec_1)
# end_time <- Sys.time()
# apply_time <- end_time - start_time
# apply_time
#


