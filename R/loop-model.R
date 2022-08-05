

loop_model <- function(bootstraps = NULL, numberConverged = 0, xy = 1, distVec = NULL) {
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


plan(multisession, workers = 4)

new_loop_model <- function(bootstraps = NULL, distance_vector = NULL) {
  models <- purrr::map(bootstraps[1:5], function(bootstrap) {
     browser()
    model <- drm(IQI ~ Distance,
      data = bootstrap[1, ],
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

    data <- predict(model, newdata = distance_vector, interval = "none")

    return(data)
  })
}

#
# start_time <- Sys.time()
# result_new <- new_loop_model(bootstraps = bootDRCdata_1,
#                              distance_vector = distVec_1)
# end_time <- Sys.time()
# apply_time <- end_time - start_time
# apply_time
#


