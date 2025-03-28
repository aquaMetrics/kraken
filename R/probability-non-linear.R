#' Probability Non-linear
#'
#' Fit the best model calculating the distance to good status. Where distance
#' cannot be modeled, the distance to the second consecutive good status (or
#' high status) station is used.
#'
#' @param data The named data frame `survey_data` from `consecutive_station()` function. See examples.
#' @param loess Use loess model (instead of best fit model).
#' @param good_moderate The EQR ratio for Good - Moderate boundary.
#' @param method Type of method used to analyse samples, either "iqi" or
#'   "residue".
#' @return list containing four named data frames: data, geoDf, geoDfBestFit and
#'   hexdfOut.
#' @export
#' @importFrom stats AIC predict
#' @importFrom rlang .data
#' @importFrom dplyr mutate group_by ungroup n select
#' @importFrom drc drm drmc L.3 L.4 L.5 LL.2 LL.3 LL.3u LL.4 LL.5 W1.2 W1.3 W1.4 W2.2 W2.3 W2.4 BC.4 BC.5 LL2.2 LL2.3 LL2.3u LL2.4 LL2.5 AR.2 AR.3 MM.2 MM.3
#' @importFrom hexbin hexbin hcell2xy
#' @examples
#' \dontrun{
#' data <- consecutive_stations(data)
#' probability <- probability_non_linear(data$survey_data)
#' }
probability_non_linear <- function(data,
                                   loess = FALSE,
                                   good_moderate = 0.64,
                                   method = "iqi") {
  # This version incorporates the following changes:
  #   1 Removing L.3 as a possible model fit
  #   2 Improved 2 station rule method
  options(stringsAsFactors = FALSE)
  set.seed(123)

  # Create variable for MCFF-Transect
  combs <- unique(data$MCFF_Transect)

  # Set some acceptance criteria for the regression model
  convergenceCriterion <- 50
  PercDontReachGoodCriterion <- 0

  # Initialise outputs
  D2Gdistr <- data.frame(cbind(
    MCFF = NA,
    MCFF_Transect = NA,
    Transect = NA,
    Bearing = NA,
    Easting = NA,
    Northing = NA,
    D2G = NA,
    D2Ghist = NA,
    D2Gtype = NA
  ))

  D2GbestFitResults <- data.frame(cbind(
    MCFF = NA,
    MCFF_Transect = NA,
    Transect = NA,
    Bearing = NA,
    Easting = NA,
    Northing = NA,
    D2G = NA
  ))

  summaryOutput <- data.frame(cbind(
    MCFF = NA,
    MCFF_Transect = NA,
    Transect = NA,
    bestModel = NA,
    ICresult = NA,
    convergedPercent = NA,
    dontAchieveGoodPercent = NA,
    stationNumber = NA,
    twoConsecutiveStations = NA,
    reducedSamplingD2G = NA
  ))

  hexdfOut <- data.frame(cbind(
    MCFF = NA,
    Transect = NA,
    Distance = NA,
    IQI = NA,
    ID = NA,
    Counts = NA,
    Source = NA
  ))

  for (i in combs) {
    message(i)

    innerTransect <- data[data$MCFF_Transect == i, ]
    reducedSamplingD2G <- NA

    # 1 Initial transect checks ------------------------------------------------
    innerTransect <- innerTransect[order(innerTransect$Distance), ]

    # Check if 7 stations taken
    numberOfStations <- length(innerTransect$IQI)
    if (numberOfStations < 7) {
      stationNumber <-
        paste0(
          "Non-compliant: Min. number of stations not taken (",
          numberOfStations, ")"
        )
    } else {
      stationNumber <-
        paste0(
          "Compliant: Min. number of stations have been taken (",
          numberOfStations, ")"
        )
    }
    # Find distance to Good based on 2 consecutive station rule
    if (method == "iqi") {
      r <- rle(innerTransect$IQI >= good_moderate)
    } else {
      r <- rle(innerTransect$IQI < good_moderate)
    }
    s <- NULL
    for (j in 1:length(r$values)) {
      s_j <- (rep(r$values[j], r$lengths[j]))
      s <- c(s, s_j)
    }
    s <- as.numeric(s)
    summed <- NULL
    for (j in 1:length(s)) {
      summed[j] <- s[j] + s[j + 1]
    }

    row_index <- which(summed == 2, arr.ind = TRUE)[1]
    if (is.na(row_index) == FALSE) {
      reducedSamplingD2G <- innerTransect$Distance[row_index]
    }

    # Have 2 consecutive Good stations been taken
    if (is.na(reducedSamplingD2G) == TRUE) {
      twoConsecutiveStations <-
        "Non-compliant: 2 consecutive stations at Good not returned"
    } else {
      twoConsecutiveStations <-
        "Compliant: 2 consecutive stations at Good are returned"
    }

    # 2 Build initial model ----------------------------------------------------

    try(mL4 <- suppressMessages(suppressWarnings(drm(IQI ~ Distance,
      data = innerTransect,
      fct = MM.3(),
      type = "continuous",
      control = drmc(
        noMessage = TRUE,
        warnVal = -1,
        trace = FALSE,
        otrace = FALSE
      )
    ))), silent = TRUE)
    try(mL4 <- suppressMessages(suppressWarnings(drm(IQI ~ Distance,
      data = innerTransect,
      fct = L.4(),
      type = "continuous",
      control = drmc(
        noMessage = TRUE,
        warnVal = -1,
        trace = FALSE,
        otrace = FALSE
      )
    ))), silent = TRUE)
    # Calculate Easting and Northing for re-use later
    easting_min <- unique(
      innerTransect$Easting[innerTransect$Distance ==
        min(innerTransect$Distance)]
    )
    northing_min <- unique(
      innerTransect$Northing[innerTransect$Distance ==
        min(innerTransect$Distance)]
    )

    easting_reduced <-
      innerTransect$Easting[innerTransect$Distance ==
        reducedSamplingD2G][1]
    northing_reduced <-
      innerTransect$Northing[innerTransect$Distance ==
        reducedSamplingD2G][1]

    if ((numberOfStations < 7) & (is.na(reducedSamplingD2G) == TRUE)) {
      # Situation 1 - Insufficient data to determine any distance to Good
      message(
        "Situation 1 - Insufficient data to determine any distance to Good"
      )
      summaryOutput <- rbind(
        summaryOutput,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          bestModel =
            "Insufficient data to determine distance using any method",
          ICresult = NA,
          convergedPercent = NA,
          dontAchieveGoodPercent = NA,
          stationNumber = stationNumber,
          twoConsecutiveStations = twoConsecutiveStations,
          reducedSamplingD2G = NA
        ))
      )

      D2Gdistr <- rbind(
        D2Gdistr,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          Bearing = unique(innerTransect$Bearing),
          Easting = easting_min,
          Northing = northing_min,
          D2G = rep(NA, 500),
          D2Ghist = rep(NA, 500),
          D2Gtype = "No result"
        ))
      )

      D2GbestFitResults <- rbind(
        D2GbestFitResults,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          Bearing = unique(innerTransect$Bearing),
          Easting = easting_min,
          Northing = northing_min,
          D2G = NA
        ))
      )

      hexdf <- data.frame(cbind(
        MCFF = unique(innerTransect$MCFF),
        Transect = unique(innerTransect$Transect),
        Distance = NA,
        IQI = NA,
        ID = NA,
        Counts = NA,
        Source = NA
      ))

      surveyData <- data.frame(cbind(
        innerTransect$MCFF,
        innerTransect$Transect,
        innerTransect$Distance,
        innerTransect$IQI,
        "",
        "",
        "Survey data"
      ))
      names(surveyData) <- c(
        "MCFF",
        "Transect",
        "Distance",
        "IQI",
        "ID",
        "Counts",
        "Source"
      )
      hexdf <- rbind(hexdf, surveyData)
      hexdfOut <- rbind(hexdfOut, hexdf)
    } else if ((numberOfStations < 7) & (is.na(reducedSamplingD2G) == FALSE)) {
      message(
        "Situation 2 - Insufficient data for regression model, but do have
      reduced monitoring result to use"
      )
      summaryOutput <- rbind(
        summaryOutput,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          bestModel = "Insufficient stations to run full model",
          ICresult = NA,
          convergedPercent = NA,
          dontAchieveGoodPercent = NA,
          stationNumber = stationNumber,
          twoConsecutiveStations = twoConsecutiveStations,
          reducedSamplingD2G = reducedSamplingD2G
        ))
      )

      D2Gdistr <- rbind(
        D2Gdistr,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          Bearing = unique(innerTransect$Bearing),
          Easting = easting_reduced,
          Northing = northing_reduced,
          D2G = rep(0, 500),
          D2Ghist = rep(reducedSamplingD2G, 500),
          D2Gtype = "Reduced analysis"
        ))
      )

      D2GbestFitResults <- rbind(
        D2GbestFitResults,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          Bearing = unique(innerTransect$Bearing),
          Easting = easting_reduced,
          Northing = northing_reduced,
          D2G = 0
        ))
      )

      hexdf <- data.frame(cbind(
        MCFF = unique(innerTransect$MCFF),
        Transect = unique(innerTransect$Transect),
        Distance = reducedSamplingD2G,
        IQI = unique(
          innerTransect[innerTransect$Distance == reducedSamplingD2G, ]$IQI
        ),
        ID = NA,
        Counts = NA,
        Source = "2 station rule"
      ))

      surveyData <- data.frame(cbind(
        innerTransect$MCFF,
        innerTransect$Transect,
        innerTransect$Distance,
        innerTransect$IQI,
        "",
        "",
        "Survey data"
      ))
      names(surveyData) <- c(
        "MCFF",
        "Transect",
        "Distance",
        "IQI",
        "ID",
        "Counts",
        "Source"
      )
      hexdf <- rbind(hexdf, surveyData)
      hexdfOut <- rbind(hexdfOut, hexdf)
    } else if ((exists("mL4") == FALSE) &
      (is.na(reducedSamplingD2G) == FALSE)) {
      message(
        "Situation 3 - Unable to fit regression model, but do have reduced monitoring result to use"
      )
      summaryOutput <- rbind(
        summaryOutput,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          bestModel = "Not possible to fit model to these data",
          ICresult = NA,
          convergedPercent = NA,
          dontAchieveGoodPercent = NA,
          stationNumber = stationNumber,
          twoConsecutiveStations = twoConsecutiveStations,
          reducedSamplingD2G = reducedSamplingD2G
        ))
      )

      D2Gdistr <- rbind(
        D2Gdistr,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          Bearing = unique(innerTransect$Bearing),
          Easting = easting_reduced,
          Northing = northing_reduced,
          D2G = rep(0, 500),
          D2Ghist = rep(reducedSamplingD2G, 500),
          D2Gtype = "Reduced analysis"
        ))
      )

      D2GbestFitResults <- rbind(
        D2GbestFitResults,
        data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          Bearing = unique(innerTransect$Bearing),
          Easting = easting_reduced,
          Northing = northing_reduced,
          D2G = 0
        ))
      )

      hexdf <- data.frame(cbind(
        MCFF = unique(innerTransect$MCFF),
        Transect = unique(innerTransect$Transect),
        Distance = NA,
        IQI = NA,
        ID = NA,
        Counts = NA,
        Source = "2 station rule"
      ))

      surveyData <- data.frame(cbind(
        innerTransect$MCFF,
        innerTransect$Transect,
        innerTransect$Distance,
        innerTransect$IQI,
        "",
        "",
        "Survey data"
      ))
      names(surveyData) <- c(
        "MCFF",
        "Transect",
        "Distance",
        "IQI",
        "ID",
        "Counts",
        "Source"
      )
      hexdf <- rbind(hexdf, surveyData)
      hexdfOut <- rbind(hexdfOut, hexdf)
    } else {
      # 3 Do model comparison --------------------------------------------------
      # Check whether linear model may be better (informal lack-of-fit test)
      linCheck <- data.frame(
        suppressMessages(suppressWarnings(mselect(mL4,
          list(L.4()),
          sorted = "IC",
          linreg = TRUE,
          icfct = AIC
        )))
      )
      linCheck$model <- row.names(linCheck)
      linCheck <- linCheck[linCheck$model != "Quad" &
        linCheck$model != "Cubic", ]
      if (linCheck[1, 5] == "Lin") {
        linCheckMsg <- " (a linear fit may offer better performance)"
      } else {
        linCheckMsg <- " (non-linear offers best performance)"
      }

      # Test alternative regression models:
      modelComp <- data.frame(suppressMessages(suppressWarnings(mselect(mL4,
        list(
          L.4(),
          L.5(),
          MM.3()
        ),
        sorted = "IC",
        linreg = FALSE,
        icfct = AIC
      )))) # L,no LL,MM3,no MM2,no AR,no BC5
      modelComp <- data.frame(modelComp[, c(2, 4)])
      # Check if simpler model significantly different - if so simplify
      modParams <- data.frame(cbind(
        Model = c(
          "L.3",
          "L.4",
          "L.5",
          "LL.2",
          "LL.3",
          "LL.3u",
          "LL.4",
          "LL.5",
          "W1.2",
          "W1.3",
          "W1.4",
          "W2.2",
          "W2.3",
          "W2.4",
          "BC.4",
          "BC.5",
          "LL2.2",
          "LL2.3",
          "LL2.3u",
          "LL2.4",
          "LL2.5",
          "AR.2",
          "AR.3",
          "MM.2",
          "MM.3"
        ),
        Params = c(
          3,
          4,
          5,
          2,
          3,
          3,
          4,
          5,
          2,
          3,
          4,
          2,
          3,
          4,
          4,
          5,
          2,
          3,
          3,
          4,
          5,
          2,
          3,
          2,
          3
        )
      )) # Change
      modelComp$Model <- row.names(modelComp)
      modelComp <- merge(modelComp, modParams, by = "Model")
      modelComp <- modelComp[order(modelComp$IC), ]

      modelComp3params <- modelComp[modelComp$Params == 3, ]
      modelComp3params <- modelComp3params[1, ]
      modelComp4params <- modelComp[modelComp$Params == 4, ]
      modelComp4params <- modelComp4params[1, ]
      bestModel <- modelComp[1, ]

      ICresult <- "Best fitting model used"

      if (bestModel$Params == 3) {
        ICresult <- "Best fitting model used"
      } else if (bestModel$Params == 4) {
        if (((modelComp3params$IC - 10) < bestModel$IC) &
          (is.na(modelComp3params$IC) == FALSE)) {
          message("Replace with 3 param model")
          modelComp <- modelComp3params
          ICresult <- "Best fitting model replaced by simpler one"
        }
      } else if (bestModel$Params == 5) {
        if (((modelComp3params$IC - 10) < bestModel$IC) &
          (is.na(modelComp3params$IC) == FALSE)) {
          message("Replace with 3 param model")
          modelComp <- modelComp3params
          ICresult <- "Best fitting model replaced by simpler one"
        } else if (((modelComp4params$IC - 10) < bestModel$IC) &
          (is.na(modelComp4params$IC) == FALSE)) {
          message("Replace with 4 param model")
          modelComp <- modelComp4params
          ICresult <- "Best fitting model replaced by simpler one"
        }
      }
      # Identify best fit
      bestFit1 <- cbind(MCFF_Transect = i, Formula = modelComp[1, 1])

      # Update model with the best one:
      modelList <- list(
        drc::L.3(),
        drc::L.4(),
        drc::L.5(),
        drc::LL.2(),
        drc::LL.3(),
        drc::LL.3u(),
        drc::LL.4(),
        drc::LL.5(),
        drc::W1.2(),
        drc::W1.3(),
        drc::W1.4(),
        drc::W2.2(),
        drc::W2.3(),
        drc::W2.4(),
        drc::BC.4(),
        drc::BC.5(),
        drc::LL2.2(),
        drc::LL2.3(),
        drc::LL2.3u(),
        drc::LL2.4(),
        drc::LL2.5(),
        drc::AR.2(),
        drc::AR.3(),
        drc::MM.2(),
        drc::MM.3()
      ) # Change
      names(modelList) <- c(
        "L.3",
        "L.4",
        "L.5",
        "LL.2",
        "LL.3",
        "LL.3u",
        "LL.4",
        "LL.5",
        "W1.2",
        "W1.3",
        "W1.4",
        "W2.2",
        "W2.3",
        "W2.4",
        "BC.4",
        "BC.5",
        "LL2.2",
        "LL2.3",
        "LL2.3u",
        "LL2.4",
        "LL2.5",
        "AR.2",
        "AR.3",
        "MM.2",
        "MM.3"
      ) # Change

      bestModel <- modelList[paste(bestFit1[, 2])][[1]]
      # bestModel <- modelList[[grep(bestFit1[, 2], modelList)[1]]]
      # if(i == "West of Burwick - 4") {
      #   browser()
      # }
      mL4 <- suppressMessages(suppressWarnings(drm(IQI ~ Distance,
        data = innerTransect,
        fct = bestModel,
        type = "continuous",
        control = drmc(
          noMessage = TRUE,
          warnVal = -1,
          trace = FALSE,
          otrace = FALSE
        )
      )))
      residsOut <- data.frame(mL4$predres)
      # if(loess == TRUE ) {
      #   mL4 <- stats::loess(IQI ~ Distance, data = innerTransect)
      #   residsOut <- data.frame("Predicted values" = mL4$fitted,
      #                           "Residuals" = mL4$residuals)
      # }

      # Collate info on best fit
      if (exists("bestFit1Collated") == FALSE) {
        bestFit1Collated <- bestFit1
      } else {
        bestFit1Collated <- rbind(bestFit1Collated, bestFit1)
      }

      # Make curve graph
      distVec <- data.frame(seq(
        from = 0,
        to = max(as.integer(innerTransect$Distance)),
        by = 1
      ))

      if (loess == TRUE) {
        mL4 <- loess(IQI ~ Distance, data = innerTransect)
        distance <- data.frame("Distance" = distVec[, 1])
        ypred_mL4 <- suppressMessages(suppressWarnings(predict(mL4,
          newdata = distance
        )))
        bestFit <- data.frame(
          "Distance" = distVec[, 1],
          "IQI" = ypred_mL4
        )
      } else {
        ypred_mL4 <- suppressMessages(suppressWarnings(predict(mL4,
          newdata = distVec,
          level = 0.95,
          interval = "confidence"
        )))
        bestFit <- cbind(distVec, ypred_mL4[, 1])
        names(bestFit) <- c("Distance", "IQI")
      }
      bestFit$Transect <- i
      if (method == "iqi") {
        D2GbestFit <- as.numeric(
          bestFit$Distance[min(which(bestFit$IQI >= good_moderate))]
        )
      } else {
        D2GbestFit <- as.numeric(
          bestFit$Distance[min(which(bestFit$IQI < good_moderate))]
        )
      }


      # 4 Explore model uncertainty --------------------------------------------
      # Produce bootstrapped data for later fitting

      bootDRC <- function(fittedModel) {
        mLboot <- NULL
        data2 <- fittedModel$origData # Original data
        fitted1 <- fittedModel$predres[, 1] # Model predicted IQI values
        resid1 <- fittedModel$predres[, 2] # Residuals
        data2[, 5] <- fitted1 + sample(scale(resid1, scale = FALSE),
          replace = TRUE
        ) # Change column
        return(data2[, ])
      }
      if (loess == TRUE) {
        bootDRC <- function(fittedModel) {
          mLboot <- NULL
          data2 <- innerTransect # Original data
          fitted1 <- fittedModel$fitted # Model predicted IQI values
          resid1 <- fittedModel$residuals # Residuals
          data2[, 5] <- fitted1 + sample(scale(resid1, scale = FALSE),
            replace = TRUE
          ) # Change column
          return(data2[, ])
        }
      }
      niter <- 1000 # Number of bootstrap resamples
      mL4List <- (rep(list(mL4), niter))
      bootDRCdata <- lapply((mL4List), bootDRC)

      convergedCount <- rep(0, length(bootDRCdata))
      nonConvergedCount <- rep(0, length(bootDRCdata))
      ypred_mLBoot <- vector(mode = "list", length(bootDRCdata))
      distVec <- data.frame(
        Distance = seq(
          from = 0,
          to = max(as.integer(innerTransect$Distance)),
          by = 1
        )
      )
      numberConverged <- 0
      xy <- 1
      # if (i == "Bellister - 1") {
      #   browser()
      # }
      while ((numberConverged < 500) & (xy <= length(bootDRCdata))) {
        mLBoot <- NULL
        if (loess == FALSE) {
          try(mLBoot <- suppressMessages(suppressWarnings(drm(IQI ~ Distance,
            data = as.data.frame(bootDRCdata[xy]),
            fct = bestModel,
            type = "continuous",
            control = drmc(
              noMessage = TRUE,
              warnVal = -1,
              trace = FALSE,
              otrace = FALSE
            )
          ))), silent = TRUE)
        } else {
          try(mLBoot <- suppressMessages(suppressWarnings(loess(IQI ~ Distance,
            data = as.data.frame(bootDRCdata[xy])
          ))), silent = TRUE)
        }

        if (is.null(mLBoot) == FALSE) {
          convergedCount[xy] <- 1
          ypred_mLBoot[[xy]] <- data.frame(
            cbind(
              Distance = distVec,
              IQI = suppressMessages(suppressWarnings(
                predict(mLBoot,
                  newdata = distVec,
                  interval = "none"
                )
              ))
            )
          )
        } else {
          nonConvergedCount[xy] <- 1
        }
        numberConverged <- sum(convergedCount)
        xy <- xy + 1
      }

      convergedPercent <- round(100 * sum(convergedCount) /
        (sum(convergedCount) + sum(nonConvergedCount)), 1)
      bootDRCmods <- ypred_mLBoot[-which(sapply(ypred_mLBoot, is.null))]

      # Calculate distance to Good distribution
      if (method == "iqi") {
        D2Gfunc <- function(x) {
          if ((max(x$IQI) >= good_moderate) & (x$IQI[nrow(x)] >= x$IQI[1])) {
            as.numeric(x$Distance[min(which(x$IQI >= good_moderate))])
          } else {
            NA
          }
        }
      } else {
        D2Gfunc <- function(x) {
          if ((max(x$IQI) < good_moderate) & (x$IQI[nrow(x)] >= x$IQI[1])) {
            as.numeric(x$Distance[min(which(x$IQI < good_moderate))])
          } else {
            NA
          }
        }
      }



      distanceToGoodDist <- lapply(bootDRCmods, D2Gfunc)
      distanceToGoodDist <- as.data.frame(t(as.data.frame(distanceToGoodDist)))
      names(distanceToGoodDist) <- c("D2G")

      distanceToGoodDistIsNull <- length(
        distanceToGoodDist$D2G[is.na(distanceToGoodDist$D2G) == TRUE]
      )
      distanceToGoodDistNotNull <- length(
        distanceToGoodDist$D2G[is.na(distanceToGoodDist$D2G) == FALSE]
      )

      d2g_is_na <- distanceToGoodDist$D2G[is.na(distanceToGoodDist$D2G) == TRUE]
      dontAchieveGoodPercent <- round(100 *
        length(d2g_is_na) /
        length(distanceToGoodDist$D2G), 1)

      bootDRCmodsUnlisted <- do.call(rbind.data.frame, bootDRCmods)
      IQIheatData <- cbind(
        "Distance" = bootDRCmodsUnlisted$Distance,
        "IQI" = bootDRCmodsUnlisted$IQI
      )
      names(IQIheatData) <- c("Distance", "IQI")
      h <- hexbin(IQIheatData)
      hexdf <- data.frame(hcell2xy(h),
        hexID = h@cell,
        counts = h@count
      )
      attr(hexdf, "cID") <- h@cID
      hexdf <- cbind(
        unique(innerTransect$MCFF),
        unique(innerTransect$Transect),
        hexdf,
        "Prob. model"
      )
      names(hexdf) <- c(
        "MCFF",
        "Transect",
        "Distance",
        "IQI",
        "ID",
        "Counts",
        "Source"
      )
      surveyData <- data.frame(cbind(
        innerTransect$MCFF,
        innerTransect$Transect,
        innerTransect$Distance,
        innerTransect$IQI,
        "",
        "",
        "Survey data"
      ))
      names(surveyData) <- c(
        "MCFF",
        "Transect",
        "Distance",
        "IQI",
        "ID",
        "Counts",
        "Source"
      )
      hexdf <- rbind(hexdf, surveyData)
      message(i)
      # Create outputs
      if ((convergedPercent >= convergenceCriterion) &
        (dontAchieveGoodPercent <= PercDontReachGoodCriterion)) {
        hexdfOut <- rbind(hexdfOut, hexdf)
        # "sit4"
        summaryOutput <- rbind(
          summaryOutput,
          data.frame(cbind(
            MCFF = unique(innerTransect$MCFF),
            MCFF_Transect = unique(innerTransect$MCFF_Transect),
            Transect = unique(innerTransect$Transect),
            bestModel = paste0(modelComp[1, 1], linCheckMsg),
            ICresult = ICresult,
            convergedPercent = convergedPercent,
            dontAchieveGoodPercent = dontAchieveGoodPercent,
            stationNumber = stationNumber,
            twoConsecutiveStations = twoConsecutiveStations,
            reducedSamplingD2G = reducedSamplingD2G
          ))
        )

        D2Gdistr <- rbind(
          D2Gdistr,
          data.frame(cbind(
            MCFF = unique(innerTransect$MCFF),
            MCFF_Transect = unique(innerTransect$MCFF_Transect),
            Transect = unique(innerTransect$Transect),
            Bearing = unique(innerTransect$Bearing),
            Easting = easting_min,
            Northing = northing_min,
            D2G = distanceToGoodDist$D2G,
            D2Ghist = distanceToGoodDist$D2G,
            D2Gtype = "Regression"
          ))
        )

        D2GbestFitResults <- rbind(
          D2GbestFitResults,
          data.frame(cbind(
            MCFF = unique(innerTransect$MCFF),
            MCFF_Transect = unique(innerTransect$MCFF_Transect),
            Transect = unique(innerTransect$Transect),
            Bearing = unique(innerTransect$Bearing),
            Easting = easting_min,
            Northing = northing_min,
            D2G = D2GbestFit
          ))
        )
      } else {
        if (is.na(reducedSamplingD2G) == TRUE) {
          # Situation 1A - Insufficient data to determine any distance to Good
          message("Situation 1A - Insufficient data to determine any distance to Good")
          summaryOutput <- rbind(
            summaryOutput,
            data.frame(cbind(
              MCFF = unique(innerTransect$MCFF),
              MCFF_Transect = unique(innerTransect$MCFF_Transect),
              Transect = unique(innerTransect$Transect),
              bestModel = paste0(modelComp[1, 1], linCheckMsg),
              ICresult = ICresult,
              convergedPercent = convergedPercent,
              dontAchieveGoodPercent = dontAchieveGoodPercent,
              stationNumber = stationNumber,
              twoConsecutiveStations = twoConsecutiveStations,
              reducedSamplingD2G = NA
            ))
          )

          D2Gdistr <- rbind(
            D2Gdistr,
            data.frame(cbind(
              MCFF = unique(innerTransect$MCFF),
              MCFF_Transect = unique(innerTransect$MCFF_Transect),
              Transect = unique(innerTransect$Transect),
              Bearing = unique(innerTransect$Bearing),
              Easting = easting_min,
              Northing = northing_min,
              D2G = rep(NA, 500),
              D2Ghist = rep(NA, 500),
              D2Gtype = "No result"
            ))
          )

          D2GbestFitResults <- rbind(
            D2GbestFitResults,
            data.frame(cbind(
              MCFF = unique(innerTransect$MCFF),
              MCFF_Transect = unique(innerTransect$MCFF_Transect),
              Transect = unique(innerTransect$Transect),
              Bearing = unique(innerTransect$Bearing),
              Easting = easting_min,
              Northing = northing_min,
              D2G = NA
            ))
          )

          surveyData <- data.frame(cbind(
            innerTransect$MCFF,
            innerTransect$Transect,
            innerTransect$Distance,
            innerTransect$IQI,
            "",
            "",
            "Survey data"
          ))
          names(surveyData) <- c(
            "MCFF",
            "Transect",
            "Distance",
            "IQI",
            "ID",
            "Counts",
            "Source"
          )
          hexdf <- rbind(hexdf, surveyData)
          hexdfOut <- rbind(hexdfOut, hexdf)
        } else {
          hexdfOut <- rbind(hexdfOut, hexdf)

          summaryOutput <- rbind(
            summaryOutput,
            data.frame(cbind(
              MCFF = unique(innerTransect$MCFF),
              MCFF_Transect = unique(innerTransect$MCFF_Transect),
              Transect = unique(innerTransect$Transect),
              bestModel =
                "Regression model fit not of sufficient quality to use",
              ICresult = NA,
              convergedPercent = convergedPercent,
              dontAchieveGoodPercent = dontAchieveGoodPercent,
              stationNumber = stationNumber,
              twoConsecutiveStations = twoConsecutiveStations,
              reducedSamplingD2G = reducedSamplingD2G
            ))
          )
          Easting <-
            innerTransect$Easting[innerTransect$Distance ==
              reducedSamplingD2G][1]
          Northing <-
            innerTransect$Northing[innerTransect$Distance ==
              reducedSamplingD2G][1]
          D2Gdistr <- rbind(
            D2Gdistr,
            data.frame(cbind(
              MCFF = unique(innerTransect$MCFF),
              MCFF_Transect = unique(innerTransect$MCFF_Transect),
              Transect = unique(innerTransect$Transect),
              Bearing = unique(innerTransect$Bearing),
              Easting = Easting,
              Northing = Northing,
              D2G = rep(0, 500),
              D2Ghist = rep(reducedSamplingD2G, 500),
              D2Gtype = "Reduced analysis"
            ))
          )

          D2GbestFitResults <- rbind(
            D2GbestFitResults,
            data.frame(cbind(
              MCFF = unique(innerTransect$MCFF),
              MCFF_Transect = unique(innerTransect$MCFF_Transect),
              Transect = unique(innerTransect$Transect),
              Bearing = unique(innerTransect$Bearing),
              Easting = Easting,
              Northing = Northing,
              D2G = 0
            ))
          )
        }
      }
    }
  } # End of outer loop combs

  # Reset manual overrides
  overrideTransect1 <- summaryOutput$MCFF_Transect[1]
  overrideTransect2 <- summaryOutput$MCFF_Transect[1]
  overrideTransect3 <- summaryOutput$MCFF_Transect[1]
  overrideDistance1 <- as.integer(-1)
  overrideDistance2 <- as.integer(-1)
  overrideDistance3 <- as.integer(-1)
  overrideBearing1 <- as.integer(-1)
  overrideBearing2 <- as.integer(-1)
  overrideBearing3 <- as.integer(-1)

  # Set outputs
  summaryOutput <- summaryOutput[-1, ]
  hexdfOut <- hexdfOut[-1, ]
  D2Gdistr <- D2Gdistr[-1, ]
  D2GbestFitResults <- D2GbestFitResults[-1, ]
  status <- "BeenRun"
  # If distance to good is NA - set to last station (minimal area of impact)
  if (any(is.na(D2GbestFitResults$D2G))) {
    # last station in transect is good?
    last_station <- data %>%
      group_by(Transect) %>%
      dplyr::arrange(Transect, desc(Station)) %>%
      dplyr::summarise(last_transect = dplyr::first(`WFD status`))

    last_station <- last_station %>% filter(Transect == which(is.na(D2GbestFitResults$D2G)))

    if (all(last_station$last_transect %in% c("Good", "High"))) {
      summaryOutput$type <-
        "area based on at least one transect reaching good at last station"
      summaryOutput$sign <- NA
    } else {
      summaryOutput$type <- "minimal area"
      summaryOutput$sign <- ">"
    }

    mini_dist_good <- data %>%
      group_by(Transect) %>%
      dplyr::summarise(mini_dist_good = max(Distance))

    D2GbestFitResults$D2G[is.na(D2GbestFitResults$D2G)] <-
      mini_dist_good$mini_dist_good[which(is.na(D2GbestFitResults$D2G))]



    mini_dist_good$Transect <- as.character(mini_dist_good$Transect)
    D2Gdistr <- dplyr::inner_join(D2Gdistr, mini_dist_good, by = "Transect")
    D2Gdistr$D2G <- D2Gdistr$mini_dist_good
  }
  # Put outputs into list
  data <- list(
    summaryOutput,
    D2Gdistr,
    D2GbestFitResults,
    hexdfOut
  )
  names(data) <- c("data", "geoDf", "geoDfBestFit", "hexdfOut")

  return(data)
}
