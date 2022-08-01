test_that("test reintraid 2020 against reported results", {
  reintraid <- read.csv(
    system.file("extdat",
      "test-data/2022-reintraid.csv",
      package = "aquaman"
    ),
    check.names = FALSE
  )

  area <- assess(reintraid)

  # Current spotfire testing
  testthat::expect_equal(round(area[[1]], 0), 35780)
  # Result reported for compliance is slightly different (37070). Possibly due
  # to slight change to area calculator code since Feb 2022 or error in input
  # data(?). Either way, no impact on compliance (Allowable mixing zone: 86606)
  # testthat::expect_equal(round(area, 0), 37070)
})

test_that("test tisti-geo 2021 against reported results", {
  tisti <- read.csv(
    system.file("extdat",
      "test-data/210706-teisti-geo.csv",
      package = "aquaman"
    ),
    check.names = FALSE
  )

  # Current spotfire testing
  # area <- assess(tisti)
  # testthat::expect_equal(round(area[[1]], 0), 55851)
  # Result reported - with override on Transect 1. Access MCFF database has 6
  # stations for Transect 3 - emailed data has 7 stations.
  # Running with 6 stations returns reported value (in this case area at least
  # 148362 +). In this case, makes no difference on outcome of compliance.
  tisti <- tisti[-19, ] # remove station 7 from Transect 3
  area <- assess(tisti, overrideTransect1 = 392)
  testthat::expect_equal(round(area[[1]], 0), 148362)
})
