test_that("area function output matches Spotfire script", {
  probability <- readRDS(
    system.file("extdat",
      "test-data/2021-05-26-bellister/test-probability.rds",
      package = "halia"
    )
  )
  breach <- breach(probability)
  area <- area(breach)
  expect_equal(class(area$ellipse), c("sf", "data.frame"))
  # Matches Spotfire script output for Bellister 2021-05-26
  expect_equal(area$fifthPercentileArea[[1]][1], 96914.924)
  expect_equal(class(area$spotfire_ellipse), "AsIs")
})
