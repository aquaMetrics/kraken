test_that("override works", {
  probability <- readRDS(
    system.file("extdat",
      "test-data/2021-05-26-bellister/test-probability.rds",
      package = "aquaman"
    )
  )

  override_test <- override(probability)
  breachs <- breach(override_test)
  areas <- area(breachs)
  # Test if no overrides provided then default is not to change output
  expect_equal(areas$fifthPercentileArea[[1]], 96914.924)

  overrides <- override(probability,
    overrideTransect4 = 68,
    overrideBearing1 = -45.5962205669678,
    overrideBearing2 = 43.7583771665816,
    overrideBearing3 = 112.813019090209,
    overrideBearing4 = -142.682429683921
  )
  breachs <- breach(overrides)
  areas <- area(breachs)
  # Test against value copied from original Spotfire tool when default for
  # overrideTransect4 is set to 68 meters. Bearings have been kept the same, but
  # testing if entering the same bearing value does not change outcome.
  expect_equal(areas$fifthPercentileArea[[1]], 110751.943)
})
