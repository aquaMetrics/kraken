test_that("survey import works", {
  file <- system.file("extdat",
                      "demo-data/220421-SelfMon-N4952-CAV1-Enhanced.xlsx",
                      package = "kraken"
  )
  data <- survey_import(file)
})

