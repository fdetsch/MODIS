expect_error(
  getCollection()
  , pattern = "Please provide a valid product"
  , info = "an error is raised if 'product' is missing"
)

expect_stdout(
  getCollection("MCD12Q1.051", forceCheck = TRUE)
  , pattern = "not available in collection '051'"
  , info = "non-available collection creates console output"
)
