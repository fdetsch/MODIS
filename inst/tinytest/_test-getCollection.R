expect_stdout(
  getCollection("MCD12Q1.051", forceCheck = TRUE)
  , pattern = "not available in collection '051'"
  , info = "non-available collection creates console output"
)
