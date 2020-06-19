expect_true(
  inherits(getProduct("MCD12Q1.006", quiet = TRUE), "MODISproduct")
  , info = "output inherits from class 'MODISproduct'"
)

