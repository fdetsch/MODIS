context("getProduct")

tst = getProduct("MCD12Q1.006", quiet = TRUE) # MODISproduct
inherits(tst, "MODISproduct")
