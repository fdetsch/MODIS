### . w/o pattern matching ----

expect_warning(
  out1 <- MODIS:::skipDuplicateProducts("MOD17A2H")
  , info = "product selection w/o pattern matching produces a warning"
)

expect_identical(
  out1
  , target = "^MOD17A2H$"
  , info = "product selection w/o pattern matching skips derivatives"
)


### . w/pattern matching ----

expect_silent(
  out2 <- MODIS:::skipDuplicateProducts("MOD17A2H.*")
  , info = "product selection w/pattern matching produces no warning"
)

expect_identical(
  out2
  , target = "MOD17A2H.*"
  , info = "product selection w/o pattern matching includes derivatives"
)

expect_identical(
  length(getProduct(out2, quiet = TRUE)@PRODUCT)
  , target = length(grep(out2, MODIS:::MODIS_Products$PRODUCT))
  , info = "expected number of products is returned"
)
