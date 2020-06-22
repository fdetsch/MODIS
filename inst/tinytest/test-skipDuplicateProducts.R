### . w/o pattern matching ----

expect_warning(
  out1 <- MODIS:::skipDuplicateProducts("MOD14")
  , info = "product selection w/o pattern matching produces a warning"
)

expect_equivalent(
  out1
  , target = "^MOD14$"
  , info = "product selection w/o pattern matching skips derivatives"
)


### . w/pattern matching ----

expect_silent(
  out2 <- MODIS:::skipDuplicateProducts("MOD14.*")
  , info = "product selection w/pattern matching produces no warning"
)

expect_equivalent(
  out2
  , target = "MOD14.*"
  , info = "product selection w/o pattern matching includes derivatives"
)

expect_true(
  length(getProduct(out2, quiet = TRUE)@PRODUCT) ==
    length(grep(out2, MODIS:::MODIS_Products$PRODUCT))
  , info = "expected number of products is returned"
)
