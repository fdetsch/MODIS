context("behavior of product pattern matching")

opt <- options("warn")[[1]]
options("warn" = 0)
on.exit(options("warn" = opt))

test_that("product selection wo/pattern matching skips derivatives", {
  out1 = expect_warning(skipDuplicateProducts("MOD14"))
  expect_match(out1, "\\^MOD14\\$")
})

test_that("product selection w/pattern matching includes derivatives", {
  out2 = skipDuplicateProducts("MOD14.*")
  expect_match(out2, "MOD14\\.\\*")
  
  lns = capture.output(getProduct(out2))
  expect_equal(length(lns), length(grep(out2, MODIS_Products$PRODUCT)))
})
