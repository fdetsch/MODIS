context("genString")

opt <- options("warn")[[1]]
options("warn" = 0)
on.exit(options("warn" = opt))

test_that("genString() throws a warning for multiple products", {
  expect_warning(genString(c("MYD15A2", "MOD15A2"), collection = "006"))
  expect_warning(genString("MOD14.*", collection = 6))
})

test_that("A warning is thrown for multiple files", {
  fls = c("MYD11A1.A2009001.h18v04.006.2015363221538.hdf", 
          "MYD11A1.A2009009.h18v04.006.2015364055036.hdf", 
          "MYD11A1.A2009017.h18v04.006.2015364115403.hdf")
  expect_warning(genString(fls))
})
