context("position indication")

test_that("position indication works for standard-length products", {
  mod13q1 = "MOD13Q1.A2001001.h21v08.006.2015140082121.hdf"
  expect_match(extractDate(mod13q1)$inputLayerDates, "^2001001$")
  expect_equal(extractDate(mod13q1, asDate = TRUE)$inputLayerDates
               , as.Date("2001-01-01"))
})

test_that("position indication works for longer-than-standard products", {
  mod15a2h = c("MOD15A2H.A2017001.h21v08.006.2017017150854.hdf"
               , "MOD15A2H.A2017001.h21v09.006.2017017150810.hdf")
  expect_match(unique(extractDate(mod15a2h)$inputLayerDates), "^2017001$")
  expect_equal(unique(extractDate(mod15a2h, asDate = TRUE)$inputLayerDates)
               , as.Date("2017-01-01"))
})

test_that("position indication works for shorter-than-standard products", {
  mod44b = "MOD44B.A2000065.h00v08.006.2017081101524.hdf"
  expect_match(unique(extractDate(mod44b)$inputLayerDates), "^2000065$")
  expect_equal(unique(extractDate(mod44b, asDate = TRUE)$inputLayerDates)
               , as.Date("2000-03-05"))
})

