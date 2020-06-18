## standard length
mod13q1 = "MOD13Q1.A2001001.h21v08.006.2015140082121.hdf"

expect_equivalent(
  extractDate(mod13q1)$inputLayerDates
  , target = "2001001"
  , info = "position indication works for standard-length products"
)
expect_equivalent(
  extractDate(mod13q1, asDate = TRUE)$inputLayerDates
  , target = as.Date("2001-01-01")
  , info = "if desired, 'Date' object is returned"
)

## longer than standard length
mod15a2h = c("MOD15A2H.A2017001.h21v08.006.2017017150854.hdf"
             , "MOD15A2H.A2017001.h21v09.006.2017017150810.hdf")

expect_equivalent(
  extractDate(mod15a2h)$inputLayerDates
  , target = rep("2017001", 2L)
  , info = "position indication works for longer than standard length products"
)

## shorter than standard length
mod44b = "MOD44B.A2000065.h00v08.006.2017081101524.hdf"

expect_equivalent(
  extractDate(mod44b)$inputLayerDates
  , target = "2000065"
  , info = "position indication works for shorter than standard length products"
)
