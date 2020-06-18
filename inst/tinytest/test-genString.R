wrn = getOption("warn")
options("warn" = 0)

expect_warning(
  MODIS:::genString(c("MYD15A2", "MOD15A2"), collection = "006")
  , info = "warning is thrown in case of 2+ products"
)
expect_warning(
  MODIS:::genString("MOD14.*", collection = 6) # matches three products
  , info = "warning is thrown for pattern matching multiple products"
)

fls = c(
  "MYD11A1.A2009001.h18v04.006.2015363221538.hdf", 
  "MYD11A1.A2009009.h18v04.006.2015364055036.hdf", 
  "MYD11A1.A2009017.h18v04.006.2015364115403.hdf"
)
expect_warning(
  MODIS:::genString(fls)
  , info = "warning is thrown in case of 2+ files"
)

options("warn" = wrn)