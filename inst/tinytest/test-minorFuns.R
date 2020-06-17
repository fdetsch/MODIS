context("minorFuns")


### . getExtension ----

xtn0 = Map(
  MODIS:::getExtension
  , c("GTiff", "HDF4Image", "ENVI", "raw binary")
)

test_that(
  "important format extensions are correct"
  , {
    expect_identical(xtn0$GTiff, ".tif")
    expect_identical(xtn0$HDF4Image, ".hdf")
    expect_identical(xtn0$ENVI, "")
    expect_identical(getExtension("raw binary"), ".hdr")
  }
)
