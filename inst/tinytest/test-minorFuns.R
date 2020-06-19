### . getExtension ----

xtn0 = Map(
  MODIS:::getExtension
  , c("GTiff", "HDF4Image", "ENVI", "raw binary")
)

expect_true(
  all(
    unlist(xtn0) == c(".tif", ".hdf", "", ".hdr")
  )
  , info = "important format extensions are correct"
)
