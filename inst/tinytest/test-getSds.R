hdf_name = system.file(
  "external"
  , "MOD13A2.A2016145.h18v04.006.2016166145124.hdf"
  , package = "MODIS"
)


### . w/o 'SDSstring' ----

layers = try(
  MODIS:::getSds(
    hdf_name
  )
  , silent = TRUE
)

if (!inherits(layers, "try-error")) {
  
  ## structure
  expect_true(
    all(
      inherits(layers, "list")
      , length(layers) == 2
      , names(layers) == c("SDSnames", "SDS4gdal")
    )
    , info = "list structure w/o 'SDSstring' looks right"
  )
  
  ## content
  expect_true(
    all(
      length(layers$SDSnames) == 3
      , grepl(
        "1 km 16 days (NDVI|VI Quality|pixel reliability)$"
        , layers$SDSnames
      )
    )
    , info = "Slot 'SDSnames' has expected length and content"
  )
  
  expect_true(
    length(layers$SDS4gdal) == 3
    , info = "Slot 'SDS4gdal' has expected length"
  )
  
  # test_that(
  #   "'gdal' and 'mrt' methods create identical output"
  #   , {
  #     expect_identical(
  #       layers[1]
  #       , getSds(
  #         hdf_name
  #         , method = "mrt"
  #       )
  #     )
  #   }
  # )
}


### . w/'SDSstring' ----

sds_string = "101"

layers1 = try(
  MODIS:::getSds(
    hdf_name
    , SDSstring = sds_string
  )
  , silent = TRUE
)

if (!inherits(layers1, "try-error")) {
  
  ## structure
  expect_true(
    all(
      inherits(layers1, "list")
      , length(layers1) == 3
      , names(layers1) == c("SDSnames", "SDSstring", "SDS4gdal")
    )
    , info = "list structure w/'SDSstring' looks right"
  )
  
  ## content
  expect_true(
    all(
      length(layers1$SDSnames) == 2
      , grepl(
        "1 km 16 days (NDVI|pixel reliability)$"
        , layers1$SDSnames
      )
    )
    , info = "Slot 'SDSnames' has expected length and content"
  )
  
  expect_identical(
    layers1$SDSstring
    , target = "1 0 1"
    , info = "Slot 'SDSstring' has expected content"
  )
  
  expect_true(
    length(layers1$SDS4gdal) == 2
    , info = "Slot 'SDS4gdal' has expected length"
  )
  
  ## comparison
  expect_identical(
    layers$SDSnames[c(1, 3)]
    , layers1$SDSnames
    , info = "Slot 'SDSnames' w/'SDSstring' is a valid subset of w/o method"
  )
  
  expect_identical(
    layers$SDS4gdal[c(1, 3)]
    , layers1$SDS4gdal
    , info = "Slot 'SDS4gdal' w/'SDSstring' is a valid subset of w/o method"
  )
}
