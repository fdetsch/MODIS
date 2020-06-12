context("getSds")

hdf_name = system.file(
  "external"
  , "MOD13A2.A2016145.h18v04.006.2016166145124.hdf"
  , package = "MODIS"
)


### . w/o 'SDSstring' ----

layers = getSds(
  hdf_name
)

test_that(
  "getSds() w/o 'SDSstring' generates expected output"
  , {
    ## structure
    expect_type(
      layers
      , "list"
    )
    expect_length(
      layers
      , 2L
    )
    expect_named(
      layers
      , c("SDSnames", "SDS4gdal")
    )
    ## content
    expect_length(
      layers$SDSnames
      , 3L
    )
    expect_true(
      all(
        grepl(
          "1 km 16 days (NDVI|VI Quality|pixel reliability)$"
          , layers$SDSnames
        )
      )
    )
    expect_length(
      layers$SDS4gdal
      , 3L
    )
  }
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


### . w/'SDSstring' ----

sds_string = "101"

layers1 = getSds(
  hdf_name
  , SDSstring = sds_string
)

test_that(
  "getSds() w/'SDSstring' generates expected output"
  , {
    ## structure
    expect_type(
      layers1
      , "list"
    )
    expect_length(
      layers1
      , 3L
    )
    expect_named(
      layers1
      , c("SDSnames", "SDSstring", "SDS4gdal")
    )
    ## content
    expect_length(
      layers1$SDSnames
      , 2L
    )
    expect_true(
      all(
        grepl(
          "1 km 16 days (NDVI|pixel reliability)$"
          , layers1$SDSnames
        )
      )
    )
    expect_identical(
      layers1$SDSstring
      , "1 0 1"
    )
    expect_length(
      layers1$SDS4gdal
      , 2L
    )
  }
)

test_that(
  "getSds() w/'SDSstring' is an otherwise equal subset of w/o 'SDSstring' method"
  , {
    expect_identical(
      layers$SDSnames[c(1, 3)]
      , layers1$SDSnames
    )
    expect_identical(
      layers$SDS4gdal[c(1, 3)]
      , layers1$SDS4gdal
    )
  }
)
