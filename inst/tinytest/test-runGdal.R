## PRODUCT INPUT ====

## early exit: product not available from a particular server
jnk = utils::capture.output(
  expect_error(
    runGdal(
      product = "MCD18C1" # only available on lpdaac
      , collection = "061"
      , tileH = 18L
      , tileV = 3L
      , begin = "2019.01.01"
      , end = "2019.12.31"
      , MODISserverOrder = c("LAADS", "NSIDC")
    )
    , pattern = "is not available on .* try another server or collection"
  )
)


## FILE INPUT ====

sysfile = system.file(
  "external/MOD13A2.A2016145.h18v04.006.2016166145124.hdf"
  , package = "MODIS"
)

jnk = suppressWarnings(
  utils::capture.output(
    tifs <- runGdal(
      sysfile
      , overwrite = TRUE
    )
  )
)

## investigate output
expect_inherits(
  tifs
  , class = "list"
)

expect_true(
  all(
    file.exists(
      unlist(
        tifs
        , use.names = FALSE
      )
    )
  )
  , info = "writes layers to temporary `.tif` files."
)

## early exit: 2+ input files
expect_error(
  runGdal(
    rep(
      sysfile
      , 2L
    )
  )
  , pattern = "^Processing of 2\\+ local .* files not supported, yet\\.$"
)

## early exit: `length(maskValue)` not `1L` or matching 'SDSstring'
jnk = utils::capture.output(
  expect_error(
    runGdal(
      sysfile
      , maskValue = c(254L, 255L)
      , quiet = TRUE
    )
    , pattern = "'maskValue' length needs to be 1 or match 'SDSstring'"
  )
)
