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

## early exit: `length(maskValue)` not `1L` or matching 'SDSstring'
jnk = utils::capture.output(
  expect_error(
    runGdal(
      "MCD15A2H"
      , collection = "061"
      , tileH = 21
      , tileV = c(7, 8)
      , begin = "2003001"
      , end = "2003010"
      , SDSstring = "110100"
      , maskValue = c(254L, 255L)
      , quiet = TRUE
    )
    , pattern = "'maskValue' length needs to be 1 or match 'SDSstring'"
  )
)
