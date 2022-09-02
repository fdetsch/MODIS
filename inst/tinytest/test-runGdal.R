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