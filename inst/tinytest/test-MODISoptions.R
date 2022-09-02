## early exit: download method invalid
expect_error(
  MODISoptions(
    dlmethod = "lynx"
  )
  , pattern = "dlmethod %in% .* is not TRUE"
)

## early exit: download server(s) invalid
expect_error(
  MODISoptions(
    MODISserverOrder = "NSDIC"
    , check_earthdata_login = FALSE
  )
  , pattern = "Provide valid 'MODISserverOrder'"
)
