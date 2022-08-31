## early exit: download method invalid
expect_error(
  MODISoptions(
    dlmethod = "lynx"
  )
  , pattern = "dlmethod %in% .* is not TRUE"
)
