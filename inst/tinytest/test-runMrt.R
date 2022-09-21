expect_error(
  runMrt(
    "MOD13Q1"
    , datum = "WGS85"
  )
  , pattern = "'arg' should be one of"
)