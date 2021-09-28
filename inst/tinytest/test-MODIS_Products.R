prd = MODIS:::MODIS_Products

## class
expect_inherits(
  prd
  , class = "list"
  , info = "built-in products data inherits from class 'list'"
)

## `names()`
expect_identical(
  names(prd)
  , c(
    "SENSOR", "PRODUCT", "PLATFORM"
    , "PF1", "PF2", "PF3", "PF4"
    , "TOPIC", "TYPE", "RES", "TEMP_RES"
    , "INTERNALSEPARATOR", "SOURCE", "POS1", "POS2"
  )
  , info = "built-in products list has expected names"
)

## `lengths()`
expect_true(
  all(lengths(prd) == nrow(MODIS::getProduct()))
  , info = "lengths of all list slots equal # of products in `getProduct()`"
)

## content
expect_true(
  all(prd$PLATFORM %in% c("Combined", "Terra", "Aqua"))
  , info = "available platforms are terra, aqua, and combined"
)

expect_true(
  all(prd$PF1 %in% c("MOTA", "MOLT", "MOLA"))
  , info = "available lpdaac server path extensions are molt, mola, and mota"
)

expect_true(
  all(prd$PF2 %in% c("MCD", "MOD", "MYD"))
  , info = "available laads server path extensions are mod, myd, and mcd"
)

expect_true(
  all(is.na(prd$PF4) | prd$PF4 %in% c("MOST", "MOSA"))
  , info = "available nsidc server path extensions are most, mosa, or na"
)

expect_true(
  all(prd$TYPE %in% c("Tile", "CMG"))
  , info = "available image types are tile and climate modeling grid (cmg)"
)
