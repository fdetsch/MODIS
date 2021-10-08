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

expect_equivalent(
  sapply(
    prd
    , class
  )
  , c(
    rep("character", 12)
    , "list"
    , rep("integer", 2)
  )
  , info = "built-in products list has expected classes"
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
  all(prd$PF1 %in% c("MOTA", "MOLT", "MOLA") | is.na(prd$PF1))
  , info = "available lpdaac/laads path features are molt, mola, and mota"
)

expect_true(
  all(prd$PF2 %in% c("MCD", "MOD", "MYD"))
  , info = "available product-specific path features are mod, myd, and mcd"
)

expect_true(
  all(is.na(prd$PF4) | prd$PF4 %in% c("MOST", "MOSA"))
  , info = "available nsidc path features are most, mosa, or na"
)

expect_identical(
  is.na(prd$PF4)
  , !is.na(prd$PF1)
  , info = "path features are empty for nsidc where available for lpdaac/laads"
)

expect_identical(
  is.na(prd$PF1)
  , !is.na(prd$PF4)
  , info = "path features are empty for lpdaac/laads where available for nsidc"
)

expect_true(
  all(prd$TYPE %in% c("Tile", "CMG", "Swath"))
  , info = "available image types are tile, cmg, and swath"
)

expect_true(
  all(prd$POS2 - prd$POS1 == 6)
  , info = "date string in file name is always 6 characters long"
)
