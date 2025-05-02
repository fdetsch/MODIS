### . getExtension ----

xtn0 = Map(
  MODIS:::getExtension
  , c("GTiff", "HDF4Image", "ENVI", "raw binary")
)

expect_true(
  all(
    unlist(xtn0) == c(".tif", ".hdf", "", ".hdr")
  )
  , info = "important format extensions are correct"
)


### `fixOrphanedHoles()` ----

## example polygon taken from `?sf::st_make_valid`
p = sf::st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))")

expect_false(
  sf::st_is_valid(p)
  , info = "created geometry is definitively invalid"
)

pr = MODIS:::fixOrphanedHoles(p)

expect_true(
  sf::st_is_valid(pr)
  , info = "fixed invalid geometry"
)

expect_identical(
  MODIS:::fixOrphanedHoles(pr)
  , target = pr
  , info = "valid geometries are returned unmodified"
)

## fixable on plane, but not on sphere (see 
## https://github.com/r-spatial/sf/issues/1732)
if (require(mapdata, quietly = TRUE)) {
  
  uses_s2 = sf::sf_use_s2()
  
  library(mapdata)
  
  m1 = maps::map("worldHires", "Spain", plot = FALSE, fill = TRUE)
  p1 = sf::st_as_sf(m1)
  
  pr1 = MODIS:::fixOrphanedHoles(p1)
  
  expect_true(
    sf::st_is_valid(pr1)
    , info = "fixed geometry that can only be valid on plane"
  )
  
  expect_identical(
    sf::sf_use_s2()
    , target = uses_s2
    , info = "output of `sf::sf_use_s2()` is same as before"
  )
}

