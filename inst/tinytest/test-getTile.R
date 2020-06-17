context("getTile")


### . points ----

data(meuse)
pts = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

test_that("target extent defaults to full tile for point features with length = 1", {
  expect_null(getTile(pts[1, ])@target$extent)
})

test_that("target extent is inherited from multi-point feature", {
  expect_is(getTile(pts[1:2, ])@target$extent, "Extent")
})


### . rasters ----

data(meuse.grid)
sp::gridded(meuse.grid) = ~ x + y
rst = raster::raster(meuse.grid)

## raster with no crs and invalid latlon coords
test_that("throw error for rasters lacking crs and with invalid coordinates", {
  expect_error(getTile(rst), regexp = "assign a valid CRS")
})

## raster with valid crs
raster::projection(rst) = "+init=epsg:28992"

test_that("target specs are inherited from raster", {
  # extent
  expect_is(getTile(rst)@target$extent, "Extent") # is 'Extent'
  expect_equal(getTile(rst)@target$extent, raster::extent(rst)) # is identical
  
  # crs
  expect_equal(getTile(rst)@target$outProj, sf::st_crs(rst))
  
  # res
  expect_equal(getTile(rst)@target$pixelSize, raster::res(rst))
  
})

## raster with no crs, but valid latlon coords
rst_ll = raster::projectRaster(rst, crs = "+init=epsg:4326")
raster::projection(rst_ll) = NA

test_that("throw no error for rasters lacking crs and with valid coordinates", {
  expect_is(tl <- getTile(rst_ll), "MODISextent")
})
