### . points ----

data(meuse, package = "sp")
pts = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

expect_null(
  getTile(pts[1, ])@target$extent
  , info = "target extent defaults to full tile for single point feature"
)

expect_true(
  inherits(getTile(pts[1:2, ])@target$extent, "Extent")
  , info = "target extent inherited from multi-point feature is of class 'Extent'"
)


### . rasters ----

data(meuse.grid, package = "sp")
x = sf::st_as_sf(
  meuse.grid
  , coords = c("x", "y")
)

rst = raster::raster(
  sf::st_as_sf(
    sf::st_make_grid(
      x
    )
  )
)

## raster with no crs and invalid latlon coords
expect_error(
  getTile(rst)
  , pattern = "assign a valid CRS"
  , info = "rasters lacking crs and with invalid coordinates produce an error"
)

## raster with valid crs
raster::projection(rst) = "+init=epsg:28992"
trgt = getTile(rst)@target

expect_true(
  inherits(trgt$extent, "Extent")
  , info = "target extent inherited from raster is of class 'Extent'"
)

expect_identical(
  trgt$extent
  , target = raster::extent(rst)
  , info = "target extent is inherited from raster"
)

expect_equivalent(
  trgt$outProj
  , target = sf::st_crs(rst)
  , info = "target crs is inherited from raster"
)

expect_equivalent(
  trgt$pixelSize
  , target = raster::res(rst)
  , info = "target resolution is inherited from raster"
)

## raster with no crs, but valid latlon coords
rst_ll = raster::projectRaster(rst, crs = "+init=epsg:4326")
raster::projection(rst_ll) = NA

expect_true(
  inherits(getTile(rst_ll), "MODISextent")
  , info = "rasters lacking crs and with valid coordinates produce regular output"
)
