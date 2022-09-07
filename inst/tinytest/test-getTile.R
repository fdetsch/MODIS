### . points ----

data(meuse, package = "sp")
pts = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

tls_pt = getTile(pts[1, ])

expect_null(
  tls_pt@target$extent
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
suppressWarnings(
  raster::projection(rst) <- "+init=epsg:28992"
)
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


### spherical geometry w/o s2 ----

## sample data
dsn = system.file(
  "vectors/Up.tab"
  , package = "rgdal"
)[1]

Up = sf::st_read(
  dsn
  , quiet = TRUE
)

expect_true(
  any(
    !sf::st_is_valid(
      Up
    )
  )
  , info = "sample data for testing spherical geometry w/o s2 is invalid"
)

expect_inherits(
  getTile(Up)
  , class = "MODISextent"
  , info = "not using s2 for geometries with ellipsoidal coordinates succeeds"
)


### 'sfc' ----

expect_identical(
  getTile(
    sf::st_as_sfc(
      pts
    )[1]
  )
  , target = tls_pt
  , info = "inputs with 'sfc' signature create same output as 'sf' analogs"
)

expect_inherits(
  getTile(
    sf::st_as_sfc(
      subset(
        MODIS:::sr
        , h == 21L & v == 9L
      )
    )
  )
  , class = "MODISextent"
  , info = "also works with 'sfc_POLYGON' input"
)
