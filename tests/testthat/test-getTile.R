context("getTile")

data(meuse)
pts = sf::st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

test_that("target extent defaults to full tile for point features with length = 1", {
  expect_null(getTile(pts[1, ])@target$extent)
})

test_that("target extent is inherited from multi-point feature", {
  expect_is(getTile(pts[1:2, ])@target$extent, "Extent")
})
