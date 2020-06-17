context("runGdal")

lap = file.path(tempdir(), "MODIS_ARC")
odp = file.path(lap, "PROCESSED")
jnk = capture.output(
  suppressMessages(
    MODISoptions(localArcPath = lap, outDirPath = odp
                 , save = FALSE, quiet = TRUE, checkTools = FALSE)
  )
)

jnk = capture.output(
  orgStruc(system.file("external", package = "MODIS")
           , pattern = "^MOD13A2.A2016145.h18v04.006.2016166145124.hdf$"
           , quiet = TRUE)
)


### 0 default settings ----

jnk = capture.output(
  tfs0 <- runGdal("MOD13A2", collection = "006"
                 , begin = "2016145", end = "2016145"
                 , tileH = 18, tileV = 4
                 , SDSstring = "111", job = "test-runGdal"
                 , checkIntegrity = FALSE, forceDownload = FALSE
                 , overwrite = TRUE)
)

rst0 = raster::stack(unlist(tfs0))

test_that("runGdal() creates standard output", {
  expect_equal(raster::nlayers(rst0), 3L)
  expect_equal(dim(rst0), c(1200, 1410, 3))
})


### 1 custom settings ----

jnk = capture.output(
  tfs1 <- runGdal(
    "MOD13A2", collection = "006"
    , begin = "2016145", end = "2016145"
    , tileH = 18, tileV = 4
    , SDSstring = "1", job = "test-runGdal"
    , outProj = 32632, pixelSize = 1000
    , checkIntegrity = FALSE, forceDownload = FALSE
    , overwrite = TRUE
  )
)

rst1 = raster::stack(unlist(tfs1))

test_that("runGdal() creates customized output", {
  expect_equal(raster::nlayers(rst1), 1L)
  expect_identical(raster::res(rst1), c(1000, 1000))
  expect_true(sf::st_crs(rst1) == sf::st_crs(32632))
})
