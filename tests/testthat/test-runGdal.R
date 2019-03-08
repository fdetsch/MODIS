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

jnk = capture.output(
  tfs <- runGdal("MOD13A2", collection = "006"
                 , begin = "2016145", end = "2016145"
                 , tileH = 18, tileV = 4
                 , SDSstring = "111", job = "test-runGdal"
                 , checkIntegrity = FALSE, forceDownload = FALSE)
)

test_that("runGdal() creates expected output", {
  fls = unlist(tfs)
  expect_length(fls, 3L)
  
  rst = raster::stack(fls)
  expect_is(rst, "Raster")
  expect_equal(dim(rst), c(185, 219, 3))
})

