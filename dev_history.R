library(MODIS)

## enable permanent file storage
lap = "~/Documents/data/MODIS_ARC"
MODISoptions(
  localArcPath = lap
  , outDirPath = file.path(
    lap
    , "PROCESSED"
  )
)

# TODO: handle absence of `.netrc` file (username, password `NULL`)
EarthdataLogin(
  usr = "ei_mr_flo"
  , pwd = rstudioapi::askForPassword()
)


## 2021-08-19 ====

### 111-strsplit-error ----

## debug(MODIS:::ModisFileDownloader)
tfs = runGdal(
  product = "MOD11A1"
  , begin = "2010001"
  , end = "2010005"
  , tileH = 21
  , tileV = 9
  , SDSstring = "101"
  , job = "mod11a1_h21v09"
)

strs = stars::read_stars(
  tfs$MOD11A1.061$`2010-01-01`[1]
) * 0.02

plot(
  strs
)


### 114-help-downloading-modis-data-in-r ----

product = "MOD13Q1"

## retrieve most recent collection
clc = getCollection(
  product
  , forceCheck = TRUE
)

## download and extract data
tfs = runGdal(
  product
  , collection = clc
  , tileH = 20
  , tileV = 11
  , begin = "2000.02.18"
  , end = "2000.02.18"
  , job = "MOD13Q1"
  , SDSstring = "11" # ndvi, evi
)


## 2021-08-31 ====

sf::gdal_utils(
  util = "info"
  , source = system.file(
    "external/MOD13A2.A2016145.h18v04.006.2016166145124.hdf"
    , package = "MODIS"
  )
)

product = "MOD11A1"

clc = getCollection(
  product
  , forceCheck = TRUE
)

paths = MODIS:::genString(
  product
  , collection = clc
  , date = "2020.01.01"
)

onl = MODIS:::filesUrl(
  paths$remotePath$LPDAAC
)

x = file.path(
  paths$remotePath$LPDAAC
  , grep(
    "h21v09.*.hdf$"
    , onl
    , value = TRUE
  )
)

## without credentials --> 'cannot open URL' error
download.file(
  x
  , destfile = tempfile(
    fileext = ".hdf"
  )
)

## without credentials --> empty response, ie `""`
jnk = sf::gdal_utils(
  util = "info"
  , source = x
)


## 2021-09-03 ====

MODISoptions()

hdfs = getHdf(
  "MOD11A1"
  , tileH = 21
  , tileV = 9
  , begin = "2021-08-01"
  , end = "2021-08-05"
  , MODISserverOrder = c("LAADS", "LPDAAC")
)

## nsidc collection retrieval
getCollection(
  "MOD10A1"
  , forceCheck = TRUE
)


## 2021-09-08 ====

if (!dir.exists("exec")) {
  dir.create(
    "exec"
  )
}

file.edit(
  "exec/exec-modis_laads_dl.R"
)


## 2022-09-01 ====

library(MODIS)

product = "MCD12Q1"

clc = getCollection(
  product
  , forceCheck = TRUE
)

(
  hdfs = getHdf(
    product
    , collection = clc
    , begin = "2000.01.01"
    , end = "2003.12.31"
    , tileH = 21L
    , tileV = 9L
    , MODISserverOrder = c("LPDAAC", "LAADS")
  )
)


## 2022-09-05 ====

library(MODIS)

lap = "~/Documents/data/MODIS_ARC"

MODISoptions(
  localArcPath = lap
  , outDirPath = file.path(
    lap
    , "PROCESSED"
  )
)

hdfs = getHdf(
  "MOD10A1F"
  , begin = "2006.01.23"
  , end = "2006.01.23"
  , tileH = 17
  , tileV = 4
)


### cstack usage ----

gdalSDS = sprintf(
  "HDF4_EOS:EOS_GRID:\"%s\":MOD_Grid_Snow_500m:CGF_NDSI_Snow_Cover"
  , unlist(hdfs)
)

ofile = file.path(
  tempdir()
  , "MOD10A1F.A2006023.CGF_NDSI_Snow_Cover.tif"
)

options = c(
  "-of", "GTiff"
  , "-co", "compress=lzw"
  , "-co", "predictor=2"
  , "-r", "near"
  , "-srcnodata", "255"
  , "-dstnodata", "255"
  , "-overwrite"
  # , "-wo", "NUM_THREADS=4"
  # , "-multi" # raises c stack usage error if enabled and `quiet = FALSE`
)

sf::gdal_utils(
  util = "warp"
  , source = gdalSDS
  , destination = ofile
  , options = options
  , quiet = TRUE
)

# gdalwarp \
#     -of GTiff \
#     -co "compress=lwz" \
#     -co "predictor=2" \
#     -r near \
#     -overwrite \
#     -multi \
#     HDF4_EOS:EOS_GRID:"/tmp/RtmpRXIfJv/MODIS/MODIS/MOD13A1.061/2021.02.18/MOD13A1.A2021049.h21v09.061.2021068101316.hdf":MODIS_Grid_16DAY_500m_VI:"500m 16 days NDVI" \
#     /tmp/RtmpRXIfJv/MODIS/PROCESSED/mod13a1_kili/MOD13A1.A2021049.500m_16_days_NDVI.tif

## benchmarking
library(microbenchmark)

microbenchmark(
  single = {
    sf::gdal_utils(
      "warp"
      , source = src
      , destination = dst
      , options = c(
        "-of", "GTiff"
        , "-co", "compress=lzw"
        , "-co", "predictor=2"
        , "-r", "near"
        , "-srcnodata", "255"
        , "-dstnodata", "255"
        , "-overwrite"
      )
      , quiet = TRUE
    )
  }
  , multi = {
    sf::gdal_utils(
      "warp"
      , source = src
      , destination = dst
      , options = c(
        "-of", "GTiff"
        , "-co", "compress=lzw"
        , "-co", "predictor=2"
        , "-r", "near"
        , "-srcnodata", "255"
        , "-dstnodata", "255"
        , "-overwrite"
        , "-multi"
      )
      , quiet = TRUE
    )
  }
  , wo = {
    sf::gdal_utils(
      "warp"
      , source = src
      , destination = dst
      , options = c(
        "-of", "GTiff"
        , "-co", "compress=lzw"
        , "-co", "predictor=2"
        , "-r", "near"
        , "-srcnodata", "255"
        , "-dstnodata", "255"
        , "-overwrite"
        , "-wo", "NUM_THREADS=4"
      )
      , quiet = TRUE
    )
  }
  , wo_multi = {
    sf::gdal_utils(
      "warp"
      , source = src
      , destination = dst
      , options = c(
        "-of", "GTiff"
        , "-co", "compress=lzw"
        , "-co", "predictor=2"
        , "-r", "near"
        , "-srcnodata", "255"
        , "-dstnodata", "255"
        , "-overwrite"
        , "-wo", "NUM_THREADS=4"
        , "-multi"
      )
      , quiet = TRUE
    )
  }
)

# Unit: milliseconds
#     expr      min       lq     mean   median       uq      max neval cld
#   single 2.928669 4.398420 4.954476 5.216608 5.597803 7.084650   100   a
#    multi 3.077062 4.039075 4.867191 5.157830 5.614667 6.685348   100   a
#       wo 3.005221 4.065321 4.840850 5.202567 5.597503 6.773891   100   a
# wo_multi 3.100692 4.045811 4.934897 5.165353 5.696735 6.849002   100   a


## 2022-09-18 ====

### mask values doesn't work for multiple tiles and "average" resampling #70

library(MODIS)

lap = "~/Documents/data/MODIS_ARC"

MODISoptions(
  localArcPath = lap
  , outDirPath = file.path(
    lap
    , "PROCESSED"
  )
  , resamplingType = "average"
)

product = "MCD15A2H"

clc = getCollection(
  product
  , forceCheck = TRUE
)


### one tile ----

# run only one tile
fpar1 = runGdal(
  product
  , collection = clc
  , tileH = 21
  , tileV = 8
  , begin = "2003001"
  , end = "2003008"
  , SDSstring = "1"
  , job = "mcd15a2h-test_onetile"
  , maskValue = 249:255
  , overwrite = TRUE
)

rst1 = raster(unlist(fpar1))
(tbl1 = table(rst1[]))[order(as.numeric(names(tbl1)), decreasing = TRUE)][1:10]
print(plot(rst1))


### two tiles ----

fpar2 = runGdal(
  product
  , collection = clc
  , tileH = 21
  , tileV = c(7, 8)
  , begin = "2003001"
  , end = "2003008"
  , SDSstring = "1"
  , job = "mcd15a2h-test_twotile"
  , maskValue = 249:255
  , overwrite = TRUE
)

rst2 = raster(unlist(fpar2))
(tbl2 <- table(rst2[]))[order(as.numeric(names(tbl2)), decreasing = TRUE)][1:10]
