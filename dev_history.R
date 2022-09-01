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