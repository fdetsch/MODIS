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