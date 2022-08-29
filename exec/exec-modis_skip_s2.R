## ENVIRONMENT ====

### pkgs ----

library(MODIS)


### global objects ----

product = "MOD11A1"

## get newest collection for product
clc = getCollection(
  product
  , forceCheck = TRUE
)

## set job name
job = sprintf(
  "%s_%s"
  , Sys.Date()
  , product
)


## PROCESSING ====

### `sf` input with ellipsoidal coordinates ----

## background:
## in `getTile,sf-method`, `sf::st_filter(sr, x)` would fail due to invalid 
## geometries in the input file and built-in `MODIS:::sr` data set (see also 
## https://github.com/fdetsch/MODIS/issues/110). with `sf::sf_use_s2` 
## deactivated, this operation succeeds.

## aoi
dsn = system.file(
  "vectors/Up.tab"
  , package = "rgdal"
)

shp = sf::st_read(
  dsn
  , layer = "Up"
  , quiet = TRUE
)

tls = getTile(
  shp
)

## download and extract data
tfs1 = runGdal(
  product
  , extent = tls
  , begin = "2021-01-01"
  , end = "2021-01-04"
  , SDSstring = "1" # 'LST_Day_1km'
  , job = job
)

## investigate
(
  out1 = tfs1 |> 
    unlist() |> 
    stack()
)


### `raster` input (uses non-valid `sr` built-in object) ----

## background:
## in `getTile,Raster-method`, `sf::st_crop(sr, x)` would fail due to invalid 
## geometries in the built-in `MODIS:::sr` data set (see also
## https://github.com/fdetsch/MODIS/issues/110). with `sf::sf_use_s2` 
## deactivated, this operation succeeds.

## aoi
rst = raster(
  xmn = 9.2
  , xmx = 17.47
  , ymn = 46.12
  , ymx = 49.3
)

## download and extract data
tfs2 = runGdal(
  product
  , extent = rst
  , begin = "2021-07-01"
  , end = "2021-07-04"
  , SDSstring = "1" # 'LST_Day_1km'
  , job = job
)

## investigate
(
  out2 = tfs2 |> 
    unlist() |> 
    stack() |> 
    plot()
)
