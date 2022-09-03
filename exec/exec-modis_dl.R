library(MODIS)
library(sf)


## ENVIRONMENT ====

### `MODISoptions()` ----

lap = file.path(
  tempdir()
  , "MODIS"
)

MODISoptions(
  localArcPath = lap
  , outDirPath = file.path(
    lap
    , "PROCESSED"
  )
  , quiet = FALSE
  , save = FALSE
)


### aoi ----

kibo = data.frame(
  y = -3.065053
  , x = 37.359031
) |> 
  st_as_sf(
    crs = 4326
    , coords = c("x", "y")
  )

kili = st_buffer(
  kibo
  , dist = 5e4
) |> 
  st_bbox() |> 
  st_as_sfc()

# mapview::mapview(
#   stars::st_as_stars(
#     kili
#   )
#   , col.regions = "cornflowerblue"
#   , alpha.regions = 0.4
#   , map.types = mapviewGetOption(
#     "basemaps"
#   )[
#     c(4, 1:3, 5)
#   ]
#   , legend = FALSE
# )


### download ----

## lpdaac
product = "MOD13A1"

clc = getCollection(
  product
  , forceCheck = TRUE
)

tifs = runGdal(
  product
  , collection = clc
  , extent = kili
  , begin = "2021-03-01"
  , end = "2021-05-31"
  , SDSstring = "1"
  , job = "mod13a1_kili"
  , MODISserverOrder = "LPDAAC"
  , quiet = FALSE
)



## LAADS
hdfs1 = getHdf(
  product
  , collection = clc
  , tileH = 21
  , tileV = 9
  , begin = "2020-08-01"
  , end = "2020-08-03"
  , MODISserverOrder = "LAADS"
  , quiet = FALSE
)

product2 = "MOD10A1"

clc2 = getCollection(
  product2
  , forceCheck = TRUE
)

hdfs2 = getHdf(
  product2
  , collection = clc2
  , tileH = 21
  , tileV = 9
  , begin = "2020-08-01"
  , end = "2020-08-03"
  , quiet = FALSE
)
