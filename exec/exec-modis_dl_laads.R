library(MODIS)

MODISoptions(
  MODISserverOrder = c(
    "LAADS"
    , "LPDAAC"
  )
  , dlmethod = "wget"
)

product = "MOD11A1"

clc = getCollection(
  product
  , forceCheck = TRUE
)

hdfs = getHdf(
  product
  , collection = clc
  , tileH = 21
  , tileV = 9
  , begin = "2021-08-01"
  , end = "2021-08-05"
)
