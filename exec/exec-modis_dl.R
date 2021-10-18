library(MODIS)

product = "MOD11A1"

clc = getCollection(
  product
)

hdfs = getHdf(
  product
  , collection = clc
  , tileH = 21
  , tileV = 9
  , begin = "2021-08-01"
  , end = "2021-08-05"
  , MODISserverOrder = "LPDAAC"
  , quiet = FALSE
)

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

hdfs2 = getHdf(
  "MOD10A1"
  , collection = clc
  , tileH = 21
  , tileV = 9
  , begin = "2020-08-01"
  , end = "2020-08-03"
  , quiet = FALSE
)
