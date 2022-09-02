## ENVIRONMENT ====

rm(list = ls(all = TRUE))


### sysdata ----
load(
  "R/sysdata.rda"
)

sysdata = ls()


### pkgs ----

library(data.table)


### funs ----

source("exec/exec-utils.R")


## WEB SCRAPING ====

### lpdaac ----

lpdaac = listLPDAACProducts()

## get product metadata
lpdaac_nfo = getLPDAACProductTypes(
  lpdaac[
    , list(
      collection = collection[.N]
    )
    , by = product
  ]
)


### laads ----

laads = listLAADSProducts(
  collections = lpdaac[
    , unique(collection)
  ]
)

## append platform info
laads = merge(
  lpdaac[
    , !"server"
  ]
  , laads
  , by = c("product", "collection")
  , all.x = TRUE
  , sort = FALSE
) |> 
  na.omit()

laads = rejectLAADSEmptyProducts(
  laads
)


### nsidc ----

nsidc = listNSIDCProducts()

## get product metadata
nsidc_nfo = getNSIDCProductTypes(
  nsidc[
    , list(
      collection = collection[.N]
    )
    , by = product
  ]
)

## rbind
clc = do.call(
  rbind
  , list(
    lpdaac
    , laads
    , nsidc
  )
)[
  order(
    product
    , collection
  )
] |> 
  subset(
    !grepl("M(O|Y)D28", product)
  )


### swath discard ----

nfo = rbind(
  lpdaac_nfo[, c("product", "type")]
  , nsidc_nfo[, c("product", "type")]
)

swaths = nfo[
  type == "Swath"
  , product
]

clc = clc[
  !product %in% swaths
]


## BUILT-IN DATA ====

### `MODIScollection` ----

## split by product
tmp = clc[
  , unique(.SD)
  , .SD = c("product", "collection")
] |> 
  transform(
    collection = as.integer(
      collection
    )
  ) |> 
  (
    \(x) {
      split(
        as.integer(x$collection)
        , f = x$product
      )
    }
  )() 

## write per-product collections to matrix
lns = lengths(tmp)
MODIScollection = data.frame(
  matrix(
    ncol = length(tmp)
    , nrow = max(lns)
    , dimnames = list(NULL, names(tmp))
  )
)

for (i in 1:length(tmp)) {
  MODIScollection[1:lns[i], i] = tmp[[i]]
}


### `MODIS_Products` ====

## template
MODIS_Products = vector(
  "list"
  , length = 15
) |> 
  stats::setNames(
    c(
      "SENSOR", "PRODUCT", "PLATFORM"
      , "PF1", "PF2", "PF3", "PF4"
      , "TOPIC", "TYPE", "RES", "TEMP_RES"
      , "INTERNALSEPARATOR", "SOURCE", "POS1", "POS2"
    )
  )

## sensor
MODIS_Products$SENSOR = rep(
  "MODIS"
  , ncol(MODIScollection)
)

## product
MODIS_Products$PRODUCT = colnames(
  MODIScollection
)

## platform
platforms = clc[
  , unique(.SD)
  , .SD = c("platform", "product")
]$platform

MODIS_Products$PLATFORM = sapply(
  platforms
  , \(platform) {
    switch(
      platform
      , "MOTA" = "Combined"
      , "MOLT" =
        , "MOST" = "Terra"
      , "MOLA" =
        , "MOSA" = "Aqua"
    )
  }
  , USE.NAMES = FALSE
)

## server path features
svs = c("LPDAAC", "NSIDC")
pfs = paste0("PF", c(1, 4))

for (i in 1:2) {
  sbs = clc[
    server == svs[i]
    , unique(.SD)
    , .SD = c("platform", "product")
  ]
  
  MODIS_Products[[pfs[i]]] = sbs$platform[
    match(
      colnames(MODIScollection)
      , sbs$product
    )
  ]
}

MODIS_Products$PF2 = regmatches(
  MODIS_Products$PRODUCT
  , regexpr(
    "^(MOD|MYD|MCD)"
    , MODIS_Products$PRODUCT
  )
)

MODIS_Products$PF3 = rep(
  NA_character_
  , ncol(MODIScollection)
)

## topic

# * nsidc: https://nsidc.org/data/modis/data_summaries
# * lpdaac: https://lpdaac.usgs.gov/product_search/?collections=Combined+MODIS&\
#           collections=Terra+MODIS&collections=Aqua+MODIS&view=list

ods_fl = "inst/external/products.ods"

sheets = readODS::list_ods_sheets(
  ods_fl
)

ods = lapply(
  sheets
  , \(sheet) {
    readODS::read_ods(
      ods_fl
      , sheet = sheet
    )
  }
) |> 
  rbindlist()

ods = ods[
  , c("Short Name", "Collection") := tstrsplit(
    `Short Name`
    , "."
    , fixed = TRUE
  )
][
  order(Collection)
  , list(
    Collection = Collection[.N]
    , Keyword = Keyword[.N]
    , `Spatial Resolution` = `Spatial Resolution`[.N]
    , `Temporal Resolution` = `Temporal Resolution`[.N]
  )
  , by = `Short Name`
][
  , -"Collection"
]

if (any(!colnames(MODIScollection) %in% ods$`Short Name`)) {
  stop("Unmatched products encountered.")
}

MODIS_Products$TOPIC = merge(
  data.table(
    "Short Name" = colnames(
      MODIScollection
    )
  )
  , ods[
    , c("Short Name", "Keyword")
  ]
  , sort = FALSE
)$Keyword

## type
MODIS_Products$TYPE = merge(
  data.table::data.table(
    product = MODIS_Products$PRODUCT
  )
  , nfo
  , by = "product"
  , sort = FALSE
)$type

## res
MODIS_Products$RES = merge(
  data.table(
    "Short Name" = colnames(
      MODIScollection
    )
  )
  , ods[
    , c("Short Name", "Spatial Resolution")
  ]
  , sort = FALSE
)$`Spatial Resolution`

## temp_res
MODIS_Products$TEMP_RES = merge(
  data.table(
    "Short Name" = colnames(
      MODIScollection
    )
  )
  , ods[
    , c("Short Name", "Temporal Resolution")
  ]
  , sort = FALSE
)$`Temporal Resolution`

## internalseparator
MODIS_Products$INTERNALSEPARATOR = rep(
  "\\."
  , ncol(MODIScollection)
)

## source
MODIS_Products$SOURCE = clc[
  , unique(.SD)
  , .SDcols = c("product", "server")
] |> 
  (\(x) split(x, x$product))() |> 
  unname() |> 
  lapply(
    "[["
    , "server"
  )

## pos1, pos2
pos = paste0("POS", 1:2)
dfs = c(3L, 9L)

for (i in 1:2) {
  MODIS_Products[[pos[i]]] = nchar(MODIS_Products$PRODUCT) + dfs[i]
}


### sysdata ----

# waldo::compare(
#   MODIS:::MODIS_Products # old
#   , MODIS_Products # new
# )
# 
# waldo::compare(
#   MODIS:::MODIScollection
#   , MODIScollectiony
# )

do.call(
  usethis::use_data
  , c(
    lapply(
      sysdata
      , as.name
    )
    , internal = TRUE
    , overwrite = TRUE
  )
)

# TODO:
# * omit sensor, pf3, possibly internalseparator from `MODIS:::MODIS_Products`
# * omit ntsg from `MODIS:::MODIS_FTPinfo`
