## ENVIRONMENT ====

### pkgs ----

library(MODIS)
library(data.table)


### funs ----

source("exec/exec-utils.R")


### `MODISoptions()` ----

## local arc path
lap = "~/Documents/data/MODIS_ARC"
MODISoptions(
  localArcPath = lap
  , outDirPath = file.path(lap, "PROCESSED")
)


### global objects ----

# ## product, collection
# product = "MOD11A1"
# collection = getCollection(
#   product
#   , forceCheck = TRUE
# )
# 
# ## server base urls
# MODIS:::genString(
#   product
#   , collection = collection
# )


## COLLECTIONS ====

### lpdaac ----

lpdaac = listLPDAACProducts()


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


### nsidc ----

nsidc = listNSIDCProducts()


### `MODIScollection` ----

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
collections = matrix(
  ncol = length(tmp)
  , nrow = max(lns)
  , dimnames = list(NULL, names(tmp))
)

for (i in 1:length(tmp)) {
  collections[1:lns[i], i] = tmp[[i]]
}

data.frame(
  collections
)


### `MODIS_Products` ====

## template
products = vector(
  "list"
  , length = length(MODIS:::MODIS_Products)
) |> 
  stats::setNames(
    names(MODIS:::MODIS_Products)
  )

## sensor
products$SENSOR = rep(
  "MODIS"
  , ncol(collections)
)

## product
products$PRODUCT = colnames(
  collections
)

## platform
platforms = clc[
  , unique(.SD)
  , .SD = c("platform", "product")
]$platform

products$PLATFORM = sapply(
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

## server path extensions
svs = c("LPDAAC", "LAADS", "NSIDC")
pfs = paste0("PF", c(1:2, 4))

for (i in 1:3) {
  sbs = clc[
    server == svs[i]
    , unique(.SD)
    , .SD = c("platform", "product")
  ]
  
  products[[pfs[i]]] = sbs$platform[
    match(
      colnames(collections)
      , sbs$product
    )
  ]
}

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
  )
  , by = `Short Name`
][
  , -"Collection"
]

if (any(!colnames(collections) %in% ods$`Short Name`)) {
  stop("Unmatched products encountered.")
}

products$TOPIC = merge(
  data.table(
    "Short Name" = colnames(
      collections
    )
  )
  , ods[
    , c("Short Name", "Keyword")
  ]
  , sort = FALSE
)$Keyword

## type
lpdaac_types = getLPDAACProductTypes(
  clc[
    server == "LPDAAC"
    , list(
      collection = collection[.N]
    )
    , by = product
  ]
)

nsidc_types = getNSIDCProductTypes(
  clc[
    server == "NSIDC"
    , list(
      collection = collection[.N]
    )
    , by = product
  ]
)

types = rbind(
  lpdaac_types[, c("product", "type")]
  , nsidc_types[, c("product", "type")]
)

products$TYPE = merge(
  data.table::data.table(
    product = products$PRODUCT
  )
  , types
  , by = "product"
  , sort = FALSE
)$type

## res
unique(ods$`Spatial Resolution`)

## temp_res

## internalseparator
products$INTERNALSEPARATOR = rep(
  "\\."
  , ncol(collections)
)

## source
products$SOURCE = clc[
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
  products[[pos[i]]] = nchar(products$PRODUCT) + dfs[i]
}


# TODO:
# * omit sensor, pf3, possibly internalseparator from `MODIS:::MODIS_Products`
# * omit ntsg from `MODIS:::MODIS_FTPinfo`
# * investigate "500.1 m" entry in `MODIS:::MODIS_Products$RES`
