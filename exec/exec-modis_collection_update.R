## ENVIRONMENT ====

### pkgs ----

library(MODIS)
library(data.table)


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

h = curl::new_handle(
  connecttimeout = 10L
)

lpdaac = Map(
  \(platform, prefix) {
    con = curl::curl(
      file.path(
        "https://e4ftl01.cr.usgs.gov"
        , platform
      )
      , open = "r"
      , handle = h
    )
    on.exit(
      close(con)
    )
    
    lns = readLines(
      con
    )
    
    data.table(
      server = "LPDAAC"
      , platform = platform
      , product = regmatches(
        lns
        , regexpr(
          paste0(prefix, "[0-9A-Z\\._]+")
          , lns
        )
      )
    )
  }
  , c("MOLT", "MOLA", "MOTA")
  , c("MOD", "MYD", "MCD")
)

## split product into product name, collection
lpdaac = lpdaac |> 
  rbindlist() |> 
  transform(
    collection = regmatches(
      product
      , regexpr(
        "\\d{3}$"
        , product
      )
    )
    , product = gsub(
      "\\.\\d{3}$"
      , ""
      , product
    )
  )


### laads ----

laads = Map(
  \(clc) {
    lns = fread(
      sprintf(
        "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/%s.csv"
        , as.integer(clc)
      )
    )
    
    mrg = merge(
      lns
      , lpdaac[
        collection == clc
      ]
      , by.x = "name"
      , by.y = "product"
      , sort = FALSE
    )
    
    data.table(
      server = "LAADS"
      , mrg[
        , c("platform", "name", "collection")
      ]
    ) |> 
      setnames(
        old = "name"
        , new = "product"
      )
  }
  , lpdaac[
    , collection |> 
      unique()
  ]
)

laads = laads |> 
  rbindlist()


### nsidc ----

crd = MODIS:::credentials()
usr = crd$login; pwd = crd$password

curl::handle_setopt(
  handle = h,
  httpauth = 1,
  userpwd = paste0(usr, ":", pwd)
)

nsidc = Map(
  \(platform, prefix) {
    con = curl::curl(
      file.path(
        "https://n5eil01u.ecs.nsidc.org"
        , platform
      )
      , open = "r"
      , handle = h
    )
    on.exit(
      close(con)
    )
    
    lns = readLines(
      con
    )
    
    data.table(
      server = "NSIDC"
      , platform = platform
      , product = regmatches(
        lns
        , regexpr(
          paste0(prefix, "[0-9A-Z\\._]+")
          , lns
        )
      )
    )
  }
  , c("MOST", "MOSA")
  , c("MOD", "MYD")
)

## split product into product name, collection
nsidc = nsidc |> 
  rbindlist() |> 
  transform(
    collection = regmatches(
      product
      , regexpr(
        "\\d{3}$"
        , product
      )
    )
    , product = gsub(
      "\\.\\d{3}$"
      , ""
      , product
    )
  )


### collections update ----

clc = do.call(
  rbind
  , list(
    lpdaac
    , laads
    , nsidc
  )
)

## split by product
tmp = clc[
  order(
    product
    , collection
  )
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


## PRODUCTS ====

MODIS:::MODIS_Products

# TODO:
# * omit sensor, pf3 from `MODIS:::MODIS_Products`
# * omit ntsg from `MODIS:::MODIS_FTPinfo`