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

## product, collection
product = "MOD11A1"
collection = getCollection(
  product
  , forceCheck = TRUE
)

# ## server base urls
# MODIS:::genString(
#   product
#   , collection = collection
# )


## COLLECTIONS ====

### lpdaac ----

lpdaac = Map(
  \(platform, prefix) {
    con = curl::curl(
      file.path(
        "https://e4ftl01.cr.usgs.gov"
        , platform
      )
      , open = "r"
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

