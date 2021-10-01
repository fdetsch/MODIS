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
  connecttimeout = 60L
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
    
    lns = suppressWarnings(
      readLines(
        con
      )
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
  ) |> 
  subset(
    !product %in% c(
      "NSIDC-0321"
      , "NSIDC-0447"
      , "MODGRNLD"
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


## PRODUCTS ====

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
  , -"Collection"
] |> 
  unique()

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
MODIS:::MODIS_Products$TYPE

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
