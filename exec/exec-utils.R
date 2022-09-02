## LPDAAC ====

#' @param connecttimeout Connection timeout passed to [curl::new_handle].
#' @param names # of cores passed to [parallel:makePSOCKcluster].

listLPDAACProducts = function(
  connecttimeout = 60L
  , names = min(
    3L
    , parallel::detectCores() - 1L
  )
) {
  
  ## initialize parallel backend
  cl = parallel::makePSOCKcluster(
    names
  )
  on.exit(
    parallel::stopCluster(
      cl
    )
  )
  
  parallel::clusterExport(
    cl
    , "connecttimeout"
    , envir = environment()
  )
  
  ## cycle through platforms, prefixes
  lpdaac = parallel::clusterMap(
    cl
    , \(platform, prefix) {
      
      # initialize curl connection
      con = curl::curl(
        file.path(
          "https://e4ftl01.cr.usgs.gov"
          , platform
        )
        , open = "r"
        , handle = curl::new_handle(
          connecttimeout = connecttimeout
        )
      )
      on.exit(
        close(con)
      )
      
      # scrape web content
      lns = readLines(
        con
      )
      
      data.table::data.table(
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
  lpdaac |> 
    data.table::rbindlist() |> 
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
}


#' @param x A 2-column `data.table` with product and corresponding collection.

getLPDAACProductTypes = function(
  x
  , names = min(
    nrow(x)
    , parallel::detectCores() - 1L
  )
) {
  
  ## initialize parallel backend
  cl = parallel::makePSOCKcluster(
    names
  )
  on.exit(
    parallel::stopCluster(
      cl
    )
  )
  
  ## construct server path suffixes
  sfxs = x[
    , paste(
      tolower(product)
      , collection
      , sep = "v"
    )
  ]
  
  ## define relevant metadata fields
  flds = c(
    "Coordinate System"
    , "Columns/Rows"
    , "Pixel Size"
  )
  
  parallel::clusterExport(
    cl
    , "flds"
    , envir = environment()
  )
  
  ## cycle through suffixes
  lst = parallel::clusterMap(
    cl
    , \(sfx) {
      
      # initialize curl connection
      con = curl::curl(
        file.path(
          "https://lpdaac.usgs.gov/products"
          , sfx
        )
      )
      on.exit(
        close(con)
      )
      
      # scrape web content
      lns = readLines(con)
      
      out = vector(
        "list"
        , length = length(flds)
      )
      
      for (i in 1:length(flds)) {
        cnt = grep(
          flds[i]
          , lns
          , value = TRUE
        )
        
        tmp = strsplit(
          gsub(
            "^\\s*"
            , ""
            , cnt
          )
          , "</*td>|</*tr>"
        )[[1]]
        
        out[[i]] = tmp[
          nzchar(tmp)
        ]
      }
      
      do.call(
        rbind
        , out
      ) |> 
        data.frame()
    }
    , sfxs
  )
  
  ## long --> wide
  dat = data.table::rbindlist(
    lst
    , idcol = "product"
  ) |> 
    data.table::setnames(
      old = c("X1", "X2")
      , new = c("variable", "value")
    ) |> 
    data.table::dcast(
      product ~ variable
    )
  
  ## append type
  dat[
    , `:=`(
      product = strsplit(
        product
        , "v"
      ) |> 
        sapply(
          "[["
          , 1
        ) |> 
        toupper()
      , type = data.table::fcase(
        # cmg
        (
          grepl("7200", `Columns/Rows`) & 
            `Pixel Size` == "5600 m"
        ) | (
          grepl("43200", `Columns/Rows`) & 
            grepl("~?1000 m", `Pixel Size`)
        )
        , "CMG"
        # swath
        , grepl("Swath", `Coordinate System`)
        , "Swath"
        , default = "Tile"
      )
    )
  ]
  
  merge(
    x
    , dat
    , by = "product"
    , sort = FALSE
  )
}


## LAADS ====

#' @param collections Collections to be processed, typically obtained through 
#'   `listLPDAACProducts()[, unique(collection)]`.

listLAADSProducts = function(
  collections
  , names = min(
    length(collections)
    , parallel::detectCores() - 1L
  )
) {
  
  ## initialize parallel backend
  cl = parallel::makePSOCKcluster(
    names
  )
  on.exit(
    parallel::stopCluster(
      cl
    )
  )
  
  ## cycle through collections
  laads = parallel::clusterMap(
    cl
    , \(clc) {
      
      # scrape web content
      lns = data.table::fread(
        sprintf(
          "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/%s.csv"
          , as.integer(clc)
        )
      )
      
      data.table::data.table(
        server = "LAADS"
        , product = lns[
          , name
        ]
      )
    }
    , collections
  )
  
  ## append collection info
  laads |> 
    data.table::rbindlist(
      idcol = "collection"
    )
}


rejectLAADSEmptyProducts = function(
  x
) {
  
  ## cycle through collection-product combos
  inputs = sprintf(
    "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/%s/%s.csv"
    , as.integer(x$collection)
    , x$product
  )
  
  idx = sapply(
    inputs
    , \(input) {
      
      # scrape web content
      data.table::fread(
        input
      ) |> 
        nrow()
    }
  ) >= 1L
  
  x[
    idx
  ]
}


## NSIDC ====

listNSIDCProducts = function(
  connecttimeout = 60L
  , names = min(
    2L
    , parallel::detectCores() - 1L
  )
) {
  
  crd = MODIS:::credentials()
  usr = crd$login; pwd = crd$password
  
  ## initialize parallel backend
  cl = parallel::makePSOCKcluster(
    names
  )
  on.exit(
    parallel::stopCluster(
      cl
    )
  )
  
  parallel::clusterExport(
    cl
    , c("connecttimeout", "usr", "pwd")
    , envir = environment()
  )
  
  ## cycle through platforms, prefixes
  nsidc = parallel::clusterMap(
    cl
    , \(platform, prefix) {
      
      # initialize curl connection
      h = curl::new_handle(
        connecttimeout = connecttimeout
      ) |> 
        curl::handle_setopt(
        httpauth = 1
        , userpwd = paste0(usr, ":", pwd)
      )
      
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
      
      # scrape web content
      lns = suppressWarnings(
        readLines(
          con
        )
      )
      
      data.table::data.table(
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
  nsidc |> 
    data.table::rbindlist() |> 
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
}


getNSIDCProductTypes = function(
  x
  , names = min(
    nrow(x)
    , names = parallel::detectCores() - 1L
  )
) {
  
  ## initialize parallel backend
  cl = parallel::makePSOCKcluster(
    names
  )
  on.exit(
    parallel::stopCluster(
      cl
    )
  )
  
  ## construct server path suffixes
  sfxs = x[
    , tolower(
      product
    )
  ]
  
  ## define relevant metadata fields
  flds = c(
    "Spatial Coverage"
    , "Spatial Resolution"
    , "Temporal Resolution"
  )
  parallel::clusterExport(
    cl
    , "flds"
    , envir = environment()
  )
  jnk = parallel::clusterEvalQ(
    cl
    , {
      source("exec/exec-utils.R")
    }
  )
  
  ## cycle through suffixes
  lst = parallel::clusterMap(
    cl
    , \(sfx) {
      
      # initialize curl connection
      con = curl::curl(
        file.path(
          "https://nsidc.org/data"
          , sfx
        )
      )
      on.exit(
        close(con)
      )
      
      # scrape web content
      lns = readLines(con)
      
      ids_fl = grep(
        "field__label"
        , lns
      )
      
      ids_fi = grep(
        "field__item"
        , lns
      )
      
      # extract required information
      out = vector(
        "list"
        , length = length(flds)
      )
      
      for (i in 1:length(flds)) {
        out[[i]] = if (flds[i] == "Spatial Coverage") {
          getNSIDCProductSpatialCoverage(
            lns
            , field_labels = ids_fl
            , field_items = ids_fi
          )
        } else if (flds[i] == "Spatial Resolution") {
          getNSIDCProductSpatialResolution(
            lns
            , field_items = ids_fi
          )
        } else if (flds[i] == "Temporal Resolution") {
          getNSIDCProductTemporalResolution(
            lns
          )
        }
      }
      
      do.call(
        rbind
        , out
      ) |> 
        data.frame()
    }
    , sfxs
  )
  
  ## long --> wide
  dat = data.table::rbindlist(
    lst
    , idcol = "product"
  ) |> 
    data.table::setnames(
      old = c("X1", "X2")
      , new = c("variable", "value")
    ) |> 
    data.table::dcast(
      product ~ variable
    )
  
  ## append type
  dat[
    , `:=`(
      product = toupper(
        product
      )
      , type = data.table::fcase(
        # cmg
        grepl(
          "0.05 deg"
          , `Spatial Resolution`
          , ignore.case = TRUE
        )
        , "CMG"
        # swath
        , `Temporal Resolution` == "5 minute"
        , "Swath"
        , default = "Tile"
      )
    )
  ]
  
  merge(
    x
    , dat
    , by = "product"
    , sort = FALSE
  )
}


getNSIDCProductSpatialCoverage = function(
  x
  , field_labels
  , field_items
) {
  
  id = grep(
    "Spatial Coverage"
    , x
  )
  
  ## find relevant field labels (i.e. N, S, E, W)
  ids_fl1 = field_labels[
    field_labels > id
  ][
    1:4
  ]
  
  ## find relevant field items (i.e. coordinates)
  ids_fi1 = field_items[
    field_items > id
  ][
    1:4
  ]
  
  ids = c(
    ids_fl1
    , ids_fi1
  ) |> 
    sort()
  
  ## extract values
  cnt = x[
    ids
  ]
  
  xtr = regmatches(
    cnt
    , m = regexpr(
      pattern = ">[NSEW0-9:\\.-]+<?"
      , text = cnt
    )
  ) |> 
    (
      \(x) gsub(">|:?<", "", x)
    )()
  
  ## create output
  args = c(
    list(
      "%s %s, %s %s, %s %s, %s %s"
    )
    , as.list(xtr)
  )
  
  c(
    "Spatial Coverage"
    , do.call(
      sprintf
      , args = args
    )
  )
}


getNSIDCProductSpatialResolution = function(
  x
  , field_items
) {
  
  id = grep(
    "Spatial Resolution"
    , x
  )
  
  ## find relevant field items (i.e. coordinates)
  ids = field_items[
    field_items > id
  ][
    1:2
  ]
  
  ## extract values
  cnt = x[
    ids
  ]
  
  xtr = regmatches(
    cnt
    , m = regexpr(
      pattern = ">[A-z0-9\\. ]+<?"
      , text = cnt
    )
  ) |> 
    (
      \(x) gsub(">|<", "", x)
    )() |> 
    tolower()
  
  c(
    "Spatial Resolution"
    , paste(
      xtr
      , collapse = ", "
    )
  )
}


getNSIDCProductTemporalResolution = function(
  x
) {
  
  ## extract values
  cnt = grep(
    "Temporal Resolution"
    , x
    , value = TRUE
  )
  
  xtr = regmatches(
    cnt
    , m = regexpr(
      pattern = ">[0-9]{1}[A-z0-9 ]+<"
      , text = cnt
    )
  ) |> 
    (
      \(x) gsub(">|<", "", x)
    )()
  
  c(
    "Temporal Resolution"
    , xtr
  )
}
