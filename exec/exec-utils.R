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
    rbindlist(
      idcol = "collection"
    )
}


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
  nsidc = clusterMap(
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