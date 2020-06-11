checkIntegrity = function(x, ...) {
  
  out = rep(NA, length(x))
  
  for (i in seq_along(x)) {
    bsn = basename(x[i])
    if (is.na(bsn) || bsn == "NA") {
      next
    } else {
      if (dirname(x[i]) == ".") {
        x[i] = paste0(
          genString(
            x = x[i]
            , remote = FALSE
            , collection = getCollection(x[i], quiet = TRUE)
          )$localPath
          , bsn
        )
      }
      
      if (file.exists(x[i])) {
        out[i] = sf::gdal_utils(
          source = correctPath(x[i], isFile = TRUE)
          , quiet = TRUE
        ) != ""
      }
    }
  }
  
  return(out)
}
