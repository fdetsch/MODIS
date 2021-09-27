clc = MODIS:::MODIScollection

## class
expect_inherits(
  clc
  , class = "data.frame"
  , info = "built-in collections data inherits from class 'data.frame'"
)

## `nrow()`
expect_identical(
  nrow(clc)
  , target = max(
    apply(
      clc
      , MARGIN = 2
      , FUN = function(x) {
        sum(!is.na(x))
      }
    )
  )
  , info = "`nrow()` of collections data equals product with most collections"
)

## `ncol()`
expect_identical(
  ncol(clc)
  , target = nrow(MODIS::getProduct())
  , info = "`ncol()` of collections data equals # of products in `getProduct()`"
)
