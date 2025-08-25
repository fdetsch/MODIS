## PRODUCT INPUT ====

### available ----

expect_stdout(
  res1 <- getProduct(
    "MCD12Q1.006"
  )
  , pattern = "^MCD12Q1 the .* product from .* with a ground resolution of"
  , info = "available products trigger expected message"
)

expect_inherits(
  res1
  , class = "MODISproduct"
  , info = "output inherits from class 'MODISproduct'"
)

## multiple
txt = utils::capture.output(
  res1.1 <- getProduct("M*D15A2H")
)

lns = sapply(
  slotNames(res1.1)[-1]
  , function(i) {
    length(slot(res1.1, i))
  }
)

expect_identical(
  unique(lns)
  , target = length(txt)
  , info = "slot lengths are identical to the # of matching products"
)


### unavailable ----

expect_stdout(
  res2 <- getProduct(
    "MOD10_L2"
  )
  , pattern = "No product found with the name"
  , info = "unavailable products trigger expected message"
)

expect_null(
  res2
  , info = "`NULL` output is returned in case product is not available"
)


## FILE INPUT ====

fl = system.file(
  "external/MOD13A2.A2016145.h18v04.006.2016166145124.hdf"
  , package = "MODIS"
)

expect_inherits(
  res3 <- getProduct(fl)
  , class = "MODISfile"
  , info = "output inherits from class 'MODISfile' if request is a file"
)

nfo = strsplit(
  basename(fl)
  , "\\."
)[[1L]]

## investigate collection slot
expect_equivalent(
  res3@CCC
  , target = nfo[4L]
  , info = "'CCC' content is the collection of the requested file"
)

expect_identical(
  names(res3@CCC)
  , target = nfo[1L]
  , info = "'CCC' content is named according to product"
)
