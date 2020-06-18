nrc = file.path(tempdir(), ".netrc")
avl = file.exists(nrc)
if (avl) {
  jnk = file.rename(nrc, paste0(nrc, ".backup"))
}

expect_null(
  MODIS:::credentials(path = nrc)
  , info = "blank output if file is missing"
)

writeLines("machine urs.earthdata.nasa.gov", nrc)
lns = MODIS:::credentials(path = nrc)
expect_null(lns$login, info = "'login' is NULL if missing in .netrc")
expect_null(lns$password, info = "'password' is NULL if missing in .netrc (i)")

writeLines(c("machine urs.earthdata.nasa.gov", "login some_usr"), nrc)
lns = MODIS:::credentials(path = nrc)
expect_null(lns$password, info = "'password' is NULL if missing in .netrc (ii)")

## credentials from .netrc are correctly imported
writeLines(c("machine urs.earthdata.nasa.gov", "login some_usr", "password some_pwd"), nrc)
lns = MODIS:::credentials(path = nrc)
expect_true(
  all(
    inherits(lns, "list")
    , identical(names(lns), c("machine", "login", "password"))
    , lns$machine == "urs.earthdata.nasa.gov"
    , lns$login == "some_usr"
    , lns$password == "some_pwd"
  )
  , info = "credentials from .netrc are correctly imported"
)

if (avl) {
  jnk = file.rename(paste0(nrc, ".backup"), nrc)
}
