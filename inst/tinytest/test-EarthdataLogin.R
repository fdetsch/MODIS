nrc = file.path(tempdir(), ".netrc")

## if available, backup existing .netrc file
avl = file.exists(nrc)
if (avl) {
  jnk = file.rename(nrc, paste0(nrc, ".backup"))
}


## `credentials()` ====

### 1 file unavailable ----

expect_null(
  MODIS:::credentials(path = nrc)
  , info = "blank output if file is missing"
)


### 2 single-entry file ----

## with machine
machine = "urs.earthdata.nasa.gov"

writeLines(paste("machine", machine), nrc)
lns = MODIS:::credentials(path = nrc)

expect_inherits(
  lns
  , class = "list"
  , info = "output inherits from class 'list'"
)

expect_identical(
  names(lns)
  , c("machine", "login", "password")
  , info = "names of output 'list' are correct"
)

expect_identical(
  lns$machine
  , machine
  , info = "'machine' looks as expected (i.e. class 'character', content)"
)
expect_null(
  lns$login
  , info = "'login' is NULL if missing in .netrc"
)
expect_null(
  lns$password
  , info = "'password' is NULL if missing in .netrc (i)"
)

## -"- and login
login = "sad_boyd"

write(paste("login", login), nrc, append = TRUE)
lns = MODIS:::credentials(path = nrc)

expect_identical(
  lns$login
  , login
  , info = "'login' looks as expected (i.e. class `character`, content)"
)
expect_null(
  lns$password
  , info = "'password' is NULL if missing in .netrc (ii)"
)

## -"- and password
password = "Lc557Gv$"

write(paste("password", password), nrc, append = TRUE)
lns = MODIS:::credentials(path = nrc)

expect_identical(
  lns$password
  , password
  , info = "'password' looks as expected (i.e. class `character`, content)"
)


### 3 multi-entry file ----

other_creds = paste(
  "machine e4ftl01.cr.usgs.gov"
  , "login romantic_swanson"
  , "password 9yerTXd@"
  , sep = "\n"
)

write(
  paste(
    ""
    , other_creds
    , sep = "\n"
  )
  , file = nrc
  , append = TRUE
)

expect_identical(
  MODIS:::credentials(
    path = nrc
  )
  , target = lns
  , info = "credentials are extracted correctly in the presence of 2+ entries"
)


## `EarthdataLogin()` ====

write(
  other_creds
  , file = nrc
)

lns1 = EarthdataLogin(
  usr = login
  , pwd = password
  , path = nrc
)

expect_identical(
  lns1
  , target = lns
  , info = "credentials are correctly written to .netrc"
)

expect_identical(
  paste(
    readLines(
      nrc
    )[1:3]
    , collapse = "\n"
  )
  , target = other_creds
  , info = "other credentials remain untouched when updating .netrc"
)


### checkEarthdataLogin() ----

expect_error(
  MODIS:::checkEarthdataLogin(
    server = "LADS"
  )
  , pattern = "'arg' should be one of .*LPDAAC.*LAADS"
)

expect_warning(
  out2.0 <- MODIS:::checkEarthdataLogin(
    path = nrc
  )
  , pattern = "Authentication failed with\n> HTTP error (401|504)"
) # 504 = Gateway Timeout, i.e. server not reachable

expect_false(
  out2.0
)

## early exit: single quotes in password
lns2 = readLines(
  nrc
)

write(
  gsub("557", "5'7", lns2)
  , file = nrc
)

expect_error(
  MODIS:::downloadFile(
    method = "wget"
    , path = nrc
  )
  , pattern = "Earthdata passwords .* must not contain single quotes"
)

expect_warning(
  out2.1 <- MODIS:::checkEarthdataLogin(
    method = "wget"
    , path = nrc
  )
  , pattern = "Authentication failed.*must not contain single quotes"
)

expect_false(
  out2.1
)

## delete temporary .netrc file
jnk = file.remove(
  nrc
)

## if applicable, restore previous .netrc file
if (avl) {
  jnk = file.rename(paste0(nrc, ".backup"), nrc)
}
