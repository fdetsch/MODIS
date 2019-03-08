context("EarthdataLogin")

nrc = "~/.netrc"
if (file.exists(nrc)) {
  on.exit(jnk <- file.rename(paste0(nrc, ".backup"), nrc))
  jnk = file.rename(nrc, paste0(nrc, ".backup"))
}

test_that("blank output if file is missing", {
  expect_null(credentials())
})

test_that("'login', 'password' is NULL if missing in ~/.netrc", {
  writeLines("machine urs.earthdata.nasa.gov", nrc)
  lns = credentials()
  expect_null(lns$login); expect_null(lns$password)
})

test_that("'password' is NULL if missing in ~/.netrc", {
  writeLines(c("machine urs.earthdata.nasa.gov", "login some_usr"), nrc)
  lns = credentials()
  expect_null(lns$password)
})

test_that("credentials from ~/.netrc are correctly imported", {
  writeLines(c("machine urs.earthdata.nasa.gov", "login some_usr", "password some_pwd"), nrc)
  lns = credentials()
  expect_is(lns, "list")
  expect_identical(names(lns), c("machine", "login", "password"))
  expect_identical(lns$machine, "urs.earthdata.nasa.gov")
  expect_identical(lns$login, "some_usr")
  expect_identical(lns$password, "some_pwd")
})
