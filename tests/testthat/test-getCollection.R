context("getCollection")

# getCollection("MOD17A3.055")
# getCollection("MCD12C1.006")

test_that("wrong collection creates console output", {
  expect_output(getCollection("MCD12Q1.051"), "not available in collection")
})

