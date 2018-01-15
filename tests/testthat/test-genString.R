context("genString")

test_that("genString() throws a warning for multiple products", {
  expect_warning(genString(c("MOD15A2", "MYD15A2")))
  expect_warning(genString("MOD15A2*"))
})