context("genString")

test_that("genString() throws a warning for multiple products", {
  expect_warning(genString(c("MYD15A2", "MOD15A2"), collection = "005"))
  expect_warning(genString("MOD15A2*", collection = 5))
})