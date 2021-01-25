test_that("multiplication works", {
  expect_error(check_server("https://abc.x"))
  expect_true(check_server("https://google.com"))
  expect_error(check_server("https://google.com/hitjere"))
  expect_true(check_server("https://petrbouchal.xyz/covid/"))
})
