test_that("school dir fn checks params correctly + returns correct outputs", {

  expect_message(x <- sk_get_directory(uzemi = "CZ0514", typ = "B", misto = "Semily",
                                       return_tibbles = T))
  expect_gt(nrow(x[[1]]), 0)
  expect_gt(ncol(x[[1]]), 0)
  expect_length(x, 1)
  expect_type(x, "list")

  expect_type(y <- sk_get_directory(uzemi = "CZ0514", typ = "B", misto = "Semily",
                                    return_tibbles = F),
              "character")
  expect_type(z <- sk_get_directory(uzemi = "CZ0514", typ = "B", misto = "Semily",
                                    return_tibbles = F, keep_files = F),
              "character")
  expect_true(all(file.exists(y)))
  expect_true(all(file.exists(z)))

  expect_s3_class(x[[1]], c("tbl_df", "tbl", "data.frame"), exact = T)
  expect_error(sk_get_directory(tables = "x"))
})
