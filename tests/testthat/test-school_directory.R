test_that("school dir fn checks params correctly + returns correct outputs", {

  expect_message(x <- vz_get_directory(uzemi = "CZ0514", typ = "B", misto = "Semily",
                                       return_tibbles = T))
  expect_gt(nrow(x), 0)
  expect_gt(ncol(x), 0)

  expect_type(y <- vz_get_directory(uzemi = "CZ0514", typ = "B", misto = "Semily",
                                    return_tibbles = F),
              "character")
  expect_type(z <- vz_get_directory(uzemi = "CZ0514",
                                    tables = c("addresses", "locations",
                                               "schools", "specialisations"),
                                    typ = "B", misto = "Semily",
                                    return_tibbles = F, keep_files = F),
              "character")
  expect_length(z, 4)
  expect_true(all(file.exists(y)))
  expect_true(all(file.exists(z)))

  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"), exact = T)
  expect_error(vz_get_directory(tables = "x"))
})
