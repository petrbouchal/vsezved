test_that("school dir fn checks params correctly + returns correct outputs", {
  withr::with_dir(tempdir(), {

    expect_message(x <- vz_get_directory(uzemi = "CZ0514", typ = "B", misto = "Semily",
                                         return_tibbles = TRUE, write_files = FALSE))
    expect_gt(nrow(x), 0)
    expect_gt(ncol(x), 0)
    expect_type(y <- vz_get_directory(uzemi = "CZ0514", typ = "B", misto = "Semily",
                                      return_tibbles = FALSE, write_files = TRUE),
                "character")
    expect_type(z <- vz_get_directory(uzemi = "CZ0514",
                                      tables = c("addresses", "locations",
                                                 "schools", "specialisations"),
                                      typ = "B", misto = "Semily",
                                      return_tibbles = FALSE, write_files = TRUE),
                "character")
    expect_length(z, 4)
    expect_true(all(file.exists(y)))
    expect_true(all(file.exists(z)))

    expect_s3_class(x, c("tbl_df", "tbl", "data.frame"), exact = T)
    expect_error(vz_get_directory(tables = "x"))
  })
})
