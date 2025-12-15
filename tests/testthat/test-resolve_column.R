testthat::test_that(".resolve_column() defaults correctly when arg is missing", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  # Forward a truly-missing arg using `...`
  f_default1 <- function(data, ...) .resolve_column(..., data = data, arg_label = "x", default_idx = 1L)
  f_default2 <- function(data, ...) .resolve_column(..., data = data, arg_label = "t", default_idx = 2L)

  testthat::expect_identical(f_default1(df), "a")
  testthat::expect_identical(f_default2(df), "b")
})

testthat::test_that(".resolve_column() errors if default_idx cannot be satisfied", {
  df1 <- data.frame(a = 1:3)

  f_default2 <- function(data, ...) .resolve_column(..., data = data, arg_label = "t", default_idx = 2L)

  testthat::expect_error(
    f_default2(df1),
    class = "timefitteR_resolve_column_data_error"
  )
})

testthat::test_that(".resolve_column() resolves bare integer indices", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  testthat::expect_identical(.resolve_column(1, df, "x", 1L), "a")
  testthat::expect_identical(.resolve_column(2, df, "x", 1L), "b")
  testthat::expect_identical(.resolve_column(3, df, "x", 1L), "c")

  # integer-ish numeric should also work with is_scalar_integerish()
  testthat::expect_identical(.resolve_column(2.0, df, "x", 1L), "b")
})

testthat::test_that(".resolve_column() errors on out-of-range integer indices", {
  df <- data.frame(a = 1:3, b = 4:6)

  testthat::expect_error(
    .resolve_column(0, df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )

  testthat::expect_error(
    .resolve_column(3, df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )

  testthat::expect_error(
    .resolve_column(-1, df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )
})

testthat::test_that(".resolve_column() resolves string column names", {
  df <- data.frame(a = 1:3, b = 4:6)

  testthat::expect_identical(.resolve_column("a", df, "x", 1L), "a")
  testthat::expect_identical(.resolve_column("b", df, "x", 1L), "b")
})

testthat::test_that(".resolve_column() errors on missing string column names", {
  df <- data.frame(a = 1:3, b = 4:6)

  testthat::expect_error(
    .resolve_column("nope", df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )
})

testthat::test_that(".resolve_column() resolves bare column names (symbols)", {
  df <- data.frame(a = 1:3, b = 4:6)

  testthat::expect_identical(.resolve_column(a, df, "x", 1L), "a")
  testthat::expect_identical(.resolve_column(b, df, "x", 1L), "b")
})

testthat::test_that(".resolve_column() errors on missing bare column names (symbols)", {
  df <- data.frame(a = 1:3, b = 4:6)

  testthat::expect_error(
    .resolve_column(nope, df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )
})

testthat::test_that(".resolve_column() rejects unsupported expressions and types", {
  df <- data.frame(a = 1:3, b = 4:6)

  testthat::expect_error(
    .resolve_column(1.5, df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )

  testthat::expect_error(
    .resolve_column(c("a", "b"), df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )

  testthat::expect_error(
    .resolve_column(1:2, df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )

  testthat::expect_error(
    .resolve_column(TRUE, df, "x", 1L),
    class = "timefitteR_resolve_column_bad_x"
  )
})

testthat::test_that(".resolve_column() uses arg_label to set the error class", {
  df <- data.frame(a = 1:3, b = 4:6)

  testthat::expect_error(
    .resolve_column("nope", df, "t", 2L),
    class = "timefitteR_resolve_column_bad_t"
  )

  testthat::expect_error(
    .resolve_column(99, df, "t", 2L),
    class = "timefitteR_resolve_column_bad_t"
  )
})

testthat::test_that(".resolve_column() always returns a length-1 character scalar", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, check.names = FALSE)

  results <- list(
    .resolve_column(a,   df, "x", 1L),
    .resolve_column("b", df, "x", 1L),
    .resolve_column(3,   df, "x", 1L),
    {
      f_default1 <- function(data, ...) .resolve_column(..., data = data, arg_label = "x", default_idx = 1L)
      f_default1(df)
    }
  )

  for (res in results) {
    testthat::expect_type(res, "character")
    testthat::expect_length(res, 1L)
    testthat::expect_true(nzchar(res))
  }
})
