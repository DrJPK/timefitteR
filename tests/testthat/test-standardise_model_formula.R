testthat::test_that(".standardise_model_formula() rejects non-formula inputs", {
  testthat::expect_error(
    .standardise_model_formula("x ~ t", x_col = "score", t_col = "time"),
    class = "fit_curve_model_error"
  )
})

testthat::test_that(".standardise_model_formula() accepts canonical constant model x ~ 1", {
  f <- .standardise_model_formula(x ~ 1, x_col = "score", t_col = "time")
  testthat::expect_true(inherits(f, "formula"))
  testthat::expect_identical(deparse(f), deparse(x ~ 1))
})

testthat::test_that(".standardise_model_formula() accepts canonical time model x ~ t", {
  f <- .standardise_model_formula(x ~ t, x_col = "score", t_col = "time")
  testthat::expect_true(inherits(f, "formula"))
  testthat::expect_identical(deparse(f), deparse(x ~ t))
})

testthat::test_that(".standardise_model_formula() rewrites human column names to x and t", {
  # no need for actual data here; just mapping names
  f <- .standardise_model_formula(score ~ time, x_col = "score", t_col = "time")
  testthat::expect_identical(deparse(f), deparse(x ~ t))
})

testthat::test_that(".standardise_model_formula() rewrites human names with transformations on RHS", {
  f <- .standardise_model_formula(score ~ poly(time, 2), x_col = "score", t_col = "time")
  testthat::expect_identical(deparse(f), deparse(x ~ poly(t, 2)))
})

testthat::test_that(".standardise_model_formula() rejects transformed LHS (canonical)", {
  testthat::expect_error(
    .standardise_model_formula(x^2 ~ t, x_col = "score", t_col = "time"),
    class = "fit_curve_model_bad_lhs"
  )

  testthat::expect_error(
    .standardise_model_formula(log(x) ~ t, x_col = "score", t_col = "time"),
    class = "fit_curve_model_bad_lhs"
  )
})

testthat::test_that(".standardise_model_formula() rejects transformed LHS (human column name)", {
  testthat::expect_error(
    .standardise_model_formula(score^2 ~ time, x_col = "score", t_col = "time"),
    class = "fit_curve_model_bad_lhs"
  )

  testthat::expect_error(
    .standardise_model_formula(log(score) ~ time, x_col = "score", t_col = "time"),
    class = "fit_curve_model_bad_lhs"
  )
})

testthat::test_that(".standardise_model_formula() rejects wrong LHS variable", {
  testthat::expect_error(
    .standardise_model_formula(t ~ x, x_col = "score", t_col = "time"),
    class = "fit_curve_model_bad_lhs"
  )

  testthat::expect_error(
    .standardise_model_formula(time ~ score, x_col = "score", t_col = "time"),
    class = "fit_curve_model_bad_lhs"
  )
})

testthat::test_that(".standardise_model_formula() rejects unknown variables", {
  testthat::expect_error(
    .standardise_model_formula(x ~ z, x_col = "score", t_col = "time"),
    class = "fit_curve_model_unknown_vars"
  )

  testthat::expect_error(
    .standardise_model_formula(score ~ z, x_col = "score", t_col = "time"),
    class = "fit_curve_model_unknown_vars"
  )

  testthat::expect_error(
    .standardise_model_formula(score ~ time + z, x_col = "score", t_col = "time"),
    class = "fit_curve_model_unknown_vars"
  )
})

testthat::test_that(".standardise_model_formula() always returns canonical LHS `x`", {
  f1 <- .standardise_model_formula(score ~ 1, x_col = "score", t_col = "time")
  f2 <- .standardise_model_formula(score ~ time, x_col = "score", t_col = "time")

  testthat::expect_identical(rlang::as_string(f1[[2]]), "x")
  testthat::expect_identical(rlang::as_string(f2[[2]]), "x")
})

testthat::test_that(".standardise_model_formula() returns a formula object", {
  f <- .standardise_model_formula(score ~ time, x_col = "score", t_col = "time")
  testthat::expect_true(inherits(f, "formula"))
})
