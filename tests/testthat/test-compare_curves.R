testthat::test_that("compare_curves() returns a compare_curves object with a table", {
  df <- data.frame(
    time  = rep(1:5, 4),
    score = rnorm(20)
  )

  res <- compare_curves(df, x = score, t = time, keep_model = FALSE)

  testthat::expect_s3_class(res, "compare_curves")
  testthat::expect_true(is.list(res))
  testthat::expect_true(is.data.frame(res$table))
  testthat::expect_true(nrow(res$table) >= 1L)

  # required columns that should always exist
  required <- c("name", "family", "formula_template", "complexity", "status", "fit_object")
  testthat::expect_true(all(required %in% names(res$table)))
})

testthat::test_that("compare_curves() filters models that cannot handle t == 0", {
  df0 <- data.frame(
    time  = c(0, 1, 2, 3, 4),
    score = rnorm(5)
  )

  res <- compare_curves(df0, x = score, t = time, keep_model = FALSE)

  # log_time requires t > 0, so it should not be attempted when t == 0 exists
  testthat::expect_false("log_time" %in% res$table$name)
})

testthat::test_that("compare_curves() filters models that cannot handle t < 0", {
  dfneg <- data.frame(
    time  = c(-1, 0, 1, 2, 3),
    score = rnorm(5)
  )

  res <- compare_curves(dfneg, x = score, t = time, keep_model = FALSE)

  # power_fixed models disallow negative t in our registry
  testthat::expect_false("sqrt_time" %in% res$table$name)
  testthat::expect_false("cuberoot_time" %in% res$table$name)
  # log_time also disallows negative t
  testthat::expect_false("log_time" %in% res$table$name)
})

testthat::test_that("compare_curves() includes per-model status and error fields on failure", {
  # use constant data where many models may still fit, but we also expect that
  # nonlinear placeholders (logistic/gompertz) will error unless filtered later
  df <- data.frame(
    time  = rep(1:5, 3),
    score = rep(1, 15)
  )

  res <- compare_curves(df, x = score, t = time, keep_model = FALSE)

  testthat::expect_true("status" %in% names(res$table))
  testthat::expect_true(any(res$table$status %in% c("ok", "error")))

  # if any errors exist, ensure error metadata columns are present
  if (any(res$table$status == "error")) {
    testthat::expect_true("error_class" %in% names(res$table))
    testthat::expect_true("error_message" %in% names(res$table))
  }
})

testthat::test_that("print.compare_curves() does not error", {
  df <- data.frame(
    time  = rep(1:5, 4),
    score = rnorm(20)
  )
  res <- compare_curves(df, x = score, t = time, keep_model = FALSE)

  testthat::expect_output(
    print(res),
    regexp = "<timefitteR::compare_curves>"
  )
})

testthat::test_that("summary.compare_curves() returns a summary_compare_curves object", {
  df <- data.frame(
    time  = rep(1:6, 5),
    score = rnorm(30)
  )
  res <- compare_curves(df, x = score, t = time, keep_model = FALSE)

  s <- summary(res)

  testthat::expect_s3_class(s, "summary_compare_curves")
  testthat::expect_true(is.character(s$rank_by) || is.na(s$rank_by))
  testthat::expect_true(is.data.frame(s$table))
})

testthat::test_that("summary.compare_curves() identifies a best model when any fits succeed", {
  df <- data.frame(
    time  = rep(1:6, 5),
    score = rnorm(30)
  )
  res <- compare_curves(df, x = score, t = time, keep_model = FALSE)
  s <- summary(res)

  # At least the intercept-only model should succeed
  testthat::expect_true(!is.null(s$best_overall))
  testthat::expect_true(is.data.frame(s$best_overall))
  testthat::expect_equal(nrow(s$best_overall), 1L)
})

testthat::test_that("print.summary_compare_curves() does not error", {
  df <- data.frame(
    time  = rep(1:6, 5),
    score = rnorm(30)
  )
  res <- compare_curves(df, x = score, t = time, keep_model = FALSE)
  s <- summary(res)

  testthat::expect_output(
    print(s),
    regexp = "<timefitteR::summary_compare_curves>"
  )
})

testthat::test_that("checks for string inputs to compare_curves()",{
  df <- data.frame(
    time  = rep(1:6, 5),
    score = rnorm(30)
  )
  res2 <- compare_curves(df, x = "score", t = "time", keep_model = FALSE)
  testthat::expect_s3_class(res2, "compare_curves")
})
