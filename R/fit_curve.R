#' Fit a curve to longitudinal data
#'
#' @description
#' `fit_curve()` is a low-level workhorse used by **timefitteR** to fit a specified
#' model formula to long-format longitudinal data. It prepares a local modelling
#' data set containing numeric `x` (the measurement) and numeric `t` (time),
#' removes incomplete cases, standardises the model formula to canonical variable
#' names (`x` and `t`), and fits the model using `stats::lm()`.
#'
#' The returned object contains common fit statistics (e.g., R-squared, RMSE,
#' AIC/BIC) and coefficient estimates with p-values, enabling downstream
#' decisions about whether a candidate curve is an adequate fit.
#'
#' @details
#' Column references for `x` and `t` may be supplied as bare names (tidyverse
#' style), single strings, or single integer column indices. If omitted, `x` and
#' `t` default to the first and second columns of `data`, respectively.
#'
#' The time column `t` is coerced to numeric using an internal coercion helper,
#' optionally stripping `t_prefix` and/or `t_suffix` prior to conversion.
#'
#' Rows with missing values in `x` or `t` are removed prior to model fitting. If
#' any rows are dropped, an informational message is emitted. If no complete
#' cases remain, the function errors.
#'
#' The model formula may be written using canonical names (`x`, `t`) or the
#' original column names supplied via `x` and `t`. The left-hand side of the
#' model must be the response variable (`x`, or the column mapped to `x`); left-
#' hand side transformations (e.g., `x^2 ~ t`) are not permitted.
#'
#' @param data A data.frame in long format containing at least the measurement
#'   and time columns.
#' @param x The measurement column. May be a bare name, a single string column
#'   name, or a single integer column index. Defaults to the first column of
#'   `data` if not supplied.
#' @param t The time column. May be a bare name, a single string column name, or
#'   a single integer column index. Defaults to the second column of `data` if
#'   not supplied.
#' @param model A model formula to be fit with `stats::lm()`. The model may refer
#'   to canonical names (`x`, `t`) or to the original column names identified by
#'   `x` and `t`. Default is `x ~ 1`.
#' @param t_prefix Optional string to strip from the start of `t` values prior to
#'   numeric coercion.
#' @param t_suffix Optional string to strip from the end of `t` values prior to
#'   numeric coercion.
#' @param keep_model Logical; if `TRUE` (default), the fitted `lm` object is
#'   retained in the result as `model_object`. If `FALSE`, `model_object` is
#'   `NULL` to reduce object size.
#'
#' @return An object of class `"fit_curve"`, a list containing:
#' \itemize{
#'   \item `model_input`: the model formula provided by the user.
#'   \item `model`: the canonical model formula actually fitted (in terms of `x` and `t`).
#'   \item `x`: metadata about the measurement column (`col`, `type`).
#'   \item `t`: metadata about the time column (`col`, `type`, `prefix`, `suffix`).
#'   \item `coefficients`: a data.frame of coefficient estimates and p-values.
#'   \item `fit`: a list of fit statistics (`n`, `r.squared`, `adj.r.squared`,
#'     `rse`, `rmse`, `df_resid`, `rss`, `aic`, `bic`, `logLik`).
#'   \item `F`: a list describing the model F-test (`statistic`, `df1`, `df2`,
#'     `p.value`), or `NA` values if unavailable.
#'   \item `model_object`: the fitted `lm` object (or `NULL` if `keep_model = FALSE`).
#' }
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   time  = rep(1:5, 10),
#'   score = rnorm(50) + rep(1:5, 10)
#' )
#'
#' # bare names
#' fit_curve(df, x = score, t = time, model = x ~ t)
#'
#' # string column names
#' fit_curve(df, x = "score", t = "time", model = score ~ time)
#'
#' # integer indices (x = 2nd col, t = 1st col)
#' fit_curve(df, x = 2, t = 1, model = x ~ t)
#'
#' # constant model
#' fit_curve(df, x = score, t = time, model = x ~ 1, keep_model = FALSE)


fit_curve <- function(data,
                      x,
                      t,
                      model = x ~ 1,
                      t_prefix = NULL,
                      t_suffix = NULL,
                      keep_model = TRUE) {

  model_input <- model

  if(!is.data.frame(data)){
    rlang::abort(
      message = "Argument `data` must be a data.frame.",
      class = "fit_curve_data_error",
      call = NULL
    )
  }

  if(ncol(data) < 2L){
    rlang::abort(
      message = "Argument `data` must have at least two columns for `x` and `t`.",
      class   = "fit_curve_data_error",
      call    = NULL
    )
  }

  if (nrow(data) == 0L) {
    rlang::abort(
      message = "No rows of useable data.",
      class   = "fit_curve_data_empty",
      call    = NULL
    )
  }

  x_col <- .resolve_column(x, data, arg_label = "x", default_idx = 1L)
  t_col <- .resolve_column(t, data, arg_label = "t", default_idx = 2L)

  #record original types for traceability (before any coercion)
  x_type_original <- class(data[[x_col]])[1L]
  t_type_original <- class(data[[t_col]])[1L]

  #check the format of columns
  # x must be numeric
  if (!is.numeric(data[[x_col]])) {
    rlang::abort(
      message = glue::glue("Column `{x_col}` specified for `x` must be numeric."),
      class   = "fit_curve_x_not_numeric",
      call    = NULL
    )
  }

  # coerce t to numeric
  t_num <- .coerce_var_numeric(
    data[[t_col]],
    prefix = t_prefix,
    suffix = t_suffix
  )

  # construct model_data with only the required columns
  model_data <- data.frame(
    t = t_num,
    x = data[[x_col]]
  )

  # remove any rows with NA in either x or t
  keep <- stats::complete.cases(model_data[, c("x", "t"), drop = FALSE])
  n_dropped <- sum(!keep)

  if (n_dropped > 0L) {
    rlang::inform(
      message = glue::glue(
        "Dropping {n_dropped} row{if (n_dropped == 1L) '' else 's'} with missing `x` and/or `t` before model fitting."
      ),
      class = "timefitteR_fit_curve_rows_dropped_na"
    )
  }

  model_data <- model_data[keep, , drop = FALSE]

  if (nrow(model_data) == 0L) {
    rlang::abort(
      message = "No complete cases remain after removing rows with missing `x` and/or `t`.",
      class   = "fit_curve_data_empty_after_na_filter",
      call    = NULL
    )
  }

  model <- .standardise_model_formula(model, x_col = x_col, t_col = t_col)
  fit_lm <- stats::lm(model, data = model_data)
  sm     <- summary(fit_lm)

  n_obs      <- stats::nobs(fit_lm)
  df_resid   <- stats::df.residual(fit_lm)
  rss        <- stats::deviance(fit_lm)
  logLik_val <- as.numeric(stats::logLik(fit_lm))
  aic_val    <- stats::AIC(fit_lm)
  bic_val    <- stats::BIC(fit_lm)
  rmse       <- sqrt(mean(stats::residuals(fit_lm)^2))

  # coefficients: Estimate and p-value
  coef_df <- as.data.frame(sm$coefficients)
  coef_df <- coef_df[, c("Estimate", "Pr(>|t|)"), drop = FALSE]
  names(coef_df) <- c("estimate", "p_value")

  # fit metrics
  fit <- list(
    n             = n_obs,
    r.squared     = unname(sm$r.squared),
    adj.r.squared = unname(sm$adj.r.squared),
    rse           = unname(sm$sigma),
    rmse          = rmse,
    df_resid      = df_resid,
    rss           = rss,
    aic           = aic_val,
    bic           = bic_val,
    logLik        = logLik_val
  )

  # F-statistics (may be absent in degenerate cases)
  if (!is.null(sm$fstatistic)) {
    fstat <- sm$fstatistic
    F <- list(
      statistic = unname(fstat["value"]),
      df1       = unname(fstat["numdf"]),
      df2       = unname(fstat["dendf"]),
      p.value   = stats::pf(
        q          = fstat["value"],
        df1        = fstat["numdf"],
        df2        = fstat["dendf"],
        lower.tail = FALSE
      )
    )
  } else {
    F <- list(
      statistic = NA_real_,
      df1       = NA_real_,
      df2       = NA_real_,
      p.value   = NA_real_
    )
  }

  model_object <- if (isTRUE(keep_model)) fit_lm else NULL
  #build result object
  res <- list(
    model_input = model_input,  # store before standardisation
    model       = model,        # canonical formula actually fitted

    t = list(
      col    = t_col,
      type   = t_type_original,
      prefix = t_prefix,
      suffix = t_suffix
    ),
    x = list(
      col  = x_col,
      type = x_type_original
    ),

    coefficients = coef_df,
    fit          = fit,
    F            = F,

    model_object = model_object
  )

  class(res) <- "fit_curve"
  res
}

#' Print method for fit_curve objects
#'
#' @param x An object of class `"fit_curve"`.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.fit_curve <- function(x, ...) {
  if (!inherits(x, "fit_curve")) {
    rlang::abort(
      message = "`x` must be a `fit_curve` object.",
      class   = "timefitteR_print_fit_curve_bad_object",
      call    = NULL
    )
  }

  # helpers
  fmt_num <- function(v, digits = 3) {
    if (length(v) == 0L || is.null(v) || is.na(v) || !is.finite(v)) return("NA")
    formatC(v, format = "f", digits = digits)
  }
  fmt_p <- function(p) {
    if (length(p) == 0L || is.null(p) || is.na(p) || !is.finite(p)) return("NA")
    if (p < 2.2e-16) return("<2.2e-16")
    format.pval(p, digits = 3, eps = 1e-16)
  }

  cat("<timefitteR::fit_curve>\n")

  # model / columns
  model_txt <- paste(deparse(x$model), collapse = " ")
  cat("  Model: ", model_txt, "\n", sep = "")

  cat(
    "  Data:  x = ", x$x$col, " (", x$x$type, "); ",
    "t = ", x$t$col, " (", x$t$type, ")\n",
    sep = ""
  )

  # fit stats
  fit <- x$fit %||% list()
  cat("  Fit:\n")
  cat("    n        = ", fit$n %||% NA_integer_, "\n", sep = "")
  cat("    R²       = ", fmt_num(fit$r.squared, 3), "\n", sep = "")
  cat("    adj. R²  = ", fmt_num(fit$adj.r.squared, 3), "\n", sep = "")
  cat("    RMSE     = ", fmt_num(fit$rmse, 3), "\n", sep = "")
  cat("    RSE      = ", fmt_num(fit$rse, 3), " (df = ", fit$df_resid %||% NA_integer_, ")\n", sep = "")
  cat("    AIC/BIC  = ", fmt_num(fit$aic, 1), " / ", fmt_num(fit$bic, 1), "\n", sep = "")
  cat("    logLik   = ", fmt_num(fit$logLik, 3), "\n", sep = "")

  # overall F-test (if available)
  if (!is.null(x$F) && is.list(x$F) && !is.na(x$F$p.value)) {
    cat(
      "  Overall F-test: F(",
      x$F$df1, ", ", x$F$df2, ") = ", fmt_num(x$F$statistic, 3),
      ", p = ", fmt_p(x$F$p.value), "\n",
      sep = ""
    )
  }

  # model retention flag
  kept <- !is.null(x$model_object)
  cat("  Model object stored: ", if (kept) "yes" else "no", "\n", sep = "")

  invisible(x)
}

#' Summary method for fit_curve objects
#'
#' @param object An object of class `"fit_curve"`.
#' @param ... Unused.
#'
#' @return An object of class `"summary_fit_curve"` containing a richer summary
#'   of the fitted curve, including (when available) full coefficient statistics.
#' @export
summary.fit_curve <- function(object, ...) {
  if (!inherits(object, "fit_curve")) {
    rlang::abort(
      message = "`object` must be a `fit_curve` object.",
      class   = "timefitteR_summary_fit_curve_bad_object",
      call    = NULL
    )
  }

  # prefer full coefficient table if model stored
  if (!is.null(object$model_object) && inherits(object$model_object, "lm")) {
    sm <- summary(object$model_object)
    coef_mat <- sm$coefficients
    coef_df <- as.data.frame(coef_mat)
    names(coef_df) <- c("estimate", "std_error", "t_value", "p_value")
  } else {
    # fall back to the stored minimal coefficient table
    coef_df <- object$coefficients
    if (!is.null(coef_df) && nrow(coef_df) > 0L) {
      coef_df$std_error <- NA_real_
      coef_df$t_value   <- NA_real_
      coef_df <- coef_df[, c("estimate", "std_error", "t_value", "p_value"), drop = FALSE]
    }
  }

  out <- list(
    model_input  = object$model_input,
    model        = object$model,
    x            = object$x,
    t            = object$t,
    fit          = object$fit,
    F            = object$F,
    coefficients = coef_df,
    model_kept   = !is.null(object$model_object)
  )

  class(out) <- "summary_fit_curve"
  out
}

#' Print method for summary_fit_curve objects
#'
#' @param x An object of class `"summary_fit_curve"`.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.summary_fit_curve <- function(x, ...) {
  if (!inherits(x, "summary_fit_curve")) {
    rlang::abort(
      message = "`x` must be a `summary_fit_curve` object.",
      class   = "timefitteR_print_summary_fit_curve_bad_object",
      call    = NULL
    )
  }

  fmt_num <- function(v, digits = 3) {
    if (length(v) == 0L || is.null(v) || is.na(v) || !is.finite(v)) return("NA")
    formatC(v, format = "f", digits = digits)
  }
  fmt_p <- function(p) {
    if (length(p) == 0L || is.null(p) || is.na(p) || !is.finite(p)) return("NA")
    if (p < 2.2e-16) return("<2.2e-16")
    format.pval(p, digits = 3, eps = 1e-16)
  }

  cat("<timefitteR::summary_fit_curve>\n")
  cat("  Model: ", paste(deparse(x$model), collapse = " "), "\n", sep = "")
  cat(
    "  Data:  x = ", x$x$col, " (", x$x$type, "); ",
    "t = ", x$t$col, " (", x$t$type, ")\n",
    sep = ""
  )

  fit <- x$fit
  cat("  Fit statistics:\n")
  cat("    n        = ", fit$n, "\n", sep = "")
  cat("    R²       = ", fmt_num(fit$r.squared, 3), "\n", sep = "")
  cat("    adj. R²  = ", fmt_num(fit$adj.r.squared, 3), "\n", sep = "")
  cat("    RMSE     = ", fmt_num(fit$rmse, 3), "\n", sep = "")
  cat("    RSE      = ", fmt_num(fit$rse, 3), " (df = ", fit$df_resid, ")\n", sep = "")
  cat("    RSS      = ", fmt_num(fit$rss, 3), "\n", sep = "")
  cat("    AIC/BIC  = ", fmt_num(fit$aic, 1), " / ", fmt_num(fit$bic, 1), "\n", sep = "")
  cat("    logLik   = ", fmt_num(fit$logLik, 3), "\n", sep = "")

  if (!is.null(x$F) && is.list(x$F) && !is.na(x$F$p.value)) {
    cat(
      "  Overall F-test: F(",
      x$F$df1, ", ", x$F$df2, ") = ", fmt_num(x$F$statistic, 3),
      ", p = ", fmt_p(x$F$p.value), "\n",
      sep = ""
    )
  }

  cat("  Coefficients:\n")
  # Print a compact coefficient table
  coefs <- x$coefficients
  if (is.null(coefs) || nrow(coefs) == 0L) {
    cat("    <none>\n")
  } else {
    # format columns a little for printing
    coefs_print <- coefs
    coefs_print$estimate  <- as.numeric(coefs_print$estimate)
    coefs_print$std_error <- as.numeric(coefs_print$std_error)
    coefs_print$t_value   <- as.numeric(coefs_print$t_value)
    coefs_print$p_value   <- as.numeric(coefs_print$p_value)

    # avoid scientific noise in the main table; p-values handled separately
    coefs_print$estimate  <- ifelse(is.finite(coefs_print$estimate),  fmt_num(coefs_print$estimate,  4), "NA")
    coefs_print$std_error <- ifelse(is.finite(coefs_print$std_error), fmt_num(coefs_print$std_error, 4), "NA")
    coefs_print$t_value   <- ifelse(is.finite(coefs_print$t_value),   fmt_num(coefs_print$t_value,   3), "NA")
    coefs_print$p_value   <- vapply(coefs$p_value, fmt_p, character(1))

    print(coefs_print, row.names = TRUE)
  }

  cat("  Model object stored: ", if (isTRUE(x$model_kept)) "yes" else "no", "\n", sep = "")
  invisible(x)
}



