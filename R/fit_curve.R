fit_curve <- function(data,
                      x,
                      t,
                      model = x ~ 1,
                      t_prefix = NULL,
                      t_suffix = NULL) {

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

  # coefficients: Estimate and p-value
  coef_df <- as.data.frame(sm$coefficients)
  coef_df <- coef_df[, c("Estimate", "Pr(>|t|)"), drop = FALSE]
  names(coef_df) <- c("estimate", "p_value")

  # fit metrics
  fit <- list(
    r.squared     = unname(sm$r.squared),
    adj.r.squared = unname(sm$adj.r.squared),
    rse           = unname(sm$sigma),
    df            = unname(sm$df[2L]),                 # residual df
    rss           = sum(sm$residuals^2, na.rm = TRUE)
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

  #build result object
  res <- list(
    model = model,
    t = list(
      col = t_col,
      type = t_type_original,
      prefix = t_prefix,
      suffix = t_suffix
    ),
    x = list(
      col = x_col,
      type = x_type_original
    ),
    coefficients = coef_df,
    fit          = fit,
    F            = F
  )

  class(res) <- "fit_curve"
  res
}
