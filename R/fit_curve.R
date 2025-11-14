fit_curve <- function(data,
                      x,
                      y,
                      model = y ~ x,
                      filters = NULL,
                      x_prefix = NULL,
                      x_suffix = NULL) {

  if(!is.data.frame(data)){
    rlang::abort(
      message = "Argument `data` must be a data.frame.",
      class = "fit_curve_data_error",
      call = NULL
    )
  }

  if(ncol(data) < 2L){
    rlang::abort(
      message = "Argument `data` must have at least two columns for `x` and `y`.",
      class   = "fit_curve_data_error",
      call    = NULL
    )
  }

  #allow bare column names to be passed for x and or y
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)

  #default to first two column names if x or y not supplied
  if(rlang::quo_is_missing(x_quo)){
    x_col <- names(data)[1L]
  }else{
    x_col <- rlang::as_name(x_quo)
  }
  if(rlang::quo_is_missing(y_quo)){
    y_col <- names(data)[2L]
  }else{
    y_col <- rlang::as_name(y_quo)
  }

  #check the columns supplied actually exist else give up
  if(!x_col %in% names(data)){
    rlang::abort(
      message = glue::glue("Column `{x_col}` specified for `x` not found in `data`."),
      class   = "fit_curve_bad_x",
      call    = NULL
    )
  }
  if(!y_col %in% names(data)){
    rlang::abort(
      message = glue::glue("Column `{y_col}` specified for `y` not found in `data`."),
      class   = "fit_curve_bad_y",
      call    = NULL
    )
  }

  #record original types for traceability (before any coercion)
  x_type_original <- class(data[[x_col]])[1L]
  y_type_original <- class(data[[y_col]])[1L]

  #apply any filters

  if (!is.null(filters)) {
    if (!is.list(filters)) {
      rlang::abort(
        message = "Argument `filters` must be a list of expressions or one-sided formulas.",
        class   = "fit_curve_filter_error",
        call    = NULL
      )
    }
    for (flt in filters) {
      if (inherits(flt, "formula")) {
        # one-sided formula: ~ Group == "A"
        expr <- rlang::f_rhs(flt)
      } else {
        expr <- flt
      }

      keep <- eval(expr, envir = data, enclos = parent.frame())

      if (!is.logical(keep) || length(keep) != nrow(data)) {
        rlang::abort(
          message = "Each filter must evaluate to a logical vector of length nrow(data).",
          class   = "fit_curve_filter_error",
          call    = NULL
        )
      }

      keep[is.na(keep)] <- FALSE
      data <- data[keep, , drop = FALSE]
    }
  }

  if (nrow(data) == 0L) {
    rlang::abort(
      message = "No rows remaining after applying `filters`.",
      class   = "fit_curve_data_empty_after_filter",
      call    = NULL
    )
  }

  #check the numeric format of columns
  if(!is.numeric(data[[y_col]])){
    rlang::abort(
      message = glue::glue("Column `{y_col}` specified for `y` must be numeric."),
      class   = "fit_curve_not_numeric",
      call    = NULL
    )
  }
  aux_x_col <- ".timefitteR_x_numeric"
  data[[aux_x_col]] <- .timefitteR_coerce_x_numeric(
    data[[x_col]],
    prefix = x_prefix,
    suffix = x_suffix
  )

  #construct modelling data
  model_data <- data
  model_data[["x"]] <- model_data[[aux_x_col]]
  model_data[["y"]] <- model_data[[y_col]]

  if (!inherits(model, "formula")) {
    rlang::abort(
      message = "Argument `model` must be a formula, e.g. `y ~ x`.",
      class   = "fit_curve_model_error",
      call    = NULL
    )
  }

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
    x = list(
      col = x_col,
      type = x_type_original,
      prefix = x_prefix,
      suffix = x_suffix
    ),
    y = list(
      col = y_col,
      type = y_type_original
    ),
    coefficients = coef_df,
    fit          = fit,
    F            = F
  )

  class(res) <- "fit_curve"
  res
}
