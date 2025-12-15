#' Compare candidate curve models for longitudinal data
#'
#' @description
#' Fits a set of candidate models from the internal model registry to the
#' supplied data and returns a comparison table of fit statistics.
#'
#' @param data A data.frame in long format.
#' @param x Measurement column (bare name, string, or integer index).
#' @param t Time column (bare name, string, or integer index).
#' @param keep_model Logical; passed through to `fit_curve()`.
#' @param ... Reserved for future options (e.g., model filtering, selection rules).
#'
#' @return An object of class `"compare_curves"` with:
#'   \itemize{
#'     \item `table`: a data.frame with one row per model.
#'     \item `fits`: a list of `fit_curve` objects (may contain failures as `NULL`).
#'     \item `registry`: the registry used (possibly filtered).
#'     \item `x`/`t`: resolved column metadata.
#'   }
#' @export
compare_curves <- function(data,
                           x,
                           t,
                           t_prefix = NULL,
                           t_suffix = NULL,
                           keep_model = TRUE,
                           ...) {

  if (!exists(".get_model_registry", mode = "function", inherits = TRUE)) {
    rlang::abort(
      message = "Internal function `.get_model_registry()` not available. Load the package namespace (e.g., devtools::load_all()) before calling compare_curves().",
      class   = "timefitteR_compare_curves_internal_missing",
      call    = NULL
    )
  }

  x_quo <- rlang::enquo(x)
  t_quo <- rlang::enquo(t)

  if (!is.data.frame(data)) {
    rlang::abort(
      message = "Argument `data` must be a data.frame.",
      class   = "compare_curves_data_error",
      call    = NULL
    )
  }

  if (ncol(data) < 2L) {
    rlang::abort(
      message = "Argument `data` must have at least two columns for `x` and `t`.",
      class   = "compare_curves_data_error",
      call    = NULL
    )
  }

  if (nrow(data) == 0L) {
    rlang::abort(
      message = "No rows of useable data.",
      class   = "compare_curves_data_empty",
      call    = NULL
    )
  }

  # Resolve columns once (so registry filtering can use the true t column)
  x_col <- if (rlang::quo_is_missing(x_quo)) {
    .resolve_column(data = data, arg_label = "x", default_idx = 1L)
  } else {
    rlang::inject(.resolve_column(!!rlang::get_expr(x_quo), data = data, arg_label = "x", default_idx = 1L))
  }

  t_col <- if (rlang::quo_is_missing(t_quo)) {
    .resolve_column(data = data, arg_label = "t", default_idx = 2L)
  } else {
    rlang::inject(.resolve_column(!!rlang::get_expr(t_quo), data = data, arg_label = "t", default_idx = 2L))
  }

  # Coerce t for constraint checking only (do not mutate data)
  t_num <- .coerce_var_numeric(
    data[[t_col]],
    prefix = t_prefix,
    suffix = t_suffix
  )
  # NOTE: time translation (e.g., shifting t to be positive) is intentionally
  # not performed here; registry compatibility is assessed on the supplied scale.

  has_t0       <- any(t_num == 0, na.rm = TRUE)
  has_tneg     <- any(t_num < 0,  na.rm = TRUE)

  registry <- .get_model_registry()

  # Filter registry based on t constraints (leave translation to caller)
  ok <- rep(TRUE, nrow(registry))
  if (has_t0) {
    ok <- ok & registry$allows_t0
  }
  if (has_tneg) {
    ok <- ok & registry$allows_t_negative
  }

  registry_used <- registry[ok, , drop = FALSE]

  #Temporary drop of nls models
  registry_used <- registry_used[!is.na(registry_used$k_params), , drop = FALSE]

  if (nrow(registry_used) == 0L) {
    rlang::abort(
      message = "No candidate models are compatible with the observed `t` values (t = 0 and/or t < 0 present).",
      class   = "compare_curves_no_compatible_models",
      call    = NULL
    )
  }

  # Fit each model; keep a list of results for downstream work
  fits <- vector("list", nrow(registry_used))
  names(fits) <- registry_used$name

  # Table rows (built incrementally)
  rows <- vector("list", nrow(registry_used))

  for (i in seq_len(nrow(registry_used))) {
    reg_i <- registry_used[i, , drop = FALSE]
    name_i <- reg_i$name
    form_i <- stats::as.formula(reg_i$formula_template, env = baseenv())

    fit_i <- tryCatch(
      fit_curve(
        data       = data,
        x          = !!rlang::get_expr(x_quo),
        t          = !!rlang::get_expr(t_quo),
        model      = form_i,
        t_prefix   = t_prefix,
        t_suffix   = t_suffix,
        keep_model = keep_model
      ),
      error = function(e) e
    )

    fits[[i]] <- fit_i

    # If fit failed, capture error info but keep the row
    if (inherits(fit_i, "error")) {
      rows[[i]] <- data.frame(
        name             = name_i,
        family           = reg_i$family,
        formula_template = reg_i$formula_template,
        complexity       = reg_i$complexity,
        k_params_registry= reg_i$k_params,
        # Fit outputs (NA)
        n                = NA_integer_,
        k_params_fit     = NA_integer_,
        r.squared        = NA_real_,
        adj.r.squared    = NA_real_,
        rse              = NA_real_,
        rmse             = NA_real_,
        df_resid         = NA_integer_,
        rss              = NA_real_,
        aic              = NA_real_,
        aicc             = NA_real_,
        bic              = NA_real_,
        logLik           = NA_real_,
        F_statistic      = NA_real_,
        F_df1            = NA_real_,
        F_df2            = NA_real_,
        F_p_value        = NA_real_,
        status           = "error",
        error_class      = class(fit_i)[1L],
        error_message    = conditionMessage(fit_i),
        stringsAsFactors = FALSE
      )
      next
    }

    # Successful fit: extract results
    rows[[i]] <- data.frame(
      name             = name_i,
      family           = reg_i$family,
      formula_template = reg_i$formula_template,
      complexity       = reg_i$complexity,
      k_params_registry= reg_i$k_params,

      n                = fit_i$fit$n,
      k_params_fit     = fit_i$fit$k_params,
      r.squared        = fit_i$fit$r.squared,
      adj.r.squared    = fit_i$fit$adj.r.squared,
      rse              = fit_i$fit$rse,
      rmse             = fit_i$fit$rmse,
      df_resid         = fit_i$fit$df_resid,
      rss              = fit_i$fit$rss,
      aic              = fit_i$fit$aic,
      aicc             = fit_i$fit$aicc,
      bic              = fit_i$fit$bic,
      logLik           = fit_i$fit$logLik,

      F_statistic      = fit_i$F$statistic,
      F_df1            = fit_i$F$df1,
      F_df2            = fit_i$F$df2,
      F_p_value        = fit_i$F$p.value,

      status           = "ok",
      error_class      = NA_character_,
      error_message    = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  table <- do.call(rbind, rows)

  if (!any(table$status == "ok", na.rm = TRUE)) {
    rlang::inform(
      message = "All candidate models failed to fit. Inspect `table$error_message` for details.",
      class   = "timefitteR_compare_curves_all_models_failed"
    )
  }

  # Add a list-column of fits (optional but useful)
  # Keep as a named list aligned with `table$name`
  fit_map <- setNames(vector("list", nrow(table)), table$name)
  for (nm in names(fits)) fit_map[[nm]] <- fits[[nm]]
  table$fit_object <- unname(fit_map[table$name])

  out <- list(
    table    = table,
    fits     = fits,
    registry = registry_used,
    x        = list(col = x_col, type = class(data[[x_col]])[1L]),
    t        = list(col = t_col, type = class(data[[t_col]])[1L])
  )

  class(out) <- "compare_curves"
  out
}

#' Print method for compare_curves objects
#'
#' @param x An object of class `"compare_curves"`.
#' @param ... Unused.
#' @export
print.compare_curves <- function(x, ...) {
  if (!inherits(x, "compare_curves")) {
    rlang::abort(
      message = "`x` must be a `compare_curves` object.",
      class   = "timefitteR_print_compare_curves_bad_object",
      call    = NULL
    )
  }

  tbl <- x$table
  if (!is.data.frame(tbl) || nrow(tbl) == 0L) {
    cat("<timefitteR::compare_curves>\n  <empty>\n")
    return(invisible(x))
  }

  # Choose ranking metric: AICc > AIC > BIC
  score_col <- if ("aicc" %in% names(tbl) && any(is.finite(tbl$aicc), na.rm = TRUE)) {
    "aicc"
  } else if ("aic" %in% names(tbl) && any(is.finite(tbl$aic), na.rm = TRUE)) {
    "aic"
  } else if ("bic" %in% names(tbl) && any(is.finite(tbl$bic), na.rm = TRUE)) {
    "bic"
  } else {
    NA_character_
  }

  ok <- !is.na(tbl$status) & tbl$status == "ok"

  # Rank OK fits by score then complexity then name
  tbl_ok <- tbl[ok, , drop = FALSE]
  if (!is.na(score_col) && nrow(tbl_ok) > 0L) {
    ord <- order(tbl_ok[[score_col]], tbl_ok$complexity, tbl_ok$name, na.last = TRUE)
    tbl_ok <- tbl_ok[ord, , drop = FALSE]
  } else if (nrow(tbl_ok) > 0L) {
    ord <- order(tbl_ok$complexity, tbl_ok$name)
    tbl_ok <- tbl_ok[ord, , drop = FALSE]
  }

  tbl_err <- tbl[!ok, , drop = FALSE]

  cat("<timefitteR::compare_curves>\n")
  cat("  x: ", x$x$col, " (", x$x$type, ");  t: ", x$t$col, " (", x$t$type, ")\n", sep = "")
  cat("  Models attempted: ", nrow(tbl), "  |  OK: ", nrow(tbl_ok), "  |  Errors: ", nrow(tbl_err), "\n", sep = "")

  if (!is.na(score_col)) {
    cat("  Ranked by: ", toupper(score_col), " (lower is better)\n", sep = "")
  }

  # Columns to show
  show_cols <- c("name", "family", "complexity", "k_params_fit", "rmse", "r.squared", "aicc", "aic", "bic", "status")
  show_cols <- show_cols[show_cols %in% names(tbl)]

  # Print top models (OK only)
  if (nrow(tbl_ok) > 0L) {
    top_n <- min(10L, nrow(tbl_ok))
    cat("\n  Top fits:\n")
    print(tbl_ok[seq_len(top_n), show_cols, drop = FALSE], row.names = FALSE)
  } else {
    cat("\n  No successful fits.\n")
  }

  # Print any errors (names only + short message)
  if (nrow(tbl_err) > 0L) {
    cat("\n  Errors:\n")
    err_view <- tbl_err[, intersect(c("name", "error_class", "error_message"), names(tbl_err)), drop = FALSE]
    # truncate long messages a bit for printing
    if ("error_message" %in% names(err_view)) {
      err_view$error_message <- substr(err_view$error_message, 1L, 120L)
    }
    print(err_view, row.names = FALSE)
  }

  invisible(x)
}

#' Summary method for compare_curves objects
#'
#' @param object An object of class `"compare_curves"`.
#' @param rank_by Which criterion to rank by. One of `"aicc"`, `"aic"`, `"bic"`.
#'   Default chooses the first available among these in that order.
#' @param ... Unused.
#' @export
summary.compare_curves <- function(object,
                                   rank_by = c("auto", "aicc", "aic", "bic"),
                                   ...) {

  if (!inherits(object, "compare_curves")) {
    rlang::abort(
      message = "`object` must be a `compare_curves` object.",
      class   = "timefitteR_summary_compare_curves_bad_object",
      call    = NULL
    )
  }

  rank_by <- rlang::arg_match(rank_by)
  tbl <- object$table

  # determine ranking metric
  if (rank_by == "auto") {
    if ("aicc" %in% names(tbl) && any(is.finite(tbl$aicc), na.rm = TRUE)) {
      rank_by <- "aicc"
    } else if ("aic" %in% names(tbl) && any(is.finite(tbl$aic), na.rm = TRUE)) {
      rank_by <- "aic"
    } else if ("bic" %in% names(tbl) && any(is.finite(tbl$bic), na.rm = TRUE)) {
      rank_by <- "bic"
    } else {
      rank_by <- NA_character_
    }
  }

  ok <- !is.na(tbl$status) & tbl$status == "ok"
  tbl_ok <- tbl[ok, , drop = FALSE]

  # add delta column (only for the rank metric)
  if (!is.na(rank_by) && nrow(tbl_ok) > 0L) {
    best_score <- min(tbl_ok[[rank_by]], na.rm = TRUE)
    delta_name <- paste0("delta_", rank_by)
    tbl[[delta_name]] <- NA_real_
    tbl[[delta_name]][ok] <- tbl_ok[[rank_by]] - best_score
  }

  # best overall
  best_overall <- NULL

  if (nrow(tbl_ok) > 0L) {
    if (!is.na(rank_by) && rank_by %in% names(tbl_ok) && any(is.finite(tbl_ok[[rank_by]]), na.rm = TRUE)) {
      idx <- which.min(tbl_ok[[rank_by]])
    } else if ("rmse" %in% names(tbl_ok) && any(is.finite(tbl_ok$rmse), na.rm = TRUE)) {
      idx <- order(tbl_ok$complexity, tbl_ok$rmse, na.last = TRUE)[1L]
    } else {
      idx <- order(tbl_ok$complexity, tbl_ok$name, na.last = TRUE)[1L]
    }

    best_overall <- tbl_ok[idx, , drop = FALSE]
  }

  # best per family
  best_by_family <- NULL
  if (!is.na(rank_by) && nrow(tbl_ok) > 0L && "family" %in% names(tbl_ok)) {
    fams <- unique(tbl_ok$family)
    fams <- fams[order(fams)]
    best_rows <- vector("list", length(fams))
    for (i in seq_along(fams)) {
      f <- fams[[i]]
      sub <- tbl_ok[tbl_ok$family == f, , drop = FALSE]
      sub <- sub[order(sub[[rank_by]], sub$complexity, sub$name, na.last = TRUE), , drop = FALSE]
      best_rows[[i]] <- sub[1L, , drop = FALSE]
    }
    best_by_family <- do.call(rbind, best_rows)
    rownames(best_by_family) <- NULL
  }

  out <- list(
    rank_by        = rank_by,
    best_overall   = best_overall,
    best_by_family = best_by_family,
    table          = tbl,
    x              = object$x,
    t              = object$t
  )

  class(out) <- "summary_compare_curves"
  out
}

#' Print method for summary_compare_curves objects
#'
#' @param x An object of class `"summary_compare_curves"`.
#' @param ... Unused.
#' @export
print.summary_compare_curves <- function(x, ...) {
  if (!inherits(x, "summary_compare_curves")) {
    rlang::abort(
      message = "`x` must be a `summary_compare_curves` object.",
      class   = "timefitteR_print_summary_compare_curves_bad_object",
      call    = NULL
    )
  }

  cat("<timefitteR::summary_compare_curves>\n")
  cat("  x: ", x$x$col, ";  t: ", x$t$col, "\n", sep = "")

  if (!is.na(x$rank_by)) {
    cat("  Ranked by: ", toupper(x$rank_by), " (lower is better)\n", sep = "")
  }

  if (!is.null(x$best_overall) && nrow(x$best_overall) == 1L) {
    cat("\n  Best overall model:\n")
    cols <- intersect(
      c("name", "family", "formula_template", "complexity", "k_params_fit", "rmse",
        "r.squared", "aicc", "aic", "bic"),
      names(x$best_overall)
    )
    print(x$best_overall[, cols, drop = FALSE], row.names = FALSE)
  } else {
    cat("\n  No successful fits to summarise.\n")
  }

  if (!is.null(x$best_by_family) && nrow(x$best_by_family) > 0L) {
    cat("\n  Best model within each family:\n")
    cols <- intersect(
      c("family", "name", "complexity", "k_params_fit", "rmse", "aicc", "aic", "bic"),
      names(x$best_by_family)
    )
    print(x$best_by_family[, cols, drop = FALSE], row.names = FALSE)
  }

  invisible(x)
}

