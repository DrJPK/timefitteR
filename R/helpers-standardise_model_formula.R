#' Standardise a model formula to use canonical `x` and `t`
#'
#' @keywords internal
.standardise_model_formula <- function(model, x_col, t_col) {

  if (!inherits(model, "formula")) {
    rlang::abort(
      message = "Argument `model` must be a formula, e.g. `x ~ t`.",
      class   = "fit_curve_model_error",
      call    = NULL
    )
  }

  # --- LHS validation (before rewrite) ---------------------------------
  lhs <- model[[2]]

  if (!rlang::is_symbol(lhs)) {
    rlang::abort(
      message = "The left-hand side of `model` must be a single variable (no transformations).",
      class   = "fit_curve_model_bad_lhs",
      call    = NULL
    )
  }

  lhs_name <- rlang::as_string(lhs)
  if (!lhs_name %in% c("x", x_col)) {
    rlang::abort(
      message = glue::glue(
        "The left-hand side of `model` must be `x` (or `{x_col}`); got `{lhs_name}`."
      ),
      class   = "fit_curve_model_bad_lhs",
      call    = NULL
    )
  }

  # --- variable validation (whole formula) ------------------------------
  vars <- all.vars(model)
  allowed <- unique(c("x", "t", x_col, t_col))

  unknown <- setdiff(vars, allowed)
  if (length(unknown) > 0L) {
    rlang::abort(
      message = glue::glue(
        "Argument `model` contains variable(s) not recognised for this fit: ",
        "{paste0('`', unknown, '`', collapse = ', ')}. ",
        "Use `x` and `t`, or the column names `", x_col, "` and `", t_col, "`."
      ),
      class = "fit_curve_model_unknown_vars",
      call  = NULL
    )
  }

  # --- deep replacement of symbols to canonical names -------------------
  replace_sym <- function(expr) {
    if (rlang::is_symbol(expr)) {
      nm <- rlang::as_string(expr)
      if (nm == x_col) return(rlang::sym("x"))
      if (nm == t_col) return(rlang::sym("t"))
      return(expr)
    }

    if (rlang::is_call(expr)) {
      expr[] <- lapply(as.list(expr), replace_sym)
      return(as.call(expr))
    }

    if (is.pairlist(expr)) {
      return(as.pairlist(lapply(expr, replace_sym)))
    }

    expr
  }

  f <- model
  f[[2]] <- replace_sym(f[[2]])
  f[[3]] <- replace_sym(f[[3]])

  # After standardisation, enforce canonical LHS exactly `x`
  if (rlang::as_string(f[[2]]) != "x") {
    rlang::abort(
      message = "After standardisation, the left-hand side of `model` must be `x`.",
      class   = "fit_curve_model_bad_lhs",
      call    = NULL
    )
  }

  # Inform only if we actually changed something
  if (!identical(f, model)) {
    rlang::inform(
      message = glue::glue(
        "Standardised `model` formula by mapping `{x_col}` -> `x` and `{t_col}` -> `t`."
      ),
      class = "timefitteR_fit_curve_model_standardised"
    )
  }

  f
}
