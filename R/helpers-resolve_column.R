#' Resolve a column reference to a column name
#'
#' @description
#' Internal helper used to resolve a column reference supplied as a bare name,
#' string, or bare integer index into a valid column name in a data.frame.
#'
#' @details
#' This function is intentionally conservative. It supports:
#' \itemize{
#'   \item bare column names (e.g., x = score),
#'   \item string column names (e.g., x = "score"),
#'   \item bare integer indices (e.g., x = 1),
#'   \item defaults when the argument is missing.
#' }
#'
#' It does not support tidyselect helpers or expressions.
#'
#' @param arg The column reference (unevaluated)
#' @param data A data.frame
#' @param arg_label Character scalar used for error messages ("x", "t", etc.)
#' @param default_idx Integer index to use if `arg` is missing
#'
#' @return A single character string giving a column name
#'
#' @keywords internal
.resolve_column <- function(arg,
                            data,
                            arg_label = "x",
                            default_idx = 1L) {

  # default when argument not supplied
  if (missing(arg)) {
    if (ncol(data) < default_idx) {
      rlang::abort(
        message = glue::glue(
          "Argument `data` must have at least {default_idx} columns ",
          "to default `{arg_label}`."
        ),
        class = "timefitteR_resolve_column_data_error",
        call  = NULL
      )
    }
    return(names(data)[default_idx])
  }

  arg_quo  <- rlang::enquo(arg)
  arg_expr <- rlang::get_expr(arg_quo)

  # ------------------------------------------------------------------
  # 1) Bare integer index (e.g., x = 1)
  # ------------------------------------------------------------------
  if (rlang::is_scalar_integerish(arg_expr)) {
    idx <- as.integer(arg_expr)

    if (is.na(idx) || idx < 1L || idx > ncol(data)) {
      rlang::abort(
        message = glue::glue(
          "Argument `{arg_label}` as a column index must be between ",
          "1 and {ncol(data)}; got {idx}."
        ),
        class = glue::glue("timefitteR_resolve_column_bad_{arg_label}"),
        call  = NULL
      )
    }

    return(names(data)[idx])
  }

  # ------------------------------------------------------------------
  # 2) String column name (e.g., x = "score")
  # ------------------------------------------------------------------
  if (rlang::is_string(arg_expr)) {
    col <- arg_expr

    if (!col %in% names(data)) {
      rlang::abort(
        message = glue::glue(
          "Column `{col}` specified for `{arg_label}` not found in `data`."
        ),
        class = glue::glue("timefitteR_resolve_column_bad_{arg_label}"),
        call  = NULL
      )
    }

    return(col)
  }

  # ------------------------------------------------------------------
  # 3) Bare name / symbol (e.g., x = score)
  # ------------------------------------------------------------------
  col <- tryCatch(
    rlang::as_string(rlang::ensym(arg)),
    error = function(e) NA_character_
  )

  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    rlang::abort(
      message = glue::glue(
        "Argument `{arg_label}` must be a bare column name, ",
        "a single string column name, or a single integer column index."
      ),
      class = glue::glue("timefitteR_resolve_column_bad_{arg_label}"),
      call  = NULL
    )
  }

  if (!col %in% names(data)) {
    rlang::abort(
      message = glue::glue(
        "Column `{col}` specified for `{arg_label}` not found in `data`."
      ),
      class = glue::glue("timefitteR_resolve_column_bad_{arg_label}"),
      call  = NULL
    )
  }

  col
}
