.timefitteR_coerce_x_numeric <- function(x,
                                        prefix = NULL,
                                        suffix = NULL) {

  col_name <- rlang::as_label(rlang::enexpr(x))

  # already numeric: nothing to do
  if (is.numeric(x)) {
    return(x)
  }

  # factor -> numeric codes based on level order
  if (is.factor(x)) {
    rlang::inform(
      glue::glue(
        "Column `{col_name}` is a factor; converting to numeric codes based on factor levels."
      ),
      class = "timefitteR_x_coerced_factor"
    )
    return(as.numeric(x))
  }

  # character -> strip prefix/suffix, then coerce
  if (is.character(x)) {
    x_trim <- x

    if (!is.null(prefix) && nzchar(prefix)) {
      # remove prefix only if it actually appears at the start
      x_trim <- ifelse(
        startsWith(x_trim, prefix),
        substr(x_trim, nchar(prefix) + 1L, nchar(x_trim)),
        x_trim
      )
    }

    if (!is.null(suffix) && nzchar(suffix)) {
      # remove suffix only if it actually appears at the end
      x_trim <- ifelse(
        endsWith(x_trim, suffix),
        substr(x_trim, 1L, nchar(x_trim) - nchar(suffix)),
        x_trim
      )
    }

    x_trim <- trimws(x_trim)

    # coerce to numeric and detect new NAs
    suppressWarnings(num <- as.numeric(x_trim))

    non_missing_input <- !is.na(x_trim) & nzchar(x_trim)
    new_na <- is.na(num) & non_missing_input

    if (any(new_na)) {
      rlang::abort(
        message = glue::glue(
          "Column `{col_name}` could not be coerced to numeric even after ",
          "trimming prefix/suffix. I give up."
        ),
        class = "timefitteR_x_not_numeric",
        call  = NULL
      )
    }

    rlang::inform(
      glue::glue(
        "Column `{col_name}` was coerced from character to numeric ",
        "after trimming prefix/suffix."
      ),
      class = "timefitteR_x_coerced_character"
    )

    return(num)
  }

  # anything else is not acceptable
  rlang::abort(
    message = glue::glue(
      "Column `{col_name}` must be numeric, factor, or character; got `{class(x)[1L]}`."
    ),
    class = "timefitteR_x_not_numeric",
    call  = NULL
  )
}
