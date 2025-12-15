#' Build the model registry
#'
#' @description
#' Internal constructor for the model registry used by `generate_models()`.
#' The registry defines candidate curve families, their formula templates
#' (as strings in canonical `x` and `t`), and minimal constraints for time.
#'
#' @return A data.frame describing candidate models.
#'
#' @keywords internal
.build_model_registry <- function() {

  # helper to define one row (keeps it easy to extend later)
  row <- function(name,
                  family,
                  formula_template,
                  complexity,
                  k_params,
                  allows_t0,
                  allows_t_negative,
                  interpretation) {
    data.frame(
      name              = as.character(name),
      family            = as.character(family),
      formula_template  = as.character(formula_template),
      complexity        = as.integer(complexity),
      k_params          = as.integer(k_params),
      allows_t0         = as.logical(allows_t0),
      allows_t_negative = as.logical(allows_t_negative),
      interpretation    = as.character(interpretation),
      stringsAsFactors  = FALSE
    )
  }

  reg <- rbind(

    # ------------------------------------------------------------
    # Tier 0: Baseline
    # ------------------------------------------------------------
    row(
      name              = "baseline_intercept",
      family            = "baseline",
      formula_template  = "x ~ 1",
      complexity        = 0L,
      k_params          = 1L,
      allows_t0         = TRUE,
      allows_t_negative = TRUE,
      interpretation    = "No systematic change in <<x>> over time (intercept-only)."
    ),

    # ------------------------------------------------------------
    # Tier 1: Simple monotonic change
    # ------------------------------------------------------------

    # Linear trend
    row(
      name              = "linear",
      family            = "polynomial",
      formula_template  = "x ~ t",
      complexity        = 1L,
      k_params          = 2L,
      allows_t0         = TRUE,
      allows_t_negative = TRUE,
      interpretation    = "<<x>> changes linearly with <<t>>."
    ),

    # Log time (requires strictly positive t)
    row(
      name              = "log_time",
      family            = "logarithmic",
      formula_template  = "x ~ log(t)",
      complexity        = 1L,
      k_params          = 2L,
      allows_t0         = FALSE,
      allows_t_negative = FALSE,
      interpretation    = "Diminishing returns: rapid early change in <<x>> that slows with <<t>> (log scale)."
    ),

    # Fixed power laws (fractional) — require non-negative t
    row(
      name              = "sqrt_time",
      family            = "power_fixed",
      formula_template  = "x ~ I(t^(1/2))",
      complexity        = 1L,
      k_params          = 2L,
      allows_t0         = TRUE,
      allows_t_negative = FALSE,
      interpretation    = "Diminishing returns: <<x>> increases with sqrt(<<t>>), slowing over time."
    ),
    row(
      name              = "cuberoot_time",
      family            = "power_fixed",
      formula_template  = "x ~ I(t^(1/3))",
      complexity        = 1L,
      k_params          = 2L,
      allows_t0         = TRUE,
      allows_t_negative = FALSE,
      interpretation    = "Strong diminishing returns: <<x>> increases with cbrt(<<t>>), slowing substantially over time."
    ),

    # Exponential forms
    row(
      name              = "exp_growth",
      family            = "exponential",
      formula_template  = "x ~ exp(t)",
      complexity        = 2L,
      k_params          = 2L,
      allows_t0         = TRUE,
      allows_t_negative = TRUE,
      interpretation    = "Exponential growth pattern in <<x>> as <<t>> increases."
    ),
    row(
      name              = "exp_decay",
      family            = "exponential",
      formula_template  = "x ~ exp(-t)",
      complexity        = 2L,
      k_params          = 2L,
      allows_t0         = TRUE,
      allows_t_negative = TRUE,
      interpretation    = "Exponential decay pattern in <<x>> as <<t>> increases."
    ),

    # ------------------------------------------------------------
    # Tier 2: Curvature & saturation
    # ------------------------------------------------------------

    # Polynomial curvature
    row(
      name              = "quadratic",
      family            = "polynomial",
      formula_template  = "x ~ t + I(t^2)",
      complexity        = 2L,
      k_params          = 3L,
      allows_t0         = TRUE,
      allows_t_negative = TRUE,
      interpretation    = "Curvilinear change in <<x>> over <<t>> (quadratic)."
    ),
    row(
      name              = "cubic",
      family            = "polynomial",
      formula_template  = "x ~ t + I(t^2) + I(t^3)",
      complexity        = 3L,
      k_params          = 4L,
      allows_t0         = TRUE,
      allows_t_negative = TRUE,
      interpretation    = "Curvilinear change with possible inflection in <<x>> over <<t>> (cubic)."
    ),

    # Saturating transform (lm-safe proxy; t must not cross -1)
    row(
      name              = "saturating_hyperbola",
      family            = "saturating",
      formula_template  = "x ~ I(t / (1 + t))",
      complexity        = 2L,
      k_params          = 2L,
      allows_t0         = TRUE,
      allows_t_negative = FALSE,
      interpretation    = "Saturating increase: <<x>> rises quickly then slows toward a plateau as <<t>> increases."
    ),

    # ------------------------------------------------------------
    # Nonlinear placeholders (planned; not lm-safe yet)
    # ------------------------------------------------------------
    row(
      name              = "logistic",
      family            = "sigmoid",
      formula_template  = "x ~ logistic(t)",
      complexity        = 4L,
      k_params          = NA_integer_,
      allows_t0         = TRUE,
      allows_t_negative = TRUE,
      interpretation    = "S-shaped change in <<x>> over <<t>> with an upper (and lower) bound (logistic)."
    ),
    row(
      name              = "gompertz",
      family            = "sigmoid",
      formula_template  = "x ~ gompertz(t)",
      complexity        = 4L,
      k_params          = NA_integer_,
      allows_t0         = TRUE,
      allows_t_negative = TRUE,
      interpretation    = "Asymmetric S-shaped change in <<x>> over <<t>> approaching an upper bound (Gompertz)."
    )
  )


  # Ensure stable ordering for predictable behaviour
  reg <- reg[order(reg$complexity, reg$family, reg$name), , drop = FALSE]
  rownames(reg) <- NULL
  reg
}
