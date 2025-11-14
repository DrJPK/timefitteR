#' Generate a sequence of time points in a semi-deterministic way
#'
#' @description
#'
#' This function should usually be **called internally**. This function generates a sequence of values (scores) using a defined quadratic function. Random, normal noise is added to each of the coefficients according to the value of noise and noise_ratio. Additional time dependent noise is added to each value
#'
#' @details
#' The default model has the following properties with respect to time:
#' - Data have an initial intercept of 0
#' - Data develop at 1.0 point per time period
#' - No curvature is expected.
#' - Default noise is drawn randomly from a normal distribution of mean=0 and sd=2 and applied at EVERY time point separately. This models those random factors that vary over time.
#' - Further default noise is applied to the participant's individual initial intercept, slope and curvature to model those factors that vary per participant. By default these are drawn randomly from normal distributions with mean=0 and sd=1.00 (intercept: 2 &times; 0.5), sd=0.20 (slope: 2 &times; 0.1), sd=0.04 (curvature: 2 &times; 0.02)
#'
#' Returns a tibble in the form:
#' | timepoint | attitude |
#' |-----------|----------|
#' | 1         | 0.234534 |
#'
#' @param intercept default value of attitude at t = 0
#' @param slope default 1st order regression coefficient
#' @param curvature default 2nd order regression coefficient
#' @param noise default SD of noise added to attitude at each timepoint
#' @param noise_ratio relative strengths of noise applied to initial intercept, slope and curvature BEFORE other factors
#' @param from first time point
#' @param length number of time points to generate
#'
#' @return A tibble of sequence data in long format
#' @export
#'
#' @importFrom tibble tibble
#'
#' @examples
#' generate_timedata_sequence()
#'
generate_timedata_sequence <- function(intercept = 0,
                                       slope = 1,
                                       curvature = 0,
                                       noise = 2,
                                       noise_ratio = c(0.5, 0.1, 0.02),
                                       from = 0,
                                       length = 10) {
  stopifnot(is.numeric(from), is.numeric(length))

  tryCatch(
    {
      length <- as.integer(length)
    },
    error = function(e) {
      message("Length could not be coerced to integer")
    }
  )
  tryCatch(
    {
      from <- as.integer(from)
    },
    error = function(e) {
      message("From could not be coerced to integer")
    }
  )

  stopifnot(length(noise_ratio) == 3, length >= 1, noise >= 0)
  ## Set some fixed noise values for this participant i.e. participant level random effect
  a_noise <- rnorm(1, 0, noise * noise_ratio[1])
  b_noise <- rnorm(1, 0, noise * noise_ratio[2])
  c_noise <- rnorm(1, 0, noise * noise_ratio[3])

  ## Make the tibble
  df <- tibble::tibble(
    timepoint = as.integer(seq(from, from + length, by = 1)),
    attitude = (intercept + a_noise) + (slope + b_noise) * timepoint + (curvature + c_noise) * timepoint^
      2 + rnorm(length(timepoint), 0, noise)
  )
  return(df)
}
