#' Generate a sequence of time points in a semi-deterministic way
#'
#'@description
#'
#' This function should only be **called internally**. This function generates a sequence of values (attitude scores) using a defined quadratic function. Random, normal noise is added to each of the coefficients according to the value of noise and noise_ratio. Further explicitly defined modifiers are applied to intercept and slope depending on gender and SES levels. Noise multipliers can be also defined by these factor variables.
#'
#'@details
#' The default model has the following properties with respect to time:
#' - Both boys and girls have an initial attitude of 0
#' - Low SES students have an initial attitude of 0, while medium SES students have an attitude of 0.5 and high SES students have an attitude of 0.8.
#' - Boys develop at 1.0 point per time period while girls develop at 0.9 (1-0.1) points per time period
#' - Low SES students develop at 1.0 points per time period while medium SES develop at 1.05 points per time period and high SES develop at 1.075 points per time period.
#' - No curvature is expected.
#' - Default noise is drawn randomly from a normal distribution of mean=0 and sd=2 and applied at EVERY time point separately. This models those random factors that vary over time.
#' - Further default noise is applied to the participant's individual initial intercept, slope and curvature to model those factors that vary per participant. By default these are drawn randomly from normal distributions with mean=0 and sd=1.00 (intercept: 2 &times; 0.5), sd=0.20 (slope: 2 &times; 0.1), sd=0.04 (curvature: 2 &times; 0.02)
#' - All additional multipliers for noise due to gender and SES are set to 1. i.e. SES and Gender do not account for any increased noise
#'
#' Returns a tibble in the form:
#' | timepoint | id | gender | ses | attitude |
#' |-----------|----|--------|-----|----------|
#' | 1 | a | Boy | Low | 0.234534 |
#'
#' @param id a character vector to use as an id for this participant
#' @param intercept default value of attitude at t = 0
#' @param slope default 1st order regression coefficient
#' @param curvature default 2nd order regression coefficient
#' @param noise default SD of noise added to attitude at each timepoint
#' @param noise_ratio relative strengths of noise applied to initial intercept, slope and curvature BEFORE other factors
#' @param gender a factor string for this participant Boy | Girl
#' @param ses a factor string for this participant Low | Medium | High
#' @param from first time point
#' @param to last time point
#' @param gender_intercept_offset Constant that is added to the noise calculations for Girls
#' @param gender_intercept_noise Multiplier that is applied to the noise component of the intercept for Girls
#' @param gender_slope_offset Constant that is added to the slope calculations for Girls
#' @param gender_slope_noise Multiplier that is applied to the noise component of the slope calculations for Girls
#' @param medium_intercept_offset Constant that is added to the noise calculations for medium ses participant
#' @param medium_intercept_noise Multiplier that is applied to the noise component of the intercept for medium ses participant
#' @param medium_slope_offset Constant that is added to the slope calculations for medium ses participant
#' @param medium_slope_noise Multiplier that is applied to the noise component of the slope calculations for medium ses participant
#' @param high_intercept_offset Constant that is added to the noise calculations for high ses participant
#' @param high_intercept_noise Multiplier that is applied to the noise component of the intercept for high ses participant
#' @param high_slope_offset Constant that is added to the slope calculations for high ses participant
#' @param high_slope_noise Multiplier that is applied to the noise component of the slope calculations for high ses participant
#'
#' @return A tibble of sequence data in long format
#'
#' @examples
#' generate_attitude_sequence('a')
#'
generate_attitude_sequence <- function(id,
                                       intercept = 0,
                                       slope = 1,
                                       curvature = 0,
                                       noise = 2,
                                       noise_ratio = c(0.5,0.1,0.02),
                                       gender = "Boy",
                                       ses = "Low",
                                       from = 0,
                                       to = 10,
                                       gender_intercept_offset = 0,
                                       gender_intercept_noise = 1,
                                       gender_slope_offset = -0.1,
                                       gender_slope_noise = 1,
                                       medium_intercept_offset = 0.5,
                                       medium_intercept_noise = 1,
                                       medium_slope_offset = 0.05,
                                       medium_slope_noise = 1,
                                       high_intercept_offset = 0.8,
                                       high_intercept_noise = 1,
                                       high_slope_offset = 0.075,
                                       high_slope_noise = 1) {
  ## Set some fixed noise values for this participant i.e. participant level random effect
  a_noise <- rnorm(1, 0, noise * noise_ratio[1])
  b_noise <- rnorm(1, 0, noise * noise_ratio[2])
  c_noise <- rnorm(1, 0, noise * noise_ratio[3])

  ## Modify the random effects based on factor levels

  if (gender == "Girl") {
    a_noise <- a_noise * gender_intercept_noise + gender_intercept_offset
    b_noise <- b_noise * gender_slope_noise + gender_slope_offset
  }

  if (ses == "Medium") {
    a_noise <- a_noise * medium_intercept_noise + medium_intercept_offset
    b_noise <- b_noise * medium_slope_noise + medium_slope_offset
  }

  if (ses == "High") {
    a_noise <- a_noise * high_intercept_noise + high_intercept_offset
    b_noise <- b_noise * high_slope_noise + high_slope_offset
  }

  ## Make the tibble
  df <- tibble::tibble(
    id = as.character(id),
    timepoint = as.integer(seq(from, to, by = 1)),
    gender = factor(gender, levels = c("Boy", "Girl")),
    ses = factor(ses, levels = c("Low", "Medium", "High")),
    attitude = (intercept + a_noise) + (slope + b_noise) * timepoint + (curvature + c_noise) * timepoint^2 + rnorm(length(timepoint), 0, noise)
  )
  return(df)
}
