#' Internal Function to generate a sequence of time points in a semi-deterministic way
#'
#' This function is only called internally
#'
#' @param id a character vector to use as an id for this participant
#' @param intercept default value of attitude at t = 0
#' @param slope default 1st order regression coefficient
#' @param curvature default 2nd order regression coefficient
#' @param noise default SD of noise added to attitude at each timepoint
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
#' @return A tibble of the form |Timepoint|id|gender|ses|attitude|
#'
#' @examples
#' generate_attitude_sequence('a')
#'
generate_attitude_sequence <- function(id,
                                       intercept = 0,
                                       slope = 1,
                                       curvature = 0,
                                       noise = 2,
                                       gender = "Boy",
                                       ses = "Low",
                                       from = 0,
                                       to = 10,
                                       gender_intercept_offset = 1,
                                       gender_intercept_noise = 1,
                                       gender_slope_offset = -0.1,
                                       gender_slope_noise = 1,
                                       medium_intercept_offset = 0.5,
                                       medium_intercept_noise = 1,
                                       medium_slope_offset = 0.05,
                                       medium_slope_noise = 1,
                                       high_intercept_offset = 1.5,
                                       high_intercept_noise = 1,
                                       high_slope_offset = 0.075,
                                       high_slope_noise = 1) {
  ## Set some fixed noise values for this participant i.e. participant level random effect
  a_noise <- rnorm(1, 0, noise / 2)
  b_noise <- rnorm(1, 0, noise / 10)
  c_noise <- rnorm(1, 0, noise / 50)

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
