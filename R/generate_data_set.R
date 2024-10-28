#' Generate a synthetic dataframe for a 2x2x3 longitudinal design
#'
#' This function can be called to generate a semi-deterministic synthetic data set for a 2 Group{Control,Intervention} &times; 2 Gender{Boy, Girl} &times; 3 SES{Low,Medium,High} design. The purpose of this is to produce data for testing the other functions in timefitteR.
#'
#' @param participants Number of participants to generate in the data set
#' @param groups column vector of names for groups
#' @param balance column vector, of same length as groups, with the relative number of participants per group. If the sum of balance is not an integer divisor of participants, then participants will be increased until this condition is met.
#' @param intercept_difference how much should be added (in scale points) to the value of initial intercept for group 2 (intervention) etc compared to group 1 (control)? This vector must have length 1 less than groups.
#' @param slope_difference How much should be added (in scale points per time period) to the slope of the intervention groups compared to the control group? This vector must have length 1 less than groups.
#' @param curvature_difference How much should be added (in scale points per time period squared) to the curvature of the intervention groups compared to the control group? This vector must have length 1 less than groups.
#' @param noise_difference_multiplier What multiplier should be attached to the intervention groups compared to the control groups? i.e. how much noisier should the the intervention group be than the control group? This vector must have length 1 less than groups.
#' @param gender_slope_difference How much should be added to the Girls group for each intervention compared to the Boys group for the intervention in scale points per time period? i.e. how much more should the intervention affect Girls than Boys? This vector must have length 1 less than groups.
#' @param gender_slope_noise_multiplier What multiplier should be used to increase the noise of the girls intervention group compared to the Boys intervention group? i.e. how much noisier should the girls intervention group be than the boys? This vector must have length 1 less than groups.
#' @param medium_slope_difference How much should be added to the Medium SES group for each intervention compared to the Low SES group for the intervention in scale points per time period? i.e. how much more should the intervention affect Medium group than the Low group? This vector must have length 1 less than groups.
#' @param medium_slope_noise_multiplier What multiplier should be used to increase the noise of the Medium SES intervention group compared to the Low SES intervention group? i.e. how much noisier should the Medium intervention group be than the Low group? This vector must have length 1 less than groups.
#' @param high_slope_difference How much should be added to the High SES group for each intervention compared to the Low SES group for the intervention in scale points per time period? i.e. how much more should the intervention affect High group than the Low group? This vector must have length 1 less than groups.
#' @param high_slope_noise_muliplier What multiplier should be used to increase the noise of the High SES intervention group compared to the Low SES intervention group? i.e. how much noisier should the High intervention group be than the Low group? This vector must have length 1 less than groups.
#' @param ... any other parameters to pass to [generate_attitude_sequence]
#'
#'
#' @return A long and tidy tibble
#' @export
#'
#'
#' @examples
#' x<-generate_data_set()

generate_data_set <- function(participants,
                              groups = c("control", "intervention"),
                              balance = c(1, 1),
                              intercept_difference = c(0),
                              slope_difference = c(1),
                              curvature_difference = c(0),
                              noise_difference_multiplier = c(1),
                              gender_slope_difference = c(1),
                              gender_slope_noise_multiplier = c(1),
                              medium_slope_difference = c(0),
                              medium_slope_noise_multiplier = c(1),
                              high_slope_difference = c(0),
                              high_slope_noise_muliplier = c(1),
                              ...) {
  stopifnot(is.numeric(participants))
  ##Check that groups and balance matches
  if (length(groups) != length(balance)) {
    if (length(groups) > length(balance)) {
      for (i in length(balance):length(groups)) {
        balance[i] <- balance[length(balance)]
      }
      cli::cli_alert_warning(
        paste0("The length of balance does not match the length of groups\nThe last element of balance has been recycled. If this is not what you wanted then try again\nThe value of balance being used is: (",
              paste0(balance, collapse=","),")")
      )
    } else{
      balance <- balance[1:length(groups)]
      cli::cli_alert_warning(
        paste0(
          "The length of balance does not match the length of groups\nThe elements of balance have been trimmed to match groups. If this is not what you wanted then try again\nThe value of balance being used is: (",
          paste0(balance, collapse=","),")")

      )
    }
  }
  ##Check that participants and balance are compatible
  if (participants %% sum(balance) != 0) {
    participants <- participants + sum(balance) - participants %% sum(balance)
    cli::cli_alert_warning(
      paste(
        "The number of participants is not divisable by the balance ratio. The number of participants has been increased to:",
        participants
      )
    )

  }
  #Clean up any params fed to the function
  n <- length(balance)-1
  intercept_difference = timefitteR:::fix_param(intercept_difference,n)
  slope_difference = timefitteR:::fix_param(slope_difference,n)
  curvature_difference = timefitteR:::fix_param(curvature_difference,n)
  noise_difference_multiplier = timefitteR:::fix_param(noise_difference_multiplier,n)
  gender_slope_difference = timefitteR:::fix_param(gender_slope_difference,n)
  gender_slope_noise_multiplier = timefitteR:::fix_param(gender_slope_noise_multiplier,n)
  medium_slope_difference = timefitteR:::fix_param(medium_slope_difference,n)
  medium_slope_noise_multiplier = timefitteR:::fix_param(medium_slope_noise_multiplier,n)
  high_slope_difference = timefitteR:::fix_param(high_slope_difference,n)
  high_slope_noise_muliplier = timefitteR:::fix_param(high_slope_noise_muliplier,n)


}
