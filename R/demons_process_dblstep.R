#' Process double-step saccades task data from the DEMoNS eye-tracking protocol
#'
#' Calculates quality control metrics and additional parameters for double-step saccades task data obtained using the DEMoNS eye-tracking protocol.
#' Allows selection of essential variables and addition of a prefix to variable names.
#' See [Nij Bijvank et al. 2023](https://doi.org/10.1093/brain/awac474) for details on double-step saccade parameters.
#' See [Nij Bijvank et al. 2018](https://doi.org/10.1371/journal.pone.0200695) for protocol details.
#' DEMoNS protocol task and MATLAB analysis script available at [protocols.io](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3).
#'
#' @inheritParams demons_process_fix
#'
#' @param demons_data_dblstep Dataframe with raw eye-tracker double-step saccades data after processing according to DEMoNS protocol.
#'
#' @returns Dataframe with additional columns indicating quality control metrics and calculated double-step saccade parameters.
#' @export
#'
#' @examples
#' demons_process_dblstep(DEMoNS_data_dblstep, keep_essential = TRUE, add_prefix = TRUE)
#'
demons_process_dblstep <- function(demons_data_dblstep, keep_essential = FALSE, add_prefix = FALSE) {
  if(keep_essential == TRUE) {
    demons_data_dblstep <- demons_data_dblstep %>% dplyr::select(
      dplyr::any_of(c(
        "Research_number", "pat_id",
        "visit", "Visit", "visit_occasion", "occasion",
        "date", "Date",
        # Number of saccades
        "T_Number_DS", "T_Number_FS", "T_Number_SS",

        # Proportions
        "T_Proportion_11_DS", # Correct double-step saccades
        "T_Proportion_12_DS", # Acceptable double-step saccades (T_proportion_acceptable_DS = T_Proportion_11_DS + T_Proportion_12_DS)
        "T_Proportion_13_DS", "T_Proportion_23_DS", # Contraversive double-step saccades (T_Proportion_contraversive_DS = T_Proportion_13_DS+T_Proportion_23_DS)
        "T_Proportion_FSS_to2ndT", # S1 to second target
        "T_Proportion_14_DS", "T_Proportion_24_DS", # Late double-step saccades (T_Proportion_late_DS = T_Proportion_14_DS+T_Proportion_24_DS)

        # Latencies
        "T_Latency_FSS", # First saccade", all
        "T_Latency_corrFSS", # Correct first saccade
        "T_Latency_FSS_to2ndT", # First saccade to second target
        "T_Latency_11_DS", # Correct double-step saccades
        "T_Intersac_interval_11_DS", # Intersaccadic interval, correct

        # Accuracy and velocity
        "T_Directiondiff_FSS", # Direction difference S1 (deg)
        "T_Directiondiff_SSS", # Direction difference S2 (deg)
        "T_Peakvel_FSS", # Peak velocity S1 (deg/s)
        "T_Peakvel_SSS", # Peak velocity S2 (deg/s)
        "T_Amplitude_FSS", # Amplitude S1 (deg)
        "T_Gain_SSS", # Gain S2
        "T_Xerror_SSS", # Horizontal error end position S2 (deg)
        "T_Yerror_SSS", # Vertical error end position S2 (deg)
        "T_XYabserror_FEP", # Absolute error FEP (deg)
        "T_Xerror_FEP", # Horizontal error FEP (deg)
        "T_Yerror_FEP" # Vertical error FEP (deg)
      ))
    )
  }

  demons_data_dblstep <- demons_data_dblstep %>%
    dplyr::mutate(
      T_proportion_acceptable_DS = T_Proportion_11_DS + T_Proportion_12_DS,
      T_Proportion_contraversive_DS = T_Proportion_13_DS + T_Proportion_23_DS,
      T_Proportion_late_DS = T_Proportion_14_DS + T_Proportion_24_DS,
      T_Proportion_11_DS_error = 1 - T_Proportion_11_DS,
      QC_numsac = T_Number_DS >= 30 & T_Number_FS >= 30 & T_Number_SS >= 30, # quality control: at least 50% valid saccades
    )

  if(add_prefix == TRUE) {
    demons_data_dblstep <- demons_data_dblstep %>%
      dplyr::rename_with(~paste0("dblstep_", .),
                         .cols = -dplyr::any_of(c(
                           "Research_number", "pat_id",
                           "visit", "Visit", "visit_occasion", "occasion",
                           "date", "Date"
                         ))
      )
  }

  return(demons_data_dblstep)
}

utils::globalVariables(c(
  "T_Proportion_11_DS", "T_Proportion_12_DS",
  "T_Proportion_13_DS", "T_Proportion_14_DS", "T_Proportion_23_DS",
  "T_Proportion_24_DS"
))

