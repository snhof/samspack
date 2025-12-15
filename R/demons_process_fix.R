#' Process fixation task data from the DEMoNS eye-tracking protocol
#'
#' Calculates additional parameters and dichotomous variables based on established cutoffs for fixation task data obtained using the DEMoNS eye-tracking protocol.
#' Can additionally only retain essential columns and/or add a prefix to all columns except identifier columns.
#' See [Nij Bijvank et al. 2019](https://doi.org/10.1167/iovs.18-26096) for cutoff references.
#' See [Nij Bijvank et al. 2018](https://doi.org/10.1371/journal.pone.0200695) for protocol details.
#' DEMoNS protocol task and MATLAB analysis script available at [protocols.io](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3).
#'
#' @param demons_data_fix Dataframe with fixation task data obtained using the DEMoNS eye-tracking protocol.
#' @param keep_essential Logical indicating whether to only keep essential identifier and outcome columns (default = FALSE).
#' @param add_prefix Logical indicating whether to add "fix_" prefix to all columns except identifier columns (default = FALSE).
#'
#' @returns Dataframe with additional columns for fixation task parameters and dichotomous variables based on established cutoffs.
#' @export
#'
#' @examples
#' demons_process_fix(DEMoNS_data_fix, keep_essential = TRUE, add_prefix = TRUE)
#'
demons_process_fix <- function(demons_data_fix, keep_essential = FALSE, add_prefix = FALSE) {
if(keep_essential == TRUE) {
  demons_data_fix <- demons_data_fix %>% dplyr::select(
    dplyr::any_of(c(
      "Research_number", "pat_id",
      "visit", "Visit", "visit_occasion", "occasion",
      "date", "Date",
      "T_Number_fixperiods",
      "C_SD_X_gaze", "C_Fix_SD_X_gaze",
      "C_SD_Y_gaze", "C_Fix_SD_Y_gaze",
      "C_BCEA_gaze", "C_Fix_BCEA_gaze",
      "C_Mean_amplitude_SWJ", "C_Mean_amplitude_sac", "C_Mean_amplitude_SWJSac",
      "C_MeanNr_SWJmin4_persec", "C_MeanNr_SWJplus4_persec",
      "C_Mean_sacmin2_sec", "C_Mean_sacplus2_sec",
      "C_Fix_median_absvel",
      "C_Linfitcoef_X_gaze", "C_Lin_SE_X_gaze",
      "C_Linfitcoef_Y_gaze", "C_Lin_SE_Y_gaze"
    ))
  )
}
  demons_data_fix <- demons_data_fix %>%
    dplyr::mutate(
      C_MeanNr_SWJ_persec = C_MeanNr_SWJmin4_persec + C_MeanNr_SWJplus4_persec, #total SWJ frequency
      C_Mean_sac_sec = C_Mean_sacmin2_sec + C_Mean_sacplus2_sec, #total saccade frequency
      C_abs_Linfitcoef_X_gaze = abs(C_Linfitcoef_X_gaze), #absolute drift X
      C_abs_Linfitcoef_Y_gaze = abs(C_Linfitcoef_Y_gaze), #absolute drift Y
      C_Mean_amplitude_SWJ = dplyr::if_else(C_MeanNr_SWJ_persec==0, NA, C_Mean_amplitude_SWJ), #set amplitude to NA if no SWJ detected
      C_Mean_amplitude_sac = dplyr::if_else(C_Mean_sac_sec==0, NA, C_Mean_amplitude_sac), #set amplitude to NA if no saccades detected
      #apply thresholds:
      ## SWJ:
      C_SWJ_freq_dich = dplyr::case_when(
        C_MeanNr_SWJ_persec > 1.29  ~ TRUE,
        C_MeanNr_SWJ_persec <= 1.29 ~ FALSE,
        TRUE~NA
      ),
      C_SWJ_amp_dich = dplyr::case_when(
        C_Mean_amplitude_SWJ > 1.10  ~ TRUE,
        C_Mean_amplitude_SWJ <= 1.10 ~ FALSE,
        TRUE~NA
      ),
      C_SWJ_dich = dplyr::case_when(
        C_SWJ_freq_dich | C_SWJ_amp_dich ~ TRUE,
        !C_SWJ_freq_dich & !C_SWJ_amp_dich ~ FALSE,
        TRUE~NA
      ),
      ## Saccades:
      C_sac_freq_dich = dplyr::case_when(
        C_Mean_sac_sec > 1.54  ~ TRUE,
        C_Mean_sac_sec <= 1.54 ~ FALSE,
        TRUE~NA
      ),
      C_sac_amp_dich = dplyr::case_when(
        C_Mean_amplitude_sac > 2.00  ~ TRUE,
        C_Mean_amplitude_sac <= 2.00 ~ FALSE,
        TRUE~NA
      ),
      C_sac_dich = dplyr::case_when(
        C_sac_freq_dich | C_sac_amp_dich ~ TRUE,
        !C_sac_freq_dich & !C_sac_amp_dich ~ FALSE,
        TRUE~NA
      ),
      ## saccadic intrusion (SWJ + saccades),
      C_SWJsac_dich = dplyr::case_when(
        C_SWJ_dich | C_sac_dich ~ TRUE,
        !C_SWJ_dich & !C_sac_dich ~ FALSE,
        TRUE~NA
      ),
      ## Drift:
      C_drift_X_dich = dplyr::case_when(
        C_abs_Linfitcoef_X_gaze > 0.28  ~ TRUE,
        C_abs_Linfitcoef_X_gaze <= 0.28 ~ FALSE,
        TRUE~NA
      ),
      C_drift_Y_dich = dplyr::case_when(
        C_abs_Linfitcoef_Y_gaze > 0.27  ~ TRUE,
        C_abs_Linfitcoef_Y_gaze <= 0.27 ~ FALSE,
        TRUE~NA
      ),
      C_drift_dich = dplyr::case_when(
        C_drift_X_dich | C_drift_Y_dich ~ TRUE,
        !C_drift_X_dich & !C_drift_Y_dich ~ FALSE,
        TRUE~NA
      ),

      ## fixational instability:
      C_fix_instab_X_dich = dplyr::case_when(
        C_Lin_SE_X_gaze > 0.30  ~ TRUE,
        C_Lin_SE_X_gaze <= 0.30 ~ FALSE,
        TRUE~NA
      ),
      C_fix_instab_Y_dich = dplyr::case_when(
        C_Lin_SE_Y_gaze > 0.34  ~ TRUE,
        C_Lin_SE_Y_gaze <= 0.34 ~ FALSE,
        TRUE~NA
      ),
      C_fix_instab_dich = dplyr::case_when(
        C_fix_instab_X_dich | C_fix_instab_Y_dich ~ TRUE,
        !C_fix_instab_X_dich & !C_fix_instab_Y_dich ~ FALSE,
        TRUE~NA
      )
    ) %>%
    dplyr::mutate(
      QC_nfix = T_Number_fixperiods >= 5, #quality control: at least 50% of fixation periods valid
    )

  if(add_prefix == TRUE) {
    demons_data_fix <- demons_data_fix %>%
      dplyr::rename_with(~paste0("fix_", .),
                  .cols = -dplyr::any_of(c(
                    "Research_number", "pat_id",
                    "visit", "Visit", "visit_occasion", "occasion",
                    "date", "Date"
                  ))
      )
  }

  return(demons_data_fix)
}

utils::globalVariables(c(
  "C_Linfitcoef_X_gaze", "C_Linfitcoef_Y_gaze", "C_MeanNr_SWJ_persec",
  "C_MeanNr_SWJmin4_persec", "C_MeanNr_SWJplus4_persec", "C_Mean_amplitude_SWJ",
  "C_Mean_amplitude_sac", "C_Mean_sac_sec", "C_Mean_sacmin2_sec",
  "C_Mean_sacplus2_sec", "T_Number_DS", "T_Number_FS", "T_Number_SS",
  "T_Number_fixperiods"
))

