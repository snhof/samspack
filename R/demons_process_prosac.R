#' Process pro-saccades task data from the DEMoNS eye-tracking protocol
#'
#' Calculates INO parameters and quality control metrics for pro-saccades task data obtained using the DEMoNS eye-tracking protocol.
#' Allows selection of essential variables and addition of a prefix to variable names.
#' See also [demons_process_INO()] for INO calculation details.
#' See [Nij Bijvank et al. 2018](https://doi.org/10.1371/journal.pone.0200695) for protocol details.
#' DEMoNS protocol task and MATLAB analysis script available at [protocols.io](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3).
#'
#' @inheritParams demons_process_fix
#' @param demons_data_prosac Dataframe with raw eye-tracker pro-saccades data after processing according to DEMoNS protocol.
#'
#' @returns Dataframe with additional columns indicating presence of INO based on AUC and PvAm metrics, as well as quality control metrics and VDI calculations.
#' @export
#'
#' @examples
#' demons_process_prosac(DEMoNS_data_prosac, keep_essential = TRUE, add_prefix = TRUE)
demons_process_prosac <- function(demons_data_prosac, keep_essential = FALSE, add_prefix = FALSE) {
  if(keep_essential == TRUE) {
    demons_data_prosac <- demons_data_prosac %>% dplyr::select(
      dplyr::any_of(c(
        "Research_number", "pat_id",
        "visit", "Visit", "visit_occasion", "occasion",
        "date", "Date",
        "T_Number_saccades",
        "L_Number_saccades",
        "L15_Number_saccades",
        "L15_VDI_AUC",
        "L15_SD_VDI_AUC",
        "L15_VDI_PvAm",
        "L15_SD_VDI_PvAm",
        "L15_VDI_peakvelocity",
        "L15_SD_VDI_peakvelocity",
        "R_Number_saccades",
        "R15_Number_saccades",
        "R15_VDI_AUC",
        "R15_SD_VDI_AUC",
        "R15_VDI_PvAm",
        "R15_SD_VDI_PvAm",
        "R15_VDI_peakvelocity",
        "R15_SD_VDI_peakvelocity",
        "T8_Latency",
        "T15_Latency",
        ## For variables below subjects with INO are excluded in some previous publications.
        "T15_Peakvelocity", # Peak velocity 15 PS
        "T8_Peakvelocity", # Peak velocity 8 PS
        "T15_Peakvel_Ampl", # pV/Am 15 PS
        "T8_Peakvel_Ampl", # Pv/Am 8 PS
        "T15_Gain", # Gain 15 PS
        "T8_Gain" # Gain 8 PS
      ))
    )
  }
  demons_data_prosac <- demons_data_prosac %>%
    demons_process_INO() %>%
    dplyr::mutate(
      QC_T_numsac =  T_Number_saccades >= 60/2, # quality control: at least 50% of total 60 saccades valid.
      QC_L15_numsac = L15_Number_saccades >= 15/2, # quality control: at least 50% of 15 degree leftward saccades valid.
      QC_R15_numsac = R15_Number_saccades >= 15/2 # quality control: at least 50% of 15 degree rightward saccades valid.
    )

  if(add_prefix == TRUE) {
    demons_data_prosac <- demons_data_prosac %>%
      dplyr::rename_with(~paste0("prosac_", .),
                         .cols = -dplyr::any_of(c(
                           "Research_number", "pat_id",
                           "visit", "Visit", "visit_occasion", "occasion",
                           "date", "Date"
                         ))
      )
  }

  return(demons_data_prosac)
}
