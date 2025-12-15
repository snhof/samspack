#' Process repeated pro-saccades task data from the DEMoNS eye-tracking protocol
#'
#' Calculates quality control metrics for repeated pro-saccades task data obtained using the DEMoNS eye-tracking protocol.
#' Allows selection of essential variables and addition of a prefix to variable names.
#' See [Nij Bijvank et al. 2018](https://doi.org/10.1371/journal.pone.0200695) for protocol details.
#' DEMoNS protocol task and MATLAB analysis script available at [protocols.io](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3).
#'
#' @inheritParams demons_process_fix
#' @param demons_data_repsac Dataframe with raw eye-tracker repeated pro-saccades data after processing according to DEMoNS protocol.
#'
#' @returns Dataframe with additional columns indicating quality control metrics.
#' @export
#'
#' @examples
#' demons_process_repsac(DEMoNS_data_repsac, keep_essential = TRUE, add_prefix = TRUE)
#'
demons_process_repsac <- function(demons_data_repsac, keep_essential = FALSE, add_prefix = FALSE) {
  if(keep_essential == TRUE) {
    demons_data_repsac <- demons_data_repsac %>% dplyr::select(
      dplyr::any_of(c(
        "Research_number", "pat_id",
        "visit", "Visit", "visit_occasion", "occasion",
        "date", "Date",
        "T_Number_saccades",
        "L_Number_saccades",
        "L_VDI_AUC",
        "L_SD_VDI_AUC",
        "L_VDI_PvAm",
        "L_SD_VDI_PvAm",
        "L_VDI_peakvelocity",
        "L_SD_VDI_peakvelocity",
        "R_Number_saccades",
        "R_VDI_AUC",
        "R_SD_VDI_AUC",
        "R_VDI_PvAm",
        "R_SD_VDI_PvAm",
        "R_VDI_peakvelocity",
        "R_SD_VDI_peakvelocity",
        "T_Latency",
        ## For variables below subjects with INO are excluded in some previous publications.
        "T_Peakvelocity",
        "T_Peakvel_Ampl", # pV/Am
        "T_Gain"
      ))
    )
  }
  demons_data_repsac <- demons_data_repsac %>%
    dplyr::mutate(
      QC_T_numsac =  T_Number_saccades >= 30/2 # quality control: at least 50% of total 60 saccades valid.
    )

  if(add_prefix == TRUE) {
    demons_data_repsac <- demons_data_repsac %>%
      dplyr::rename_with(~paste0("repsac_", .),
                         .cols = -dplyr::any_of(c(
                           "Research_number", "pat_id",
                           "visit", "Visit", "visit_occasion", "occasion",
                           "date", "Date"
                         ))
      )
  }

  return(demons_data_repsac)
}
