#' Process anti-saccades task data from the DEMoNS eye-tracking protocol
#'
#' Calculates quality control metrics for anti-saccades task data obtained using the DEMoNS eye-tracking protocol.
#' Allows selection of essential variables and addition of a prefix to variable names.
#' See [Nij Bijvank et al. 2018](https://doi.org/10.1371/journal.pone.0200695) for protocol details.
#' DEMoNS protocol task and MATLAB analysis script available at [protocols.io](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3).
#'
#' @inheritParams demons_process_fix
#' @param demons_data_antisac Dataframe with raw eye-tracker anti-saccades data after processing according to DEMoNS protocol.
#'
#' @returns Dataframe with additional columns indicating quality control metrics.
#' @export
#'
#' @examples
#' demons_process_antisac(DEMoNS_data_antisac, keep_essential = TRUE, add_prefix = TRUE)
#'
demons_process_antisac <- function(demons_data_antisac, keep_essential = FALSE, add_prefix = FALSE) {
  if(keep_essential == TRUE) {
    demons_data_antisac <- demons_data_antisac %>% dplyr::select(
      dplyr::any_of(c(
        "Research_number", "pat_id",
        "visit", "Visit", "visit_occasion", "occasion",
        "date", "Date",
        "T_Number_saccades",
        "T_Proportion_errors",
        "T_Latency_correct",
        "T_Latency_incorrect",
        "T_Latency_corrsac_incorrect",
        "T_Peakvelocity_correct",
        "T_Gain_correct",
        "T_XYabserror_FEP"
      ))
    )
  }

  demons_data_antisac <- demons_data_antisac %>%
    dplyr::mutate(
      QC_numsac =  T_Number_saccades >= 40/2, # quality control: at least 50% of total 40 saccades valid.
    )

  if(add_prefix == TRUE) {
    demons_data_antisac <- demons_data_antisac %>%
      dplyr::rename_with(~paste0("antisac_", .),
                         .cols = -dplyr::any_of(c(
                           "Research_number", "pat_id",
                           "visit", "Visit", "visit_occasion", "occasion",
                           "date", "Date"
                         ))
      )
  }

  return(demons_data_antisac)
}
