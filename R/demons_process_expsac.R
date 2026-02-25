#' Process Express saccades task data from the DEMoNS eye-tracking protocol
#'
#' Calculates quality control metrics for express saccades task data obtained using the DEMoNS eye-tracking protocol.
#' Allows selection of essential variables and addition of a prefix to variable names.
#' See [Nij Bijvank et al. 2018](https://doi.org/10.1371/journal.pone.0200695) for protocol details.
#' DEMoNS protocol task and MATLAB analysis script available at [protocols.io](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3).
#'
#' @inheritParams demons_process_fix
#' @param demons_data_expsac Dataframe with raw eye-tracker express saccades data after processing according to DEMoNS protocol.
#'
#' @returns Dataframe with additional columns indicating quality control metrics.
#' @export
#'
#' @examples
#' demons_process_expsac(DEMoNS_data_expsac, keep_essential = TRUE, add_prefix = TRUE)
#'
demons_process_expsac <- function(demons_data_expsac, keep_essential = FALSE, add_prefix = FALSE) {
  if(keep_essential == TRUE) {
    demons_data_expsac <- demons_data_expsac %>% dplyr::select(
      dplyr::any_of(c(
        "Research_number", "pat_id",
        "visit", "Visit", "visit_occasion", "occasion",
        "date", "Date",
        "T_Number_saccades",
        "T_Latency",
        "T_Peakvelocity",
        "T_Gain"
      ))
    )
  }

  demons_data_expsac <- demons_data_expsac %>%
    dplyr::mutate(
      QC_numsac =  T_Number_saccades >= 15, # quality control: at least 50% of total 30 saccades valid.
    )

  if(add_prefix == TRUE) {
    demons_data_expsac <- demons_data_expsac %>%
      dplyr::rename_with(~paste0("expsac_", .),
                         .cols = -dplyr::any_of(c(
                           "Research_number", "pat_id",
                           "visit", "Visit", "visit_occasion", "occasion",
                           "date", "Date"
                         ))
      )
  }

  return(demons_data_expsac)
}
