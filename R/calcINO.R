# Calculate INO function ----

#' Process raw eye-tracker data to determine presence of INO
#'
#' @param ETdata Dataframe with raw eye-tracker pro-saccades data after processing according to DEMoNS protocol.
#'
#' @returns Dataframe with additional columns indicating presence of INO based on AUC and PvAm metrics, as well as quality control metrics and VDI calculations.
#' @export
#' @importFrom magrittr %>%
calcINO <- function(ETdata) {
  ETdata %>% demons_process_INO()
}

utils::globalVariables(c("L15_Number_saccades", "L15_VDI_AUC", "L15_VDI_AUC_ln", "L15_VDI_PvAm",
                         "L15_VDI_PvAm_ln", "L15_VDI_peakvelocity", "L_Number_saccades",
                         "R15_Number_saccades", "R15_VDI_AUC", "R15_VDI_AUC_ln", "R15_VDI_PvAm",
                         "R15_VDI_PvAm_ln", "R15_VDI_peakvelocity", "R_Number_saccades",
                         "T_Number_saccades"))
