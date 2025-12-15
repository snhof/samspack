#' Process DEMoNS protocol eye-tracker data to calculate INO parameters
#'
#' @param demons_data_prosac Dataframe with raw eye-tracker pro-saccades data after processing according to DEMoNS protocol.
#'
#' @returns Dataframe with additional columns indicating presence of INO based on AUC and PvAm metrics, as well as quality control metrics and VDI calculations.
#' @export
#' @importFrom magrittr %>%
demons_process_INO <- function(demons_data_prosac) {
  #INO calculation
  demons_data_prosac %>% dplyr::mutate(
    L_INO_AUC = dplyr::case_when(
      is.na(L15_VDI_AUC) ~ NA,
      L15_VDI_AUC > 1.174 ~ TRUE,
      TRUE ~ FALSE
    ),
    R_INO_AUC = dplyr::case_when(
      is.na(R15_VDI_AUC) ~ NA,
      R15_VDI_AUC > 1.174 ~ TRUE,
      TRUE ~ FALSE
    ),
    L_INO_PvAm = dplyr::case_when(
      is.na(L15_VDI_PvAm) ~ NA,
      L15_VDI_PvAm > 1.180 ~ TRUE,
      TRUE ~ FALSE
    ),
    R_INO_PvAm = dplyr::case_when(
      is.na(R15_VDI_PvAm) ~ NA,
      R15_VDI_PvAm > 1.180 ~ TRUE,
      TRUE ~ FALSE
    ),
    INO_AUC = dplyr::case_when(
      is.na(L15_VDI_AUC) & is.na(R15_VDI_AUC) ~ NA,
      L15_VDI_AUC > 1.174 | R15_VDI_AUC > 1.174 ~ TRUE,
      TRUE ~ FALSE
    ),
    INO_PvAm = dplyr::case_when(
      is.na(L15_VDI_PvAm) & is.na(R15_VDI_PvAm) ~ NA,
      L15_VDI_PvAm > 1.180 | R15_VDI_PvAm > 1.180 ~ TRUE,
      TRUE ~ FALSE
    ),
    INO_OR = dplyr::case_when(
      is.na(INO_AUC) & is.na(INO_PvAm) ~ NA,
      INO_AUC == TRUE | INO_PvAm == TRUE ~ TRUE,
      TRUE ~ FALSE
    ),
    INO_AND = dplyr::case_when(
      INO_AUC == TRUE & INO_PvAm == TRUE ~ TRUE,
      is.na(INO_AUC) | is.na(INO_PvAm) ~ NA,
      TRUE ~ FALSE
    ),
    INO_leftwards = dplyr::case_when(
      is.na(L15_VDI_AUC) & is.na(L15_VDI_PvAm) ~ NA,
      L15_VDI_AUC > 1.174 | L15_VDI_PvAm > 1.180 ~ TRUE,
      TRUE ~ FALSE
    ),
    INO_rightwards = dplyr::case_when(
      is.na(R15_VDI_AUC) & is.na(R15_VDI_PvAm) ~ NA,
      R15_VDI_AUC > 1.174 | R15_VDI_PvAm > 1.180 ~ TRUE,
      TRUE ~ FALSE
    ),
    INO_bilateral = dplyr::case_when(
      (L15_VDI_AUC > 1.174 | L15_VDI_PvAm > 1.180) & (R15_VDI_AUC > 1.174 | R15_VDI_PvAm > 1.180) ~ TRUE,
      (is.na(L15_VDI_AUC) & is.na(L15_VDI_PvAm)) | (is.na(R15_VDI_AUC) & is.na(R15_VDI_PvAm)) ~ NA,
      TRUE ~ FALSE
    ),
    INO_direction = dplyr::case_when(
      INO_bilateral ~ "Bilateral",
      INO_rightwards ~ "Rightwards",
      INO_leftwards ~ "Leftwards",
      TRUE ~ NA_character_
    ),
    INO_nounibi = dplyr::case_when(
      INO_direction == "Bilateral" ~ "Bilateral",
      INO_direction == "Leftwards" | INO_direction == "Rightwards" ~ "Unilateral",
      INO_OR != TRUE ~ "No INO",
      TRUE ~ NA_character_
    )
  ) %>%
    #quality control
    dplyr::mutate(
      T_Number_saccades_frac = (T_Number_saccades / 60),
      L_Number_saccades_frac = (L_Number_saccades / 30),
      L15_Number_saccades_frac = (L15_Number_saccades / 15),
      R_Number_saccades_frac = (R_Number_saccades / 30),
      R15_Number_saccades_frac = (R15_Number_saccades / 15)
    ) %>%
    dplyr::mutate(
      L15_VDI_AUC_ln = log(L15_VDI_AUC),
      R15_VDI_AUC_ln = log(R15_VDI_AUC),
      L15_VDI_PvAm_ln = log(L15_VDI_PvAm),
      R15_VDI_PvAm_ln = log(R15_VDI_PvAm),
    ) %>%
    #calculate VDI per subject
    dplyr::mutate(
      max_VDI_AUC = pmax(L15_VDI_AUC, R15_VDI_AUC),
      max_VDI_PvAm = pmax(L15_VDI_PvAm, R15_VDI_PvAm),
      mean_VDI_AUC = rowMeans(dplyr::across(c(L15_VDI_AUC, R15_VDI_AUC))),
      mean_VDI_PvAm = rowMeans(dplyr::across(c(L15_VDI_PvAm, R15_VDI_PvAm))),
      mean_VDI_peakvelocity = rowMeans(dplyr::across(c(L15_VDI_peakvelocity, R15_VDI_peakvelocity))),
      INO_VDI_AUC = dplyr::case_when(
        INO_nounibi == "Unilateral" ~ pmax(L15_VDI_AUC, R15_VDI_AUC),
        INO_nounibi == "Bilateral" ~ rowMeans(dplyr::across(c(L15_VDI_AUC, R15_VDI_AUC))),
        INO_nounibi == "No INO" ~ NA
      ),
      INO_VDI_PvAm = dplyr::case_when(
        INO_nounibi == "Unilateral" ~ pmax(L15_VDI_PvAm, R15_VDI_PvAm),
        INO_nounibi == "Bilateral" ~ rowMeans(dplyr::across(c(L15_VDI_PvAm, R15_VDI_PvAm))),
        INO_nounibi == "No INO" ~ NA
      )) %>%
    #calculate VDI per subject (logaritmically transformed)
    dplyr::mutate(
      max_VDI_AUC_ln = pmax(L15_VDI_AUC_ln, R15_VDI_AUC_ln),
      max_VDI_PvAm_ln = pmax(L15_VDI_PvAm_ln, R15_VDI_PvAm_ln),
      mean_VDI_AUC_ln = rowMeans(dplyr::across(c(L15_VDI_AUC_ln, R15_VDI_AUC_ln))),
      mean_VDI_PvAm_ln = rowMeans(dplyr::across(c(L15_VDI_PvAm_ln, R15_VDI_PvAm_ln))),
      INO_VDI_AUC_ln = dplyr::case_when(
        INO_nounibi == "Unilateral" ~ pmax(L15_VDI_AUC_ln, R15_VDI_AUC_ln),
        INO_nounibi == "Bilateral" ~ rowMeans(dplyr::across(c(L15_VDI_AUC_ln, R15_VDI_AUC_ln))),
        TRUE ~ rowMeans(dplyr::across(c(L15_VDI_AUC_ln, R15_VDI_AUC_ln)))
      ),
      INO_VDI_PvAm_ln = dplyr::case_when(
        INO_nounibi == "Unilateral" ~ pmax(L15_VDI_PvAm_ln, R15_VDI_PvAm_ln),
        INO_nounibi == "Bilateral" ~ rowMeans(dplyr::across(c(L15_VDI_PvAm_ln, R15_VDI_PvAm_ln))),
        TRUE ~ rowMeans(dplyr::across(c(L15_VDI_PvAm_ln, R15_VDI_PvAm_ln)))
      ))
}

utils::globalVariables(c("L15_Number_saccades", "L15_VDI_AUC", "L15_VDI_AUC_ln", "L15_VDI_PvAm",
                         "L15_VDI_PvAm_ln", "L15_VDI_peakvelocity", "L_Number_saccades",
                         "R15_Number_saccades", "R15_VDI_AUC", "R15_VDI_AUC_ln", "R15_VDI_PvAm",
                         "R15_VDI_PvAm_ln", "R15_VDI_peakvelocity", "R_Number_saccades",
                         "T_Number_saccades"))
