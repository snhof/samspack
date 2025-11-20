## code to prepare `cutoff_VDI` dataset goes here

cutoff_VDI <- tibble::tibble (
  VDI_type = c("AUC", "PvAm"),
  cutoff = c(1.174, 1.180)
)

cutoff_VDI_AUC = 1.174
cutoff_VDI_PvAm = 1.180

usethis::use_data(cutoff_VDI, overwrite = TRUE)
usethis::use_data(cutoff_VDI_AUC, overwrite = TRUE)
usethis::use_data(cutoff_VDI_PvAm, overwrite = TRUE)
