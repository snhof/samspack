# Prepare reference measurement for export with package
# Date: 15-12_2025
# Author: S.N Hof

# load packages ----
library(tidyverse)
library(readxl)

# Import eye tracking data -----
# 1. Fixation task
DEMoNS_data_fix <- read_xlsx("M:/MS groep/MS onderzoek/Neuro-oftalmologie/Eyetracking/Eyetracking data/Tests and reference measurements/output files/Fixation_output_09-Jun-2023.xlsx")
# 2. Pro-saccades task
DEMoNS_data_prosac <- read_xlsx("M:/MS groep/MS onderzoek/Neuro-oftalmologie/Eyetracking/Eyetracking data/Tests and reference measurements/output files/Pro-saccades_output_09-Jun-2023.xlsx")
# 3. Anti-saccades task
DEMoNS_data_antisac <- read_xlsx("M:/MS groep/MS onderzoek/Neuro-oftalmologie/Eyetracking/Eyetracking data/Tests and reference measurements/output files/Anti-saccades_output_09-Jun-2023.xlsx")
# 4. Express saccades task
DEMoNS_data_expsac <- read_xlsx("M:/MS groep/MS onderzoek/Neuro-oftalmologie/Eyetracking/Eyetracking data/Tests and reference measurements/output files/Express_saccades_output_09-Jun-2023.xlsx")
# 5. Double-step saccades task
DEMoNS_data_dblstep <- read_xlsx("M:/MS groep/MS onderzoek/Neuro-oftalmologie/Eyetracking/Eyetracking data/Tests and reference measurements/output files/Double-step_saccades_output_09-Jun-2023.xlsx")
# 6. Repeated pro-saccades task
DEMoNS_data_repsac <- read_xlsx("M:/MS groep/MS onderzoek/Neuro-oftalmologie/Eyetracking/Eyetracking data/Tests and reference measurements/output files/Repeated_saccades_output_09-Jun-2023.xlsx")

# combine all eyetracking data into one dataframe ----
DEMoNS_data <- DEMoNS_data_fix %>% rename_with(~paste0("fix_", .), .cols = -c(Research_number, Date)) %>%
  full_join(DEMoNS_data_prosac %>% rename_with(~paste0("prosac_", .), .cols = -c(Research_number, Date)),
            by = c("Research_number", "Date")) %>%
  full_join(DEMoNS_data_antisac %>% rename_with(~paste0("antisac_", .), .cols = -c(Research_number, Date)),
            by = c("Research_number", "Date")) %>%
  full_join(DEMoNS_data_expsac %>% rename_with(~paste0("expsac_", .), .cols = -c(Research_number, Date)),
            by = c("Research_number", "Date")) %>%
  full_join(DEMoNS_data_dblstep %>% rename_with(~paste0("dblstep_", .), .cols = -c(Research_number, Date)),
            by = c("Research_number", "Date")) %>%
  full_join(DEMoNS_data_repsac %>% rename_with(~paste0("repsac_", .), .cols = -c(Research_number, Date)),
            by = c("Research_number", "Date")) %>%
  relocate(Research_number, Date)

# save as internal dataset ----
usethis::use_data(DEMoNS_data_fix, overwrite = TRUE)
usethis::use_data(DEMoNS_data_prosac, overwrite = TRUE)
usethis::use_data(DEMoNS_data_antisac, overwrite = TRUE)
usethis::use_data(DEMoNS_data_expsac, overwrite = TRUE)
usethis::use_data(DEMoNS_data_dblstep, overwrite = TRUE)
usethis::use_data(DEMoNS_data_repsac, overwrite = TRUE)

usethis::use_data(DEMoNS_data, overwrite = TRUE)
