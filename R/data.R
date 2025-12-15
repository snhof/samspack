#' Randomly generated MS trial data
#'
#' Randomly generated data from a fictitious MS trial for testing purposes. Includes 100 patients with 5 timepoints each.
#'
#' @format ## 'MS_trial_data'
#' A data frame with 500 rows and 8 columns:
#' \describe{
#'   \item{pat_id}{Patient ID ranging from pat001 to pat100}
#'   \item{age}{Baseline age ranging from 18 to 70 years with a mean of 40 years}
#'   \item{intervention}{Patient in intervention or control group}
#'   \item{gender}{Male or female}
#'   \item{time}{Timepoint ranging from 1 to 5}
#'   \item{INO}{Does patient have INO? INO is unlikely to resolve once patients have it}
#'   \item{SDMT}{Score on the Symbol Digits Modalities test. Ranging from 0 to 60. Score is more likely to decline over time.}
#'   \item{EDSS}{Expanded Disability Status Scale score. Ranging from 0 to 10 in increments of 0.5. Follows a bimodal distribution with peaks around 3 and 6. EDSS is more likely to increase over time.}
#' }
#' @source Generated using internal script.
"MS_trial_data"

#' VDI cutoff values for determining INO
#'
#' Dataframe with cutoff values for versional dysconjugacy indices (VDI) for area under the curve (VDI-AUC) and peak velocity corrected for amplitude (VDI-pV/Am) to determine presence of internuclear ophthalmoplegia (INO).
#' VDI data should be obtained using prosaccades task from the DEMoNS protocol (Nij Bijvank et al. 2018, https://doi.org/10.1371/journal.pone.0200695).
#' Cutoff values were established in Nij Bijvank et al. 2019 (https://doi.org/10.1212/WNL.0000000000007499).
#'
#' @name cutoff_VDI
#'
#' @source Cutoff values were established in Nij Bijvank et al. 2019 (https://doi.org/10.1212/WNL.0000000000007499).
"cutoff_VDI"

#' @rdname cutoff_VDI
"cutoff_VDI_AUC"

#' @rdname cutoff_VDI
"cutoff_VDI_PvAm"

#' Example measurement DEMoNS eyetracking protocol
#'
#' Example data obtained from two measurements using the DEMoNS eyetracking protocol [(Nij Bijvank et al. 2018)](https://doi.org/10.1371/journal.pone.0200695). Task and analysis MATLAB scripts are available at [protocols.io](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3).
#' Includes data for fixation, pro-saccades, anti-saccades, express saccades, double-step saccades and repeated saccades task.
#' For data specific to single tasks, see [DEMoNS_data_fix], [DEMoNS_data_prosac], [DEMoNS_data_antisac], [DEMoNS_data_expsac], [DEMoNS_data_dblstep] and [DEMoNS_data_repsac].
#'
#' @name DEMoNS_data
#' @source Generated using tasks and scripts available at [protocols.io](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3).
"DEMoNS_data"

#' @rdname DEMoNS_data
"DEMoNS_data_fix"

#' @rdname DEMoNS_data
"DEMoNS_data_prosac"

#' @rdname DEMoNS_data
"DEMoNS_data_antisac"

#' @rdname DEMoNS_data
"DEMoNS_data_expsac"

#' @rdname DEMoNS_data
"DEMoNS_data_dblstep"

#' @rdname DEMoNS_data
"DEMoNS_data_repsac"

