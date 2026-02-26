
<!-- README.md is generated from README.Rmd. Please edit that file -->

# samspack

<!-- badges: start -->

[![R-CMD-check](https://github.com/snhof/samspack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/snhof/samspack/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Samspack is a personal R package containing functions frequently used
in data analysis and visualization, with a focus on multiple sclerosis
(MS) research and eye-tracking data. It provides:

- **Multiple regression helpers** — run many regression models in one
  call and get a tidy summary table.
- **DEMoNS eye-tracking processing** — process raw output from the
  [DEMoNS
  protocol](https://www.protocols.io/view/demons-protocol-for-measurement-and-analysis-of-ey-x54v98eyml3e/v3)
  tasks (fixation, pro-saccades, anti-saccades, express saccades,
  double-step saccades, repeated saccades).
- **INO detection** — calculate versional dysconjugacy indices and
  determine presence of internuclear ophthalmoplegia (INO).
- **VFQ-25 scoring** — score the Visual Function Questionnaire-25
  (Dutch version).

## Installation

You can install the development version of samspack from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("snhof/samspack")
```

## Multiple regression helpers

The regression helper functions let you specify vectors of outcome
variables, predictors, and covariates. The functions then run every
combination and return a single tidy results table.

Four model types are supported:

| Function | Model type |
|---|---|
| `lm_mult()` | Linear regression (`lm`) |
| `lmer_mult()` | Linear mixed-effects model (`lmer`) |
| `glm_log_mult()` | Logistic regression (`glm`, binomial) |
| `geeglm_log_mult()` | Logistic GEE regression (`geeglm`) |

### Linear regression example

``` r
library(samspack)

lm_mult(
  data       = MS_trial_data,
  outcomes   = c("EDSS", "SDMT"),
  predictors = "age",
  covariates = c("", "+ gender")
)
```

### Linear mixed-effects model example

``` r
lmer_mult(
  data       = MS_trial_data,
  outcomes   = c("EDSS", "SDMT"),
  predictors = c("intervention", "intervention * time"),
  covariates = c("", "+ age + gender"),
  randoms    = c("+ (1|pat_id)")
)
```

### Logistic regression example

``` r
glm_log_mult(
  data       = MS_trial_data,
  outcomes   = "INO",
  predictors = c("intervention", "age"),
  covariates = c("", "+ gender")
)
```

### Logistic GEE regression example

``` r
geeglm_log_mult(
  data       = MS_trial_data,
  outcomes   = "INO",
  predictors = c("intervention", "intervention * time"),
  covariates = c("", "+ gender + age"),
  id         = "pat_id"
)
```

### Constructing formulas manually

`construct_formulas()` builds a data frame of all formula combinations,
which can be passed directly to any of the regression helpers or used
with base R model functions.

``` r
construct_formulas(
  outcomes   = c("EDSS", "SDMT", "log(SDMT)"),
  predictors = c("intervention", "intervention * time"),
  covariates = c("", "+ age", "+ age + gender"),
  randoms    = c("", "+ (1|pat_id)")
)
```

## DEMoNS eye-tracking data processing

The `demons_process_*` functions add derived parameters and dichotomous
outcome variables (based on published cut-offs) to raw DEMoNS output.
Each function corresponds to one task in the protocol.

| Function | Task |
|---|---|
| `demons_process_fix()` | Fixation |
| `demons_process_prosac()` | Pro-saccades |
| `demons_process_antisac()` | Anti-saccades |
| `demons_process_expsac()` | Express saccades |
| `demons_process_dblstep()` | Double-step saccades |
| `demons_process_repsac()` | Repeated saccades |

Example datasets (`DEMoNS_data`, `DEMoNS_data_fix`,
`DEMoNS_data_prosac`, etc.) are included for testing and demonstration.

``` r
demons_process_fix(DEMoNS_data_fix, keep_essential = TRUE, add_prefix = TRUE)
```

## INO detection

`calcINO()` (a wrapper for `demons_process_INO()`) processes
pro-saccades data and uses versional dysconjugacy index (VDI) cut-off
values to classify eyes as INO-positive or INO-negative.

``` r
calcINO(DEMoNS_data_prosac)
```

## VFQ-25 scoring

`VFQ_scoring()` converts raw Dutch-language VFQ-25 questionnaire
responses to numerical subscale scores and a composite total score.

``` r
# df is a data frame with columns named VFQ1 through VFQ25
VFQ_scoring(df)
```

## Checking distributions and outliers

### check_histograms()

`check_histograms()` is one of the most frequently used functions in
the package. It iterates over every numeric variable in a data frame
and returns a list of histograms, each showing the mean, median, ±2 SD
lines, a boxplot, a density curve, and outlier labels.

``` r
check_histograms(data = MS_trial_data, id = pat_id, facet_cols = gender)
```

Use `wrap_plots_split()` to display the list of plots across multiple
pages (4 plots per page by default):

``` r
check_histograms(data = MS_trial_data, id = pat_id) |> wrap_plots_split()
```

`histogram_sp()` creates a single histogram with the same annotations
and is the underlying function called by `check_histograms()`.

### check_scatterplots()

`check_scatterplots()` creates scatterplots for all combinations of
specified outcome and predictor variables, with outliers automatically
labeled.

``` r
check_scatterplots(
  data       = MS_trial_data,
  outcomes   = c("EDSS", "SDMT"),
  predictors = c("age", "time"),
  id         = "pat_id"
)
```

## Checking model assumptions

### lmer_assump_tests()

`lmer_assump_tests()` takes a fitted `lmer` model and returns a
combined plot for checking the three key assumptions: normality of
residuals (histogram), linearity (residuals vs fitted), and normality
of random effects (QQ-plot).

``` r
model <- lme4::lmer(SDMT ~ intervention + (1|pat_id), data = MS_trial_data, REML = FALSE)
lmer_assump_tests(model)
```

`lmer_mult_assump_tests()` is a convenience wrapper that mirrors
`lmer_mult()` and returns an assumption-check plot for every model in
the batch.

### lm_assump_tests()

`lm_assump_tests()` takes a fitted `lm` model and returns a combined
plot for normality of residuals and linearity/homoscedasticity.

``` r
lm(SDMT ~ age, data = MS_trial_data) |> lm_assump_tests()
```

`lm_mult_assump_tests()` is a convenience wrapper that mirrors
`lm_mult()` and returns an assumption-check plot for every model in the
batch.

## Function reference

Below is a complete list of all exported functions grouped by category.

### Multiple regression — run models

| Function | Description |
|---|---|
| `construct_formulas()` | Build a data frame of all formula combinations from vectors of outcomes, predictors, covariates and random effects |
| `lm_mult()` | Run multiple linear regression models (`lm`) and return a tidy results table |
| `lmer_mult()` | Run multiple linear mixed-effects models (`lmer`) and return a tidy results table |
| `glm_log_mult()` | Run multiple logistic regression models (`glm`, binomial) and return a tidy results table |
| `geeglm_log_mult()` | Run multiple logistic GEE regression models (`geeglm`) and return a tidy results table |

### Multiple regression — assumption checks

| Function | Description |
|---|---|
| `lm_assump_tests()` | Assumption-check plots for a single `lm` model |
| `lm_mult_assump_tests()` | Assumption-check plots for a batch of `lm` models |
| `lmer_assump_tests()` | Assumption-check plots for a single `lmer` model |
| `lmer_mult_assump_tests()` | Assumption-check plots for a batch of `lmer` models |

### Exploratory data analysis

| Function | Description |
|---|---|
| `check_histograms()` | Histograms with mean, median, ±2 SD, boxplot, density and outlier labels for all numeric variables in a data frame |
| `histogram_sp()` | Single histogram with the same annotations as `check_histograms()` |
| `check_scatterplots()` | Scatterplots for all outcome × predictor combinations with outlier labels |
| `is_outlier()` | Detect outliers using the Tukey method (Q1/Q3 ± 1.5 × IQR) |
| `wrap_plots_split()` | Split a list of ggplot objects into pages of *n* plots |

### DEMoNS eye-tracking

| Function | Description |
|---|---|
| `demons_process_fix()` | Process fixation task data |
| `demons_process_prosac()` | Process pro-saccades task data |
| `demons_process_antisac()` | Process anti-saccades task data |
| `demons_process_expsac()` | Process express saccades task data |
| `demons_process_dblstep()` | Process double-step saccades task data |
| `demons_process_repsac()` | Process repeated saccades task data |
| `calcINO()` | Calculate VDI metrics and classify INO from pro-saccades data |

### Questionnaires and clinical scores

| Function | Description |
|---|---|
| `VFQ_scoring()` | Score the Dutch VFQ-25 questionnaire into subscale and total scores |

### Saving plots

| Function | Description |
|---|---|
| `ggsavepp()` | Save a ggplot with dimensions optimised for a PowerPoint slide |
| `ggsavepphalf()` | Save a ggplot at half PowerPoint slide width |
| `ggsaveppfull()` | Save a ggplot at full PowerPoint slide dimensions |
| `ggsaveppquart()` | Save a ggplot at quarter PowerPoint slide dimensions |
| `ggsavemagnify()` | Save a ggplot with a custom magnification factor |
