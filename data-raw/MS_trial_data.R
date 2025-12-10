## code to prepare `MS_trial_data` dataset goes here

library(tidyverse)

set.seed(123)

n <- 100

# Generate patient IDs
pat_ids <- str_c("pat", str_pad(1:n, width = 3, pad = "0"))

# Generate ages
ages <- rnorm(n, mean = 40, sd = 10) %>%
  pmin(70) %>%
  pmax(18) %>%
  round()

# Assign intervention groups
intervention_group <- sample(c("intervention", "control"), size = n, replace = TRUE)

# Assign gender (fixed per patient)
gender <- sample(c("male", "female"), size = n, replace = TRUE)

# Base patient data
base_data <- tibble(
  pat_id = pat_ids,
  age = ages,
  intervention = intervention_group,
  gender = gender
)

# EDSS levels and bimodal distribution
edss_levels <- seq(0, 10, by = 0.5)
edss_probs <- dnorm(edss_levels, mean = 3, sd = 1) + dnorm(edss_levels, mean = 6, sd = 1)
edss_probs <- edss_probs / sum(edss_probs)

# Expand and simulate
MS_trial_data <- expand_grid(base_data, time = 1:5) %>%
  arrange(pat_id, time) %>%
  group_by(pat_id) %>%
  group_modify(~ {
    intervention <- .x$intervention[1]

    # SDMT decline
    sdmt_base <- rnorm(1, mean = 50, sd = 5)
    sdmt_decline <- if (intervention == "intervention") runif(1, 0.5, 1.5) else runif(1, 1.5, 3)
    SDMT <- round(pmax(0, sdmt_base - (.x$time - 1) * sdmt_decline))

    # EDSS progression
    edss_start <- sample(edss_levels, size = 1, prob = edss_probs)
    edss_step <- if (intervention == "intervention") 0.5 else 1
    EDSS <- accumulate(
      .x = .x$time,
      .init = edss_start,
      .f = function(prev, ...) {
        step <- sample(c(0, edss_step), size = 1, prob = c(0.6, 0.4))
        min(10, max(prev, prev + step))
      }
    )[-1]

    # INO with sticky TRUE behavior
    INO <- logical(nrow(.x))
    INO[1] <- sample(c(TRUE, FALSE), size = 1)
    for (i in 2:length(INO)) {
      if (INO[i - 1]) {
        INO[i] <- sample(c(TRUE, FALSE), size = 1, prob = c(0.85, 0.15))  # stickiness
      } else {
        INO[i] <- sample(c(TRUE, FALSE), size = 1, prob = c(0.5, 0.5))    # neutral
      }
    }

    .x %>%
      mutate(
        INO = INO,
        SDMT = SDMT,
        EDSS = EDSS
      )
  }) %>%
  ungroup()

usethis::use_data(MS_trial_data, overwrite = TRUE)
