#' construct_formulas: Construct regression formulas from outcomes, predictors, additional terms, random effects, and/or custom formulas
#'
#' Construct a data frame of regression formulas based on all combinations of specified outcomes, predictors, additional terms (via `...`), random effects, and/or custom formulas.
#' These formulas can be used in regression functions like [lm()], [lm_mult()], [lme4::lmer()], [lmer_mult()], [glm()], [glm_log_mult()], [geepack::geeglm()], and [geeglm_log_mult()].
#'
#' @param outcomes a vector with outcome variables (dependent variables), where the names of the outcome variables are in quotes, e.g. `c("outcome1", "outcome2", "outcome3")`. It is possible to specify log transformation of the outcome variables, e.g. `c("log(outcome1)", "log(outcome2)")`.
#' @param predictors a vector with predictors (independent variables), where the names of the variables are in quotes, e.g. `c("predictor1", "predictor2", "predictor3")`. It is possible to directly recode binary or categorical variables and get an output for both encodings, e.g. `c("predictor1", "fct_rev(predictor1)")`. It is also possible to specify interaction terms, e.g. `c("predictor1 * predictor2", "predictor2 * predictor 3")`.
#' @param ... optional named vectors of additional formula terms, e.g. `covariates = c("", "+ age")` or `interactions = c("", "* time")`. All arguments in `...` are expanded across all combinations and inserted between `predictors` and `randoms` in the final formulas, in the order provided.
#' @param randoms a vector containing random effects, where the entire enumeration of random effects are in quotes, different random effects in the same model are separated and preceded by a plus and notation from the lme4 package used, e.g. `c("+ (1|var1)", "+ (var2|var1)", "+ (1|var1/var3)", "+ (var2|var1) + (1|var1:var3)", "+ (var2|var1/var3)")`. If you want to run a model with and without random effects, specify it like this: `c("", "+ (1|var1)")`.
#' @param formulas optional vector of formulas specified manually, e.g. `c("outcome1 ~ predictor1 + covariate1 + (1|var1)", "outcome2 ~ predictor2 + covariate2 + (1|var2)")`. You can combine this with using the other inputs for single variables, all models are then run.
#'
#' @returns Data frame with a column 'formula' containing all constructed regression formulas.
#' @export
#'
#' @examples
#'
#' construct_formulas(
#' outcomes = c("EDSS", "SDMT", "log(SDMT)"),
#' predictors = c("intervention", "intervention * time"),
#' interactions = c("", " * time"),
#' covariates = c("", " + age", "+ age + gender", "+ age + forcats::fct_rev(gender)"),
#' randoms = c("", " + (1|pat_id)"),
#' formulas = c("SDMT ~ intervention * age + (1|pat_id)")
#' )
#'
construct_formulas <- function(outcomes = NULL, predictors = NULL, ..., randoms = "", formulas = NULL) {
  extra_terms <- list(...)
  extra_terms <- extra_terms[!vapply(extra_terms, is.null, logical(1))]

  if(!is.null(outcomes) & !is.null(predictors)){
    grid_inputs <- c(
      list(
        outcome = outcomes,
        predictor = predictors
      ),
      extra_terms,
      list(
        random = randoms
      )
    )

    # Expand all supplied vectors (outcomes/predictors/.../randoms) into every combination.
    # `do.call()` is used so `...` terms can be passed as dynamic arguments to `expand_grid()`.
    df_grid <- do.call(tidyr::expand_grid, grid_inputs)
    # Keep only `...` terms between predictor and random effect; this respects the order supplied in `...`.
    # `setdiff()` removes fixed columns so the same code works for any number of extra arguments.
    middle_cols <- setdiff(names(df_grid), c("outcome", "predictor", "random"))
    middle_terms <- if (length(middle_cols) > 0) {
      # Concatenate the per-row values from all extra term columns into one middle formula segment.
      # `do.call(paste0, ...)` handles an arbitrary number of `...` columns without hard-coding names.
      do.call(paste0, df_grid[middle_cols])
    } else {
      ""
    }

    #create data frame with regression formulas
    dplyr::bind_rows(
      df_grid %>%
        dplyr::mutate(formula = paste0(paste(outcome, predictor, sep = " ~ "), middle_terms, random)) %>%
        dplyr::select(dplyr::where(~ !(all(.=="")))),

      # add regressions defined by formula
      dplyr::tibble(formula = formulas)
    )

  }  else {
    # Only include regression defined by formula
    dplyr::tibble(formula = formulas)
  }
}
