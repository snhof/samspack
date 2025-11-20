#'lmer_mult: Run multiple linear mixed effects models and output table
#'
#'Run multiple linear mixed effect models with the lme4 and lmerTest packages.
#'
#'The output contains some additional variables that are not in the standard mixed model output namely:
#'
#'isSingular = is TRUE if the model is ‘singular’. There is debate whether you can then use the model or not. See ?isSingular
#'
#'Loglik = the log likelihood of the model with which to compare models
#'
#'Deviance = -2 * loglik
#'
#'term_ranova = output of the ranova() function that performs the likelihood ratio test(LRT). This variable indicates for which term in the regression formula the LRT was performed.
#'
#'npar_ranova = output of the ranova() function performing the likelihood ratio test(LRT). This variable indicates the number of parameters in the model.
#'
#'p_ranova = output of the ranova() function performing the likelihood ratio test(LRT). This variable indicates the p value of the LRT.
#'
#'"std_beta" to obtain standardized regression coefficients may be implemented in the future, but is currently not available.
#'
#' @param data data frame with outcomes, predictors, covariates and variables for random effects in long format with a row for every observation of the outcome variables. See [MS_trial_data] for an example dataset.
#' @param outcomes a vector with your outcome variables (dependent variables), where the names of your variables are in quotes. For example: c(‘outcome1’, ‘outcome2’, ‘outcome3’). Here it is also possible to log transform your outcomes, e.g. c(‘log(outcome1)’, ‘log(outcome2)’)
#' @param predictors A vector containing your predictors (independent variables), where the names of your variables are in quotes. For example: c(‘predictor1’, ‘predictor2’, ‘predictor3’). It is possible to directly recode binary or categorical variables and get an output for both encodings, e.g. c(‘predictor1’, ‘fct_rev(predictor1)’). It is also possible to specify interaction terms, e.g. c(‘predictor1 * predictor2’, ‘predictor2 * predictor 3’).
#' @param covariates a vector containing your covariates (variables you want to correct for), where the entire enumeration of covariates per model are in quotes and different covariates in the same model are separated by a plus. Also, covariates should be preceded by a plus. For example: c(‘+ covariate 1 + covariate 2’, ‘+ covariate2 + covariate3’). If you want to run a model with and without covariates then specify it like this: c(‘’, ‘+ covariate 1 + covariate 2’)
#' @param randoms a vector containing your random effects, where the entire enumeration of random effects per model are in quotes and different random effects in the same model are separated by a plus. Also, random effects must be preceded by a plus. Use notation from lme4 package for random effects. For example: c(‘+ (1|var1)’, ‘+ (var2|var1)’, ‘+ (1|var1/var3)’, ‘+ (var2|var1) + (1|var1:var3)’, ‘+ (var2|var1/var3)’). If you want to run a model with and without random effects, specify it like this: c(‘’, ‘+ (1|var1)’)
#' @param formula Here you can optionally specify your variables as regression formulas instead of single variables. You can combine using this with using the other inputs, all models are then run. for example: c(‘outcome1 ~ predictor1 + covariate1 + (1|var1)’, ‘outcome2 ~ predictor2 + covariate2 + (1|var2)’)
#' @param control This gives you access to the ‘control’ input of the lmer() function. This allows you to bypass certain errors. For example, in case of error about number of observations, you can try control = lmerControl(check.nobs.vs.nRE = ‘ignore’).
#' @param exp_log Do you want to exponentiate the results for log transformed outcomes, set this to TRUE.
#' @param show_progress Do you want to see the progress of the function, set this to TRUE.
#'
#' @returns Large table with results from all regression formulas
#' @export
#'
#' @examples
#'
#' lmer_mult(
#'  data = MS_trial_data,
#'  outcomes = c("EDSS", "SDMT"),
#'  predictors = c("intervention", "intervention * time"),
#'  covariates = c("", "+ age + gender"),
#'  randoms = c("+ (1|pat_id)")
#'  )
#'
lmer_mult <- function(data, outcomes = NULL, predictors=NULL, covariates="", randoms = NULL, formula=NULL, control = lme4::lmerControl(), exp_log = FALSE, show_progress = FALSE) {

  if(!is.null(outcomes) & !is.null(predictors)){
    #   #Remove non continuous variables from outcomes
    #   outcomes = intersect(outcomes,
    #                        data %>% select(-where(is.logical)) %>% names()
    #   ) Does not work with transformation in formula (e.g. log(var1))

    #create data frame with regression formulas
    dplyr::bind_rows(
      tidyr::expand_grid(
        outcome = outcomes,
        predictor = predictors,
        covariate = covariates,
        random = randoms
      ) %>%
        dplyr::mutate(formula = paste0(paste(outcome, predictor, sep = "~"), covariate, random)),

      # add regressions defined by formula
      dplyr::tibble(formula = formula)
    ) -> df

  }  else {
    # Only include regression defined by formula
    dplyr::tibble(formula = formula) -> df
  }

  df %>%
    # run a lineair model and iterate through each formula
    dplyr::mutate(
      model = purrr::map(formula,
                         .f = purrr::safely(
                           ~lmerTest::lmer(formula = as.formula(.x), data = data, REML = FALSE, control = control),
                           otherwise = NA,
                           quiet = FALSE
                         ),
                         .progress = ifelse(show_progress == TRUE, "Running regressions", FALSE)
      )) %>% tidyr::unnest_wider(col=model, strict = TRUE, names_sep = "_") %>%

    #calculating additional handy model parameters
    dplyr::mutate(
      model_error = purrr::map(model_error, purrr::possibly(.f = ~conditionMessage(.x))),
      isSingular = purrr::map(model_result, .f = purrr::possibly(~lme4::isSingular(.x))),
      logLik = purrr::map(model_result, .f = purrr::possibly(~stats::logLik(.x) %>% as.numeric() %>%round(digits = 1))),
      deviance = purrr::map(model_result, .f = purrr::possibly(~-2*stats::logLik(.x) %>% as.numeric() %>% round(digits = 1))),
      ranova = purrr::map(model_result, .f = purrr::possibly(~lmerTest::ranova(.x)), .progress = ifelse(show_progress, "Doing LRT test", FALSE)),
      term_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% attr("formulae") %>% names() %>% dplyr::nth(1))),
      npar_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% dplyr::pull(npar) %>% dplyr::nth(2))),
      p_ranova = purrr::map(ranova, .f = purrr::possibly(~.x %>% dplyr::pull(`Pr(>Chisq)`) %>% dplyr::nth(2))),
      model_result = purrr::map(model_result, .f = purrr::possibly(~broom.mixed::tidy(.x, conf.int = TRUE)), .progress = ifelse(show_progress, "Making output pretty", FALSE))
    ) %>% dplyr::select(-ranova) %>%

    # unnest each model so all models form a single table
    tidyr::unnest(col = c(model_result, model_error, isSingular, logLik, deviance, term_ranova, npar_ranova, p_ranova)) %>%

    # round all numeric columns
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~round(.x, digits = 5)),
      model = "Linear mixed model",
      dplyr::across(c(estimate, conf.low, conf.high), ~dplyr::if_else(stringr::str_detect(outcome, "log\\(") & exp_log == TRUE, exp(.x), .x))
    ) %>%
    dplyr::relocate(model)
}

utils::globalVariables(c(
  "deviance", "isSingular", "logLik", "npar_ranova", "p_ranova", "random", "ranova", "term_ranova"
))
