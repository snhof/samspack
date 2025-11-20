#' VFQ-25 questionnaire data naming and scoring
#'
#' @param df dataframe with raw VFQ-25 data
#'
#' @returns dataframe with added columns
#' @export
#' @importFrom magrittr %>%
VFQ_scoring <- function(df) {
  df %>% dplyr::mutate(
    dplyr::across(dplyr::matches("VFQ1(?!\\d)", perl = TRUE), ~factor(.x, c("Uitstekend", "Zeer goed", "Goed", "Redelijk", "Slecht"))),
    dplyr::across(dplyr::matches("VFQ2(?!\\d)", perl = TRUE), ~factor(.x, c("Uitstekend", "Goed", "Redelijk", "Slecht", "Zeer slecht", "Volledig blind"))),
    dplyr::across(dplyr::contains("VFQ3"), ~factor(.x, c("Nooit", "Zelden", "Soms", "Vaak", "Altijd"))),
    dplyr::across(dplyr::contains("VFQ4"), ~factor(.x, c("Geen", "Licht", "Matig", "Ernstig", "Heel ernstig"))),
    dplyr::across(dplyr::contains(c("VFQ5","VFQ6","VFQ7","VFQ8","VFQ9","VFQ10","VFQ11","VFQ12","VFQ13","VFQ14", "VFQ16")),
           ~factor(.x, c("Geen enkele moeite", "Een beetje moeite", "Matige moeite", "Enorme moeite", "Hiermee gestopt vanwege het slechte gezichtsvermogen", "Hiermee gestopt om andere redenen, of op u niet van toepassing"))),
    dplyr::across(dplyr::matches("VFQ15(?![abc])", perl = TRUE), ~factor(.x, c("Ja", "Nee"))),
    dplyr::across(dplyr::contains("VFQ15a"), ~factor(.x, c("Ik heb nooit auto gereden", "Ik heb het autorijden opgegeven"))),
    dplyr::across(dplyr::contains("VFQ15b"), ~factor(.x, c("Voornamelijk mijn gezichtsvermogen", "Voornamelijk om andere redenen", "Zowel mijn gezichtsvermogen als om andere redenen"))),
    dplyr::across(dplyr::contains("VFQ15c"), ~factor(.x, c("Geen enkele moeite", "Een beetje moeite", "Matige moeite", "Enorme moeite"))),
    dplyr::across(dplyr::contains(c("VFQ17", "VFQ18", "VFQ19")),
           ~factor(.x, c("Altijd", "Meestal", "Soms", "Zelden", "Nooit"))),
    dplyr::across(dplyr::contains(c("VFQ20", "VFQ21", "VFQ22", "VFQ23", "VFQ24", "VFQ25")),
           ~factor(.x, c("Helemaal juist", "Over het algemeen juist", "Weet het niet zeker", "Over het algemeen onjuist", "Helemaal onjuist"))),
    dplyr::across(dplyr::matches("VFQ\\d+"), ~as.numeric(.x))
  ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::matches("VFQ[134](?!\\d)", perl = TRUE), ~dplyr::case_match(.x, 1 ~ 100, 2 ~ 75, 3 ~ 50, 4 ~ 25, 5 ~ 0)),
      dplyr::across(dplyr::contains("VFQ15c"), ~dplyr::case_match(.x, 1 ~ 100, 2 ~ 75, 3 ~ 50, 4 ~ 25, 5 ~ 0)),
      dplyr::across(dplyr::contains("VFQ15c"), ~dplyr::case_when(
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ15b"))) == 1 ~ 0,
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ15b"))) == 2 | dplyr::pull(dplyr::pick(dplyr::contains("VFQ15b"))) == 3 ~ NA,
        TRUE ~ dplyr::pull(dplyr::pick(dplyr::contains("VFQ15c")))
      )),
      dplyr::across(dplyr::matches("VFQ2(?!\\d)", perl = TRUE), ~dplyr::case_match(.x, 1 ~ 100, 2 ~ 80, 3 ~ 60, 4 ~ 40, 5 ~ 20, 6 ~ 0)),
      dplyr::across(dplyr::contains(c("VFQ5", "VFQ6", "VFQ7", "VFQ8", "VFQ9", "VFQ10", "VFQ11", "VFQ12", "VFQ13", "VFQ14", "VFQ16")),
             ~dplyr::case_match(.x, 1 ~ 100, 2 ~ 75, 3 ~ 50, 4 ~ 25, 5 ~ 0, 6 ~ NA)),
      dplyr::across(dplyr::contains(c("VFQ17", "VFQ18", "VFQ19", "VFQ20", "VFQ21", "VFQ22", "VFQ23", "VFQ24", "VFQ25")),
             ~dplyr::case_match(.x, 1 ~ 0, 2 ~ 25, 3 ~ 50, 4 ~ 75, 5 ~ 100))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      VFQ_general_health = dplyr::pull(dplyr::pick(dplyr::matches("VFQ1(?!\\d)", perl = TRUE))),
      VFQ_general_vision = dplyr::pull(dplyr::pick(dplyr::matches("VFQ2(?!\\d)", perl = TRUE))),
      VFQ_ocular_pain = mean(c(dplyr::pull(dplyr::pick(dplyr::contains("VFQ4"))), dplyr::pull(dplyr::pick(dplyr::contains("VFQ19")))), na.rm = TRUE),
      VFQ_near_activities = mean(c(
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ5"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ6"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ7")))
      ), na.rm = TRUE),
      VFQ_distance_activities = mean(c(
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ8"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ9"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ14")))
      ), na.rm = TRUE),
      VFQ_social_functioning = mean(c(
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ11"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ13")))
      ), na.rm = TRUE),
      VFQ_mental_health = mean(c(
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ3"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ21"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ22"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ25")))
      ), na.rm = TRUE),
      VFQ_role_difficulties = mean(c(
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ17"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ18")))
      ), na.rm = TRUE),
      VFQ_dependency = mean(c(
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ20"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ23"))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ24")))
      ), na.rm = TRUE),
      VFQ_driving = mean(c(
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ15c"))),
        dplyr::pull(dplyr::pick(dplyr::matches("VFQ16(?!a)", perl = TRUE))),
        dplyr::pull(dplyr::pick(dplyr::contains("VFQ16a")))
      ), na.rm = TRUE),
      VFQ_color_vision = dplyr::pull(dplyr::pick(dplyr::contains("VFQ12"))),
      VFQ_peripheral_vision = dplyr::pull(dplyr::pick(dplyr::contains("VFQ10"))),
      VFQ_total = mean(c(
        VFQ_general_vision,
        VFQ_ocular_pain,
        VFQ_near_activities,
        VFQ_distance_activities,
        VFQ_social_functioning,
        VFQ_mental_health,
        VFQ_role_difficulties,
        VFQ_dependency,
        VFQ_driving,
        VFQ_color_vision,
        VFQ_peripheral_vision
      ), na.rm = TRUE)
    ) %>% dplyr::ungroup()
}

utils::globalVariables(c(
  "VFQ_general_vision", "VFQ_ocular_pain", "VFQ_near_activities", "VFQ_distance_activities", "VFQ_social_functioning", "VFQ_mental_health",
  "VFQ_role_difficulties", "VFQ_dependency", "VFQ_driving", "VFQ_color_vision", "VFQ_peripheral_vision"
  ))


