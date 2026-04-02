# Helper that creates a minimal VFQ data frame with required columns.
# All questions are set to valid ordinal values (level 1 = best score).
make_vfq_data <- function() {
  data.frame(
    VFQ1  = "Uitstekend",
    VFQ2  = "Uitstekend",
    VFQ3  = "Nooit",
    VFQ4  = "Geen",
    VFQ5  = "Geen enkele moeite",
    VFQ6  = "Geen enkele moeite",
    VFQ7  = "Geen enkele moeite",
    VFQ8  = "Geen enkele moeite",
    VFQ9  = "Geen enkele moeite",
    VFQ10 = "Geen enkele moeite",
    VFQ11 = "Geen enkele moeite",
    VFQ12 = "Geen enkele moeite",
    VFQ13 = "Geen enkele moeite",
    VFQ14 = "Geen enkele moeite",
    VFQ15 = "Ja",
    VFQ15a = "Ik heb nooit auto gereden",
    VFQ15b = "Voornamelijk mijn gezichtsvermogen",
    VFQ15c = "Geen enkele moeite",
    VFQ16 = "Geen enkele moeite",
    VFQ16a = "Geen enkele moeite",
    VFQ17 = "Altijd",
    VFQ18 = "Altijd",
    VFQ19 = "Nooit",
    VFQ20 = "Helemaal juist",
    VFQ21 = "Helemaal juist",
    VFQ22 = "Helemaal juist",
    VFQ23 = "Helemaal juist",
    VFQ24 = "Helemaal juist",
    VFQ25 = "Helemaal juist",
    stringsAsFactors = FALSE
  )
}

test_that("VFQ_scoring returns a dataframe", {
  result <- VFQ_scoring(make_vfq_data())
  expect_s3_class(result, "data.frame")
})

test_that("VFQ_scoring adds subscale score columns", {
  result <- VFQ_scoring(make_vfq_data())
  subscales <- c(
    "VFQ_general_health", "VFQ_general_vision", "VFQ_ocular_pain",
    "VFQ_near_activities", "VFQ_distance_activities", "VFQ_social_functioning",
    "VFQ_mental_health", "VFQ_role_difficulties", "VFQ_dependency",
    "VFQ_driving", "VFQ_color_vision", "VFQ_peripheral_vision",
    "VFQ_total"
  )
  expect_true(all(subscales %in% names(result)))
})

test_that("VFQ_scoring subscale scores are numeric and within 0-100", {
  result <- VFQ_scoring(make_vfq_data())
  subscales <- c(
    "VFQ_general_health", "VFQ_general_vision", "VFQ_ocular_pain",
    "VFQ_near_activities", "VFQ_distance_activities", "VFQ_social_functioning",
    "VFQ_mental_health", "VFQ_role_difficulties", "VFQ_dependency",
    "VFQ_color_vision", "VFQ_peripheral_vision", "VFQ_total"
  )
  for (col in subscales) {
    val <- result[[col]]
    if (!is.na(val)) {
      expect_true(val >= 0 && val <= 100,
                  info = paste("Column", col, "has value", val, "outside 0-100"))
    }
  }
})

test_that("VFQ_scoring processes multiple rows", {
  df <- rbind(make_vfq_data(), make_vfq_data())
  result <- VFQ_scoring(df)
  expect_equal(nrow(result), 2)
})
