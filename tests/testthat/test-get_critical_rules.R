# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_critical_rules(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_critical_rules(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ------------------------------------------------------------------------------
# no target present in questionnaire
# ------------------------------------------------------------------------------

testthat::test_that("errors when no critical rules present in questionnaire", {

  testthat::expect_error(
    critical_rules_df <- get_critical_rules(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      )
    ),
    regexp = "No critical rules found"
  )

  testthat::expect_no_error(
    critical_rules_df <- get_critical_rules(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "critical_rules_qnr.json"
      )
    )
  )

})


# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("returns data frame with expected columns", {

  critical_rules_df <- get_critical_rules(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "critical_rules_qnr.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = critical_rules_df,
    class = "data.frame"
  )

  critical_rules_col_names_expected <- c(
    "type",
    "object_type",
    "rule_id",
    "rule_message",
    "rule_expression",
    "rule_description"
  )

  # names of the data frame
  critical_rules_col_names_found <- names(critical_rules_df)

  # has core variable attributes
  testthat::expect_true(
    all(critical_rules_col_names_expected %in% critical_rules_col_names_found)
  )

})



