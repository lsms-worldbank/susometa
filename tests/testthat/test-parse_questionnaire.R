# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    parse_questionnaire(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    parse_questionnaire(
      json_path = testthat::test_path(
        "fixtures", "qnr", "Categories",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("returns data frame with expected columns", {

  qnr_df <- parse_questionnaire(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a list
  testthat::expect_type(
    object = qnr_df,
    type = "list"
  )

  # that is named
  testthat::expect_named(
    object = qnr_df
  )

  qnr_col_names_expected <- c(
    "type",
    'object_type',
    "public_key",
    "varname",
    "answer_text_1",
    "answer_value_1"
  )

  # names of the data frame
  qnr_col_names_found <- names(qnr_df)

  # has core variable attributes
  testthat::expect_true(
    all(qnr_col_names_expected %in% qnr_col_names_found)
  )

})




