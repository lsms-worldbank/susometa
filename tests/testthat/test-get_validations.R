# ==============================================================================
# inputs
# ==============================================================================

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_validations(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_validations(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("get_validations() returns data frame with expected columns", {

  validations_df <- get_validations(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = validations_df,
    class = "data.frame"
  )

  # names of the data frame
  # expected
  validations_expected_col_names <- c(
    "public_key",
    "object_type",
    "type",
    "varname",
    "text",
    "expression_number",
    "validation_expression",
    "validation_message",
    "severity"
  )
  # actual
  validations_col_names <- names(validations_df)

  # has expected attributes
  testthat::expect_true(
    all(validations_expected_col_names %in% validations_col_names)
  )

})
